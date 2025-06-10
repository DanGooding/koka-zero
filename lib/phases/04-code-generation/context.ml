open Core
open Import

module Locals = struct
  type t = (Variable.t * Ctl_repr.t) list

  let find t v =
    List.find_map t ~f:(fun (v', value) ->
      Option.some_if Variable.(v = v') value)
  ;;

  let add t ~name ~value = (name, value) :: t

  let intersect_names t names =
    List.filter t ~f:(fun (name, _) -> Set.mem names name)
  ;;

  (* TODO: filter shadowed names out? *)
end

module Return_value_pointer = struct
  type t =
    | Pure
    | Ctl of { is_yield_i1_pointer : Llvm.llvalue }

  let type_ (t : t) : Evidence_passing_syntax.Type.t =
    match t with
    | Pure -> Pure
    | Ctl _ -> Ctl
  ;;

  let compile_return t (return_value : Ctl_repr.t) =
    let open Codegen.Let_syntax in
    match t, return_value with
    | Pure, Pure return_value ->
      let%map _return = Codegen.use_builder (Llvm.build_ret return_value) in
      ()
    | Ctl { is_yield_i1_pointer }, Ctl maybe_yield ->
      let%bind is_yield_i1 =
        Ctl_repr.Maybe_yield_repr.get_is_yield_i1 maybe_yield
      in
      let%bind _store =
        Codegen.use_builder (Llvm.build_store is_yield_i1 is_yield_i1_pointer)
      in
      let%bind content = Ctl_repr.Maybe_yield_repr.get_content maybe_yield in
      let%map _return = Codegen.use_builder (Llvm.build_ret content) in
      ()
    | Pure, Ctl _ | Ctl _, Pure _ ->
      let expected = type_ t in
      let actual = Ctl_repr.type_ return_value in
      Codegen.impossible_error
        (Sexp.to_string
           [%message
             "return value's type doesn't match function's return type"
               (expected : Evidence_passing_syntax.Type.t)
               (actual : Evidence_passing_syntax.Type.t)])
  ;;
end

module Closure = struct
  module Shape = struct
    type t = Variable.t list list [@@deriving sexp_of]

    let empty = []

    let is_empty = function
      | [] -> true
      | _ -> false
    ;;

    let names t = List.concat t |> Variable.Set.of_list
  end

  type t =
    { closure : Llvm.llvalue
    ; shape : Shape.t
    }

  let empty =
    let open Codegen.Let_syntax in
    let%map pointer_type = Types.pointer in
    let closure = Llvm.const_pointer_null pointer_type in
    let shape = Shape.empty in
    { closure; shape }
  ;;

  let is_empty { shape; _ } = Shape.is_empty shape
  let names { shape; _ } = Shape.names shape

  (** [compile_extend_closure closure names_and_values ~runtime] creates a new
      closure with the specified values (which should all be
      [Types.opaque_pointer]), and the given parent *)
  let compile_extend_closure
    :  Llvm.llvalue
    -> (Variable.t * Llvm.llvalue) list
    -> runtime:Runtime.t
    -> Llvm.llvalue Codegen.t
    =
    fun closure named_vars ~runtime ->
    let open Codegen.Let_syntax in
    let%bind closure_type = Types.closure in
    let%bind closure_ptr =
      Helpers.heap_allocate closure_type "extended_closure" ~runtime
    in
    let%bind pointer_type = Types.pointer in
    let num_vars = List.length named_vars in
    let vars_type = Llvm.array_type pointer_type num_vars in
    let%bind vars_ptr = Helpers.heap_allocate vars_type "vars" ~runtime in
    let var_values_and_register_names =
      List.map named_vars ~f:(fun (name, value) ->
        value, Helpers.register_name_of_variable name)
    in
    let array_type = Llvm.array_type pointer_type num_vars in
    let%bind () =
      Helpers.compile_populate_array
        vars_ptr
        var_values_and_register_names
        ~array_type
    in
    let%bind i64 = Codegen.use_context Llvm.i64_type in
    let num_vars_value = Llvm.const_int i64 num_vars in
    let%bind closure_type = Types.closure in
    let%map () =
      (* TODO: a first-class concept of a struct with populate+accessors *)
      Helpers.compile_populate_struct
        ~struct_type:closure_type
        closure_ptr
        [ num_vars_value, "num_vars"; vars_ptr, "vars"; closure, "parent" ]
    in
    closure_ptr
  ;;

  let compile_extend { closure = parent; shape } locals ~runtime =
    let open Codegen.Let_syntax in
    let var_names = List.map locals ~f:(fun (name, _value) -> name) in
    let shape = var_names :: shape in
    let%map closure_ptr = compile_extend_closure parent locals ~runtime in
    { closure = closure_ptr; shape }
  ;;

  (** compile accessing [ closure->vars[i] ] *)
  let compile_get_var (closure : Llvm.llvalue) ~(i : int) name =
    let open Codegen.Let_syntax in
    let%bind closure_type = Types.closure in
    let%bind vars =
      Helpers.compile_access_field closure ~struct_type:closure_type ~i:1 "vars"
    in
    let%bind i64 = Codegen.use_context Llvm.i64_type in
    let index = Llvm.const_int i64 i in
    let%bind pointer_type = Types.pointer in
    let%bind var_ptr =
      Codegen.use_builder
        (Llvm.build_gep
           pointer_type
           vars
           (Array.of_list [ index ])
           (name ^ "_ptr"))
    in
    (* treat all values as pointer types, even potential immediates *)
    Codegen.use_builder (Llvm.build_load pointer_type var_ptr name)
  ;;

  let index_of_variable vs v =
    match List.findi vs ~f:(fun _i v' -> Variable.(v = v')) with
    | Some (i, _v) -> Some i
    | None -> None
  ;;

  let mem { shape; _ } v =
    List.exists shape ~f:(fun vars ->
      List.mem vars v ~equal:[%equal: Variable.t])
  ;;

  let rec compile_get { closure; shape } v =
    let open Codegen.Let_syntax in
    match shape with
    | [] ->
      let message =
        sprintf "variable not found in closure: %s" (Variable.to_string_user v)
      in
      Codegen.impossible_error message
    | vs :: parent_shape ->
      (match index_of_variable vs v with
       | Some i ->
         compile_get_var closure ~i (Helpers.register_name_of_variable v)
       | None ->
         (* compile accessing closure->parent *)
         let%bind closure_type = Types.closure in
         let%bind parent_ptr =
           Helpers.compile_access_field
             closure
             ~struct_type:closure_type
             ~i:2
             "parent"
         in
         (* recurse on that *)
         let parent_closure = { closure = parent_ptr; shape = parent_shape } in
         compile_get parent_closure v)
  ;;
end

module Toplevel = struct
  type t = Function_repr.Callable.t Variable.Map.t

  let of_ordered_alist alist : t =
    Variable.Map.of_alist_multi alist |> Map.map ~f:List.last_exn
  ;;

  let find t v = Map.find t v
end

type t =
  { locals : Locals.t
  ; return_value_pointer : Return_value_pointer.t
  ; closure : Closure.t
  ; toplevel : Toplevel.t
  }

let create_toplevel toplevel =
  let open Codegen.Let_syntax in
  let locals = [] in
  let return_value_pointer =
    (* slightly inaccurate, can't actually 'return' in a toplevel expr *)
    Return_value_pointer.Pure
  in
  let%map closure = Closure.empty in
  { locals; return_value_pointer; closure; toplevel }
;;

let compile_capture
      { locals; closure; toplevel = _; return_value_pointer = _ }
      ~free
      ~runtime
  =
  let open Codegen.Let_syntax in
  match Locals.intersect_names locals free with
  | [] ->
    (* we don't need to capture anything *)
    let used_names_from_closure = Set.inter (Closure.names closure) free in
    (match Set.is_empty used_names_from_closure with
     | true ->
       (* don't need the closure at all *)
       Closure.empty
     | false -> (* existing parent closure has required names *) return closure)
  | escaping_locals ->
    let escaping_pure, escaping_ctl =
      List.partition_map escaping_locals ~f:(fun (name, repr) ->
        match (repr : Ctl_repr.t) with
        | Pure value -> First (name, value)
        | Ctl value -> Second (name, value))
    in
    (match escaping_ctl with
     | [] ->
       (* these locals need to be captured *)
       Closure.compile_extend closure escaping_pure ~runtime
     | _ :: _ ->
       let names = List.map escaping_ctl ~f:Tuple2.get1 in
       raise_s
         [%message
           "unable to capture values of Ctl repr in closure"
             (names : Variable.t list)])
;;

let compile_get { locals; closure; toplevel; return_value_pointer = _ } v =
  let open Codegen.Let_syntax in
  match Locals.find locals v with
  | Some value -> return value
  | None ->
    (match Closure.mem closure v with
     | true ->
       let%map value = Closure.compile_get closure v in
       Ctl_repr.Pure value
     | false ->
       (match Toplevel.find toplevel v with
        | Some callable ->
          let%map value = Function_repr.compile_wrap_callable callable in
          Ctl_repr.Pure value
        | None ->
          let message =
            sprintf
              "variable not found in scope: %s"
              (Variable.to_string_user v)
          in
          Codegen.impossible_error message))
;;

let add_local_exn t ~name ~value =
  let locals = Locals.add t.locals ~name ~value in
  { t with locals }
;;
