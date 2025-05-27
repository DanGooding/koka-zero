open Core
open Import

module Locals = struct
  type t = (Variable.t * Llvm.llvalue) list

  let find t v =
    List.find_map t ~f:(fun (v', value) ->
      Option.some_if Variable.(v = v') value)
  ;;

  let add t ~name ~value = (name, value) :: t

  let inter_names t names =
    List.filter t ~f:(fun (name, _) -> Set.mem names name)
  ;;

  (* TODO: filter shadowed names out? *)
end

module Closure = struct
  module Shape = struct
    type t = Variable.t list list [@@deriving sexp]

    let empty = []
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

  let find t v =
    Map.find t v
  end

type t =
  { locals : Locals.t
  ; closure : Closure.t
  ; toplevel : Toplevel.t
  }

let create_toplevel toplevel =
  let open Codegen.Let_syntax in
  let locals = [] in
  let%map closure = Closure.empty in
  { locals; closure; toplevel }
;;

let compile_capture { locals; closure; toplevel = _ } ~free ~runtime =
  match Locals.inter_names locals free with
  | [] ->
    (* we don't need to capture anything *)
    Codegen.return closure
  | escaping_locals ->
    (* these locals need to be captured *)
    Closure.compile_extend closure escaping_locals ~runtime
;;

let compile_get { locals; closure; toplevel } v =
  let open Codegen.Let_syntax in
  match Locals.find locals v with
  | Some value -> return value
  | None ->
    (match Closure.mem closure v with
     | true -> Closure.compile_get closure v
     | false ->
       (match Toplevel.find toplevel v with
        | Some callable ->
          Function_repr.compile_wrap_callable callable
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
