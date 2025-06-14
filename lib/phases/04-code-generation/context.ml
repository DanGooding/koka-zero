open Core
open Import

module Locals = struct
  type t = (Variable.t * Ctl_repr.t) list

  let find t v =
    List.find_map t ~f:(fun (v', value) ->
      Option.some_if Variable.(v = v') value)
  ;;

  let mem t v = find t v |> Option.is_some
  let add t ~name ~value = (name, value) :: t

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
  (* it's important that the shape has the same order as the values are stored
     into the closure itself. *)
  let variable_ordering = [%compare: Variable.t]

  module Shape : sig
    type t [@@deriving sexp_of]

    (** raises if empty, or if not in order *)
    val create_exn : Variable.t list -> t

    val mem : t -> Variable.t -> bool
    val length : t -> int
    val index_of_variable : t -> Variable.t -> int option
    val to_list : t -> Variable.t list
  end = struct
    type t = Variable.t list [@@deriving sexp_of]

    let create_exn vs =
      match vs with
      | [] -> raise_s [%message "closure must be non-empty"]
      | vs ->
        (match List.is_sorted vs ~compare:variable_ordering with
         | true -> vs
         | false ->
           raise_s
             [%message
               "closure variables must be in sorted order, so creator and body \
                agree"])
    ;;

    let mem = List.mem ~equal:[%equal: Variable.t]
    let length = List.length
    let to_list t = t

    let index_of_variable vs v =
      match List.findi vs ~f:(fun _i v' -> [%equal: Variable.t] v v') with
      | Some (i, _v) -> Some i
      | None -> None
    ;;
  end

  type t =
    { closure : Llvm.llvalue
    ; shape : Shape.t
    }

  let get_var_pointer
        (closure : Llvm.llvalue)
        (closure_type : Llvm.lltype)
        ~(i : int)
        ~name
    =
    let open Codegen.Let_syntax in
    let%bind i32 = Codegen.use_context Llvm.i32_type in
    Codegen.use_builder
      (Llvm.build_gep
         closure_type
         closure
         (Array.of_list (List.map [ 0; 1; i ] ~f:(Llvm.const_int i32)))
         (name ^ "_ptr"))
  ;;

  let compile_create
        (contents : (Variable.t * Llvm.llvalue) list)
        ~code_address
        ~runtime
    =
    let open Codegen.Let_syntax in
    let%bind closure_type =
      Types.closure_struct ~num_captured:(List.length contents)
    in
    let%bind closure_ptr =
      Helpers.heap_allocate closure_type "closure" ~runtime
    in
    let%bind code_address_ptr =
      Codegen.use_builder
        (Llvm.build_struct_gep closure_type closure_ptr 0 "code_address_ptr")
    in
    let%bind _store =
      Codegen.use_builder (Llvm.build_store code_address code_address_ptr)
    in
    (* ensure contents sorted *)
    let contents =
      List.sort
        contents
        ~compare:(Comparable.lift variable_ordering ~f:Tuple2.get1)
    in
    let%map () =
      List.mapi contents ~f:(fun i (name, value) ->
        let%bind var_pointer =
          get_var_pointer
            closure_ptr
            closure_type
            ~i
            ~name:(Helpers.register_name_of_variable name)
        in
        let%map _store =
          Codegen.use_builder (Llvm.build_store value var_pointer)
        in
        ())
      |> Codegen.all_unit
    in
    let shape = List.map contents ~f:(fun (name, _value) -> name) in
    let shape = Shape.create_exn shape in
    { shape; closure = closure_ptr }
  ;;

  (** compile accessing [ closure->vars[i] ] *)
  let compile_get_var (closure : Llvm.llvalue) ~(i : int) ~num_captured ~name =
    let open Codegen.Let_syntax in
    let%bind closure_type = Types.closure_struct ~num_captured in
    let%bind var_ptr = get_var_pointer closure closure_type ~i ~name in
    let%bind pointer_type = Types.pointer in
    (* treat all values as pointer types, even potential immediates *)
    Codegen.use_builder (Llvm.build_load pointer_type var_ptr name)
  ;;

  let mem { shape; _ } v = Shape.mem shape v

  let compile_get { closure; shape } v =
    match Shape.index_of_variable shape v with
    | Some i ->
      compile_get_var
        closure
        ~i
        ~num_captured:(Shape.length shape)
        ~name:(Helpers.register_name_of_variable v)
    | None ->
      let message =
        sprintf "variable not found in closure: %s" (Variable.to_string_user v)
      in
      Codegen.impossible_error message
  ;;

  let get_type _t _v = Evidence_passing_syntax.Type.Pure
end

module Toplevel = struct
  type t = Value_repr.Unpacked.Function.t Variable.Map.t

  let of_ordered_alist alist : t =
    Variable.Map.of_alist_multi alist |> Map.map ~f:List.last_exn
  ;;

  let find t v = Map.find t v
  let mem t v = Map.mem t v
  let type_ _t _v = Evidence_passing_syntax.Type.Pure
end

type t =
  { locals : Locals.t
  ; return_value_pointer : Return_value_pointer.t
  ; closure : Closure.t option
  ; toplevel : Toplevel.t
  }

let create_toplevel toplevel =
  let locals = [] in
  let return_value_pointer =
    (* slightly inaccurate, can't actually 'return' in a toplevel expr *)
    Return_value_pointer.Pure
  in
  let closure = None in
  { locals; return_value_pointer; closure; toplevel }
;;

let find { locals; closure; toplevel; return_value_pointer = _ } v =
  match Locals.mem locals v with
  | true -> `Local
  | false ->
    let in_closure =
      match closure with
      | None -> false
      | Some closure -> Closure.mem closure v
    in
    (match in_closure with
     | true -> `Closure
     | false ->
       (match Toplevel.mem toplevel v with
        | true -> `Toplevel
        | false -> `Not_found))
;;

let get_type t v =
  match find t v with
  | `Local -> Locals.find t.locals v |> Option.value_exn |> Ctl_repr.type_
  | `Closure -> Closure.get_type (Option.value_exn t.closure) v
  | `Toplevel -> Toplevel.type_ t.toplevel v
  | `Not_found ->
    raise_s [%message "variable not found in scope: %s" (v : Variable.t)]
;;

let compile_get t v =
  let open Codegen.Let_syntax in
  match find t v with
  | `Local -> return (Locals.find t.locals v |> Option.value_exn)
  | `Closure ->
    let%map value = Closure.compile_get (Option.value_exn t.closure) v in
    Ctl_repr.Pure value
  | `Toplevel ->
    let function_ = Toplevel.find t.toplevel v |> Option.value_exn in
    let%map value = Value_repr.Unpacked.Function.pack function_ in
    Ctl_repr.Pure value
  | `Not_found ->
    let message =
      sprintf "variable not found in scope: %s" (Variable.to_string_user v)
    in
    Codegen.impossible_error message
;;

let get_captured t ~free =
  (* new closure will contain [free - toplevel] *)
  let free_with_sources = Set.to_map free ~f:(fun v -> find t v) in
  let to_capture =
    Map.filteri free_with_sources ~f:(fun ~key:v ~data:source ->
      match source with
      | `Toplevel -> false
      | `Local | `Closure -> true
      | `Not_found ->
        raise_s [%message "captured variable is not in scope" (v : Variable.t)])
    |> Map.keys
  in
  match List.is_empty to_capture with
  | true -> None
  | false ->
    let captured_with_types =
      List.map to_capture ~f:(fun v -> v, get_type t v)
    in
    let captured_pure, captured_ctl =
      List.partition_map captured_with_types ~f:(fun (name, type_) ->
        match (type_ : Evidence_passing_syntax.Type.t) with
        | Pure -> First name
        | Ctl -> Second name)
    in
    (match captured_ctl with
     | [] -> ()
     | _ :: _ ->
       (* we could implement this someday, but there's no need right now *)
       raise_s
         [%message
           "unable to capture values of Ctl repr in closure"
             (captured_ctl : Variable.t list)]);
    let captured_pure =
      List.sort captured_pure ~compare:Closure.variable_ordering
    in
    Some (Closure.Shape.create_exn captured_pure)
;;

let compile_capture t ~captured_shape ~code_address ~runtime =
  let open Codegen.Let_syntax in
  let%bind captued =
    Closure.Shape.to_list captured_shape
    |> List.map ~f:(fun v ->
      let%map value = compile_get t v in
      v, Ctl_repr.pure_exn value)
    |> Codegen.all
  in
  let%map closure = Closure.compile_create captued ~code_address ~runtime in
  closure
;;

let add_local_exn t ~name ~value =
  let locals = Locals.add t.locals ~name ~value in
  { t with locals }
;;
