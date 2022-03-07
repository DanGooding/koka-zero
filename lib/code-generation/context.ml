open Core
open Import

module Locals = struct
  type t = (Variable.t * Llvm.llvalue) list

  let find t v =
    List.find_map t ~f:(fun (v', x) -> Option.some_if Variable.(v = v') x)
  ;;

  let add t ~name ~value = (name, value) :: t

  (* TODO: filter shadowed names out? *)
end

module Closure = struct
  module Shape = struct
    module T = struct
      type t =
        | Level of Variable.t list * t
        | Toplevel of Variable.t list
      [@@deriving sexp]
    end (* disable "fragile-match" for generated code *) [@warning "-4"]

    include T
  end

  type t =
    { closure : Llvm.llvalue (** of type [closure *] *)
    ; shape : Shape.t
    }

  (** [compile_extend_closure closure names_and_values ~runtime] creates a new
      closure with the specified values (which should all be
      [Types.opaque_pointer]), and the given parent *)
  let compile_extend_closure
      :  Llvm.llvalue -> (Variable.t * Llvm.llvalue) list -> runtime:Runtime.t
      -> Llvm.llvalue Codegen.t
    =
   fun closure named_vars ~runtime ->
    let open Codegen.Let_syntax in
    let%bind closure_type = Types.closure in
    let%bind closure_ptr =
      Helpers.heap_allocate closure_type "extended_closure" ~runtime
    in
    let%bind opaque_pointer = Types.opaque_pointer in
    let%bind parent_opaque_pointer =
      Codegen.use_builder
        (Llvm.build_bitcast closure opaque_pointer "parent_ptr")
    in
    let num_vars = List.length named_vars in
    let vars_type = Llvm.array_type opaque_pointer num_vars in
    let%bind vars_ptr = Helpers.heap_allocate vars_type "vars" ~runtime in
    let var_values_and_register_names =
      List.map named_vars ~f:(fun (name, value) ->
          value, Helpers.register_name_of_variable name)
    in
    let%bind () =
      (* vars_ptr has type [opaque_pointer x num_vars]*, we can fill in the
         elements in the same way as a struct *)
      Helpers.compile_populate_array vars_ptr var_values_and_register_names
    in
    let%bind i64 = Codegen.use_context Llvm.i64_type in
    let num_vars_value = Llvm.const_int i64 num_vars in
    let opaque_pointer_ptr = Llvm.pointer_type opaque_pointer in
    let%bind vars_ptr =
      Codegen.use_builder
        (Llvm.build_bitcast vars_ptr opaque_pointer_ptr "vars")
    in
    let%map () =
      Helpers.compile_populate_struct
        closure_ptr
        [ num_vars_value, "num_vars"
        ; vars_ptr, "vars"
        ; parent_opaque_pointer, "parent"
        ]
    in
    closure_ptr
 ;;

  let compile_extend { closure = parent; shape } locals ~runtime =
    let open Codegen.Let_syntax in
    let var_names = List.map locals ~f:(fun (name, _value) -> name) in
    let shape = Shape.Level (var_names, shape) in
    let%map closure_ptr = compile_extend_closure parent locals ~runtime in
    { closure = closure_ptr; shape }
  ;;

  let compile_make_toplevel names_and_code ~runtime =
    let open Codegen.Let_syntax in
    let%bind opaque_pointer = Types.opaque_pointer in
    let parent_closure = Llvm.const_pointer_null opaque_pointer in
    let%map closure =
      compile_extend_closure parent_closure names_and_code ~runtime
    in
    let names = List.map names_and_code ~f:(fun (name, _code) -> name) in
    let shape = Shape.Toplevel names in
    { closure; shape }
  ;;

  (** compile accessing [ closure->vars\[i\] ] *)
  let compile_get_var (i : int) (closure : Llvm.llvalue) name =
    let open Codegen.Let_syntax in
    let%bind vars_ptr =
      Codegen.use_builder (Llvm.build_struct_gep closure 1 "vars_field")
    in
    let%bind vars = Codegen.use_builder (Llvm.build_load vars_ptr "vars") in
    let%bind i64 = Codegen.use_context Llvm.i64_type in
    let index = Llvm.const_int i64 i in
    let%bind var_ptr =
      Codegen.use_builder
        (Llvm.build_gep vars (Array.of_list [ index ]) (name ^ "_ptr"))
    in
    Codegen.use_builder (Llvm.build_load var_ptr name)
  ;;

  let index_of_variable vs v =
    match List.findi vs ~f:(fun _i v' -> Variable.(v = v')) with
    | Some (i, _v) -> Some i
    | None -> None
  ;;

  let rec compile_get { closure; shape } v =
    let open Codegen.Let_syntax in
    match shape with
    | Shape.Toplevel vs ->
      (match index_of_variable vs v with
      | Some i ->
        compile_get_var i closure (Helpers.register_name_of_variable v)
      | None ->
        let message =
          sprintf
            "variable not found in closure: %s"
            (Variable.to_string_user v)
        in
        Codegen.impossible_error message)
    | Shape.Level (vs, parent_shape) ->
      (match index_of_variable vs v with
      | Some i ->
        compile_get_var i closure (Helpers.register_name_of_variable v)
      | None ->
        (* compile accessing closure->parent *)
        let%bind parent_field_ptr =
          Codegen.use_builder (Llvm.build_struct_gep closure 2 "parent_field")
        in
        let%bind parent_opaque_ptr =
          Codegen.use_builder (Llvm.build_load parent_field_ptr "parent_opaque")
        in
        let%bind closure_type = Types.closure in
        let closure_ptr_type = Llvm.pointer_type closure_type in
        let%bind parent =
          Codegen.use_builder
            (Llvm.build_bitcast parent_opaque_ptr closure_ptr_type "parent")
        in
        (* recurse on that *)
        let parent_closure = { closure = parent; shape = parent_shape } in
        compile_get parent_closure v)
  ;;
end

type t =
  | Local of
      { locals : Locals.t
      ; closure : Closure.t
      }
  | Toplevel of Closure.t

let compile_capture t ~runtime =
  match t with
  | Local { locals; closure } -> Closure.compile_extend closure locals ~runtime
  | Toplevel closure -> Codegen.return closure
;;

let compile_get t v =
  let open Codegen.Let_syntax in
  match t with
  | Local { locals; closure } ->
    (match Locals.find locals v with
    | Some value -> return value
    | None -> Closure.compile_get closure v)
  | Toplevel closure -> Closure.compile_get closure v
;;

let add_local_exn t ~name ~value =
  match t with
  | Toplevel _ -> raise_s [%message "attempt to add a local at toplevel"]
  | Local { locals; closure } ->
    let locals = Locals.add locals ~name ~value in
    Local { locals; closure }
;;
