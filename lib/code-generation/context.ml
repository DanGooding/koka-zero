open Core
open Import

module Parameters = struct
  type t = (Variable.t * Llvm.llvalue) list
  (* TODO: does this contain the extraneous `self` parameter for non recursive
     functions? *)

  let find t v =
    List.find_map t ~f:(fun (v', x) -> Option.some_if Variable.(v = v') x)
  ;;
end

module Closure = struct
  module Shape = struct
    module T = struct
      type t =
        | Level of Variable.t list * t
        | Empty
      [@@deriving sexp]
    end (* disable "fragile-match" for generated code *) [@warning "-4"]

    include T
  end

  type t =
    { closure : Llvm.llvalue (** of type [closure *] *)
    ; shape : Shape.t
    }

  let extend { closure = parent; shape } parameters ~runtime =
    let open Codegen.Let_syntax in
    let%bind closure_type = Types.closure in
    let%bind closure_ptr =
      Helpers.heap_allocate closure_type "extended_closure" ~runtime
    in
    let%bind opaque_pointer = Types.opaque_pointer in
    let%bind parent_opaque_pointer =
      Codegen.use_builder
        (Llvm.build_bitcast parent opaque_pointer "parent_ptr")
    in
    let num_vars = List.length parameters in
    let vars_type = Llvm.array_type opaque_pointer num_vars in
    let%bind vars_ptr = Helpers.heap_allocate vars_type "vars" ~runtime in
    let var_names = List.map parameters ~f:(fun (name, _value) -> name) in
    let var_values_and_register_names =
      List.map parameters ~f:(fun (name, value) ->
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
    let shape = Shape.Level (var_names, shape) in
    { closure = closure_ptr; shape }
  ;;

  let rec compile_get { closure; shape } v =
    let open Codegen.Let_syntax in
    match shape with
    | Shape.Empty ->
      let message =
        sprintf "variable not found in closure: %s" (Variable.to_string_user v)
      in
      Codegen.impossible_error message
    | Shape.Level (vs, parent_shape) ->
      (match List.findi vs ~f:(fun _i v' -> Variable.(v = v')) with
      | Some (i, _v) ->
        (* compile accessing closure->vars[i] *)
        let%bind vars_ptr =
          Codegen.use_builder (Llvm.build_struct_gep closure 1 "vars_field")
        in
        let%bind vars = Codegen.use_builder (Llvm.build_load vars_ptr "vars") in
        let%bind i64 = Codegen.use_context Llvm.i64_type in
        let index = Llvm.const_int i64 i in
        let%bind var_ptr =
          Codegen.use_builder
            (Llvm.build_gep vars (Array.of_list [ index ]) "var_ptr")
        in
        Codegen.use_builder (Llvm.build_load var_ptr "var")
      | None ->
        (* compile accessing closure->parent *)
        let%bind parent_ptr =
          Codegen.use_builder (Llvm.build_struct_gep closure 2 "parent_field")
        in
        let%bind parent =
          Codegen.use_builder (Llvm.build_load parent_ptr "parent")
        in
        (* recurse on that *)
        let parent_closure = { closure = parent; shape = parent_shape } in
        compile_get parent_closure v)
  ;;
end

type t =
  { parameters : Parameters.t
  ; closure : Closure.t
  }

let compile_get { parameters; closure } v =
  let open Codegen.Let_syntax in
  match Parameters.find parameters v with
  | Some value -> return value
  | None -> Closure.compile_get closure v
;;
