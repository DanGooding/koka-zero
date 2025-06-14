open Core
open Import

let register_name_of_variable = function
  | Variable.User s -> "u_" ^ s
  | Variable.Language s -> "l_" ^ s
  (* generated names tend to already have numerical suffixes - use underscore to
     separate *)
  | Variable.Generated s -> "g_" ^ s ^ "_"
;;

let const_tag i =
  let open Codegen.Let_syntax in
  let%map tag_type = Types.variant_tag in
  Llvm.const_int tag_type i
;;

let const_op_normal_tag = const_tag 0
let const_op_tail_tag = const_tag 1

let const_label i =
  let open Codegen.Let_syntax in
  let%map label_type = Types.label in
  Llvm.const_int label_type i
;;

let heap_allocate type_ name ~runtime =
  let size = Llvm.size_of type_ in
  let { Runtime.malloc; _ } = runtime in
  Runtime.Function.build_call malloc ~args:(Array.of_list [ size ]) name
;;

let heap_store type_ v name ~runtime =
  let open Codegen.Let_syntax in
  let%bind ptr = heap_allocate type_ name ~runtime in
  let%map _store = Codegen.use_builder (Llvm.build_store v ptr) in
  ptr
;;

let heap_store_aux
  :  Llvm.lltype Codegen.t
  -> string
  -> Llvm.llvalue
  -> runtime:Runtime.t
  -> Llvm.llvalue Codegen.t
  =
  fun t name v ~runtime ->
  let open Codegen.Let_syntax in
  let%bind t = t in
  heap_store t v name ~runtime
;;

let heap_store_marker = heap_store_aux Types.marker "marker"
let heap_store_label = heap_store_aux Types.label "label"

let dereference ptr type_ name =
  Codegen.use_builder (Llvm.build_load type_ ptr name)
;;

let dereference_aux
  : Llvm.lltype Codegen.t -> string -> Llvm.llvalue -> Llvm.llvalue Codegen.t
  =
  fun t name ptr ->
  let open Codegen.Let_syntax in
  let%bind t = t in
  dereference ptr t name
;;

let dereference_marker = dereference_aux Types.marker "marker"
let dereference_label = dereference_aux Types.label "label"

let compile_populate_struct
  :  struct_type:Llvm.lltype
  -> Llvm.llvalue
  -> (Llvm.llvalue * string) list
  -> unit Codegen.t
  =
  fun ~struct_type struct_ptr members ->
  let open Codegen.Let_syntax in
  List.mapi members ~f:(fun i (x, name) ->
    let%bind member_ptr =
      Codegen.use_builder
        (Llvm.build_struct_gep struct_type struct_ptr i (name ^ "_field_ptr"))
    in
    let%map _store = Codegen.use_builder (Llvm.build_store x member_ptr) in
    ())
  |> Codegen.all_unit
;;

let compile_populate_array ~array_type array_ptr elements =
  let open Codegen.Let_syntax in
  let%bind i64 = Codegen.use_context Llvm.i64_type in
  List.mapi elements ~f:(fun i (x, name) ->
    (* first zero is because we require an array pointer, rather than an
       element pointer *)
    let indices = List.map [ 0; i ] ~f:(Llvm.const_int i64) |> Array.of_list in
    let%bind element_ptr =
      Codegen.use_builder (Llvm.build_gep array_type array_ptr indices name)
    in
    let%map _store = Codegen.use_builder (Llvm.build_store x element_ptr) in
    ())
  |> Codegen.all_unit
;;

let compile_access_field struct_ptr ~struct_type ~i name =
  let open Codegen.Let_syntax in
  let%bind field_ptr =
    Codegen.use_builder
      (Llvm.build_struct_gep struct_type struct_ptr i (name ^ "_field_ptr"))
  in
  let field_type = (Llvm.struct_element_types struct_type).(i) in
  Codegen.use_builder (Llvm.build_load field_type field_ptr name)
;;
