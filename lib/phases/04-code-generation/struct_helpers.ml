open! Core
open! Import

let const_tag i =
  let open Codegen.Let_syntax in
  let%map tag_type = Types.variant_tag in
  Llvm.const_int tag_type i
;;

let const_op_normal_tag = const_tag 0
let const_op_tail_tag = const_tag 1

let heap_allocate type_ name ~runtime =
  let size = Llvm.size_of type_ in
  let { Runtime.malloc; _ } = runtime in
  Runtime.Function.build_call malloc ~args:(Array.of_list [ size ]) name
;;

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

let compile_access_field struct_ptr ~struct_type ~i name =
  let open Codegen.Let_syntax in
  let%bind field_ptr =
    Codegen.use_builder
      (Llvm.build_struct_gep struct_type struct_ptr i (name ^ "_field_ptr"))
  in
  let field_type = (Llvm.struct_element_types struct_type).(i) in
  Codegen.use_builder (Llvm.build_load field_type field_ptr name)
;;
