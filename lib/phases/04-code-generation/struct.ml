open! Core
open! Import
include Struct_intf

module Make (Arg : Arg_S) = struct
  include Arg

  let index_exn field =
    List.find_mapi_exn Field.all ~f:(fun i f ->
      Option.some_if ([%equal: Field.t] field f) i)
  ;;

  let type_ =
    let open Codegen.Let_syntax in
    let%bind field_types = List.map Field.all ~f:Field.type_ |> Codegen.all in
    Codegen.use_context (fun context ->
      Llvm.struct_type context (Array.of_list field_types))
  ;;

  let project struct_value field =
    let open Codegen.Let_syntax in
    let%bind type_ = type_ in
    let index = index_exn field in
    let name = Field.name field in
    let%bind field_ptr =
      Codegen.use_builder
        (Llvm.build_struct_gep type_ struct_value index (name ^ "_ptr"))
    in
    let%bind field_type = Field.type_ field in
    Codegen.use_builder (Llvm.build_load field_type field_ptr name)
  ;;

  let iteri_fields ~f = List.mapi Field.all ~f |> Codegen.all_unit

  let populate struct_value ~f =
    let open Codegen.Let_syntax in
    let%bind type_ = type_ in
    iteri_fields ~f:(fun index field ->
      let name = Field.name field in
      let field_value = f field in
      let%bind field_ptr =
        Codegen.use_builder
          (Llvm.build_struct_gep type_ struct_value index (name ^ "_ptr"))
      in
      let%map _store =
        Codegen.use_builder (Llvm.build_store field_value field_ptr)
      in
      ())
  ;;
end
