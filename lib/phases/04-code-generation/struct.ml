open! Core
open! Import
include Struct_intf

module Make (Arg : Arg_S) = struct
  include Arg

  let index_exn t field =
    List.find_mapi_exn (Field.all t) ~f:(fun i f ->
      Option.some_if ([%equal: Field.t] field f) i)
  ;;

  let type_ t =
    let open Codegen.Let_syntax in
    let%bind field_types =
      List.map (Field.all t) ~f:Field.type_ |> Codegen.all
    in
    Codegen.use_context (fun context ->
      Llvm.struct_type context (Array.of_list field_types))
  ;;

  let project ?name t struct_value field =
    let open Codegen.Let_syntax in
    let%bind type_ = type_ t in
    let index = index_exn t field in
    let name = Option.value name ~default:(Field.name field) in
    let%bind field_ptr =
      Codegen.use_builder
        (Llvm.build_struct_gep type_ struct_value index (name ^ "_ptr"))
    in
    let%bind field_type = Field.type_ field in
    Codegen.use_builder (Llvm.build_load field_type field_ptr name)
  ;;

  let iteri_fields t ~f = List.mapi (Field.all t) ~f |> Codegen.all_unit

  let populate t struct_value ~f =
    let open Codegen.Let_syntax in
    let%bind type_ = type_ t in
    iteri_fields t ~f:(fun index field ->
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

  let heap_allocate t ~name ~runtime =
    let open Codegen.Let_syntax in
    let%bind type_ = type_ t in
    let size = Llvm.size_of type_ in
    let { Runtime.malloc; _ } = runtime in
    Runtime.Function.build_call malloc ~args:(Array.of_list [ size ]) name
  ;;
end
