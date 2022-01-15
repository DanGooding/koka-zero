open Core

let opaque_pointer =
  let open Codegen.Let_syntax in
  (* byte pointer *)
  let%map i8 = Codegen.use_context Llvm.i8_type in
  Llvm.pointer_type i8
;;

let bool =
  (* could use i1 (single bit) but can't allocate that little, nor can an i8*
     point to it *)
  Codegen.use_context Llvm.i8_type
;;

let int = Codegen.use_context Llvm.i64_type
let unit = Codegen.use_context Llvm.i8_type
let variant_tag = Codegen.use_context Llvm.i8_type
let padding = Codegen.use_context Llvm.i8_type

let ctl_yield =
  let open Codegen.Let_syntax in
  let%bind variant_tag = variant_tag in
  let%bind opaque_pointer = opaque_pointer in
  let marker = opaque_pointer in
  let op_clause = opaque_pointer in
  let resumption = opaque_pointer in
  let fields = [ variant_tag; marker; op_clause; resumption ] in
  Codegen.use_context (fun context ->
      Llvm.struct_type context (Array.of_list fields))
;;

let ctl_pure =
  let open Codegen.Let_syntax in
  let%bind variant_tag = variant_tag in
  let%bind opaque_pointer = opaque_pointer in
  let%bind padding = padding in
  let value = opaque_pointer in
  let padding_array = Llvm.array_type padding (8 * 2) in
  let fields = [ variant_tag; value; padding_array ] in
  Codegen.use_context (fun context ->
      Llvm.struct_type context (Array.of_list fields))
;;

(* TODO: somehow ensure these are all the same size! bitcasts _should_ be
   rejected by assert_valid_function if they are different sizes *)
let ctl =
  let open Codegen.Let_syntax in
  let%bind variant_tag = variant_tag in
  let%bind padding = padding in
  let padding_array = Llvm.array_type padding (8 * 3) in
  let fields = [ variant_tag; padding_array ] in
  Codegen.use_context (fun context ->
      Llvm.struct_type context (Array.of_list fields))
;;

let marker = Codegen.use_context Llvm.i64_type
let label = Codegen.use_context Llvm.i64_type
