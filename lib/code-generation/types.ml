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
let marker = Codegen.use_context Llvm.i64_type
let label = Codegen.use_context Llvm.i64_type
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
  let value = opaque_pointer in
  let fields = [ variant_tag; value ] in
  Codegen.use_context (fun context ->
      Llvm.struct_type context (Array.of_list fields))
;;

let ctl =
  let open Codegen.Let_syntax in
  let%bind variant_tag = variant_tag in
  let%bind padding = padding in
  let padding_array = Llvm.array_type padding (7 + (8 * 3)) in
  let fields = [ variant_tag; padding_array ] in
  Codegen.use_context (fun context ->
      Llvm.struct_type context (Array.of_list fields))
;;

let closure =
  let open Codegen.Let_syntax in
  let%bind num_vars = Codegen.use_context Llvm.i64_type in
  let%bind opaque_pointer = opaque_pointer in
  let variable_array = Llvm.pointer_type opaque_pointer in
  (* no recursive pointer sadly *)
  let parent_closure = opaque_pointer in
  let fields = [ num_vars; variable_array; parent_closure ] in
  Codegen.use_context (fun context ->
      Llvm.struct_type context (Array.of_list fields))
;;

let function_object =
  let open Codegen.Let_syntax in
  let%bind opaque_pointer = opaque_pointer in
  let code_address = opaque_pointer in
  let%bind closure = closure in
  let closure_ptr = Llvm.pointer_type closure in
  let%bind is_recursive = Codegen.use_context Llvm.i1_type in
  let fields = [ code_address; closure_ptr; is_recursive ] in
  Codegen.use_context (fun context ->
      Llvm.struct_type context (Array.of_list fields))
;;

let function_code num_args =
  let open Codegen.Let_syntax in
  let%bind opaque_pointer = opaque_pointer in
  let%bind function_object = function_object in
  let function_object_ptr = Llvm.pointer_type function_object in
  let%map closure = closure in
  let closure_ptr = Llvm.pointer_type closure in
  let argument_types =
    function_object_ptr
    :: closure_ptr
    :: List.init num_args ~f:(fun _i -> opaque_pointer)
  in
  Llvm.function_type opaque_pointer (Array.of_list argument_types)
;;

let main_function =
  let open Codegen.Let_syntax in
  let%map i32 = Codegen.use_context Llvm.i32_type in
  Llvm.function_type i32 (Array.of_list [])
;;
