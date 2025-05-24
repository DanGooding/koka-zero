open Core

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

let pointer = Codegen.use_context Llvm.pointer_type

let ctl_yield =
  let open Codegen.Let_syntax in
  let%bind variant_tag = variant_tag in
  let%bind pointer = pointer in
  let marker = pointer in
  let op_clause = pointer in
  let resumption = pointer in
  let fields = [ variant_tag; marker; op_clause; resumption ] in
  Codegen.use_context (fun context ->
      Llvm.struct_type context (Array.of_list fields))
;;

let ctl_pure =
  let open Codegen.Let_syntax in
  let%bind variant_tag = variant_tag in
  let%bind pointer = pointer in
  let value = pointer in
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

let op =
  let open Codegen.Let_syntax in
  let%bind variant_tag = variant_tag in
  let%bind pointer = pointer in
  let clause = pointer in
  let fields = [ variant_tag; clause ] in
  Codegen.use_context (fun context ->
      Llvm.struct_type context (Array.of_list fields))
;;

let closure =
  let open Codegen.Let_syntax in
  let%bind num_vars = Codegen.use_context Llvm.i64_type in
  let%bind pointer = pointer in
  let variable_array = (* pointer to array of pointers *) pointer in
  let parent_closure = pointer in
  let fields = [ num_vars; variable_array; parent_closure ] in
  Codegen.use_context (fun context ->
      Llvm.struct_type context (Array.of_list fields))
;;

let function_object =
  let open Codegen.Let_syntax in
  let%bind pointer = pointer in
  let code_address = pointer in
  let closure_ptr = pointer in
  let%bind is_recursive = Codegen.use_context Llvm.i1_type in
  let fields = [ code_address; closure_ptr; is_recursive ] in
  Codegen.use_context (fun context ->
      Llvm.struct_type context (Array.of_list fields))
;;

let function_code num_args =
  let open Codegen.Let_syntax in
  let%map pointer = pointer in
  let function_object_ptr = pointer in
  let closure_ptr = pointer in
  let argument_types =
    function_object_ptr
    :: closure_ptr
    :: List.init num_args ~f:(fun _i -> pointer)
  in
  Llvm.function_type pointer (Array.of_list argument_types)
;;

let main_function =
  let open Codegen.Let_syntax in
  let%map i32 = Codegen.use_context Llvm.i32_type in
  Llvm.function_type i32 (Array.of_list [])
;;
