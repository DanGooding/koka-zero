open Core
open! Import

let bool =
  (* could use i1 (single bit) but can't allocate that little, nor can an i8*
     point to it *)
  Codegen.use_context Llvm.i8_type
;;

let int = Codegen.use_context Llvm.i64_type
let marker = Codegen.use_context Llvm.i64_type
let label = Codegen.use_context Llvm.i64_type
let variant_tag = Codegen.use_context Llvm.i8_type
let pointer = Codegen.use_context Llvm.pointer_type

let op =
  let open Codegen.Let_syntax in
  let%bind variant_tag = variant_tag in
  let%bind pointer = pointer in
  let clause = pointer in
  let fields = [ variant_tag; clause ] in
  Codegen.use_context (fun context ->
    Llvm.struct_type context (Array.of_list fields))
;;

let closure_struct ~num_captured =
  let open Codegen.Let_syntax in
  let%bind pointer = pointer in
  let code_address = pointer in
  let captures = Llvm.array_type pointer num_captured in
  let fields = [ code_address; captures ] in
  Codegen.use_context (fun context ->
    Llvm.struct_type context (Array.of_list fields))
;;

let main_function =
  let open Codegen.Let_syntax in
  let%map i32 = Codegen.use_context Llvm.i32_type in
  Llvm.function_type i32 (Array.of_list [])
;;
