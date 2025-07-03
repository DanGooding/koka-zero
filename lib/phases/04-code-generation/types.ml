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
let pointer = Codegen.use_context Llvm.pointer_type

let main_function =
  let open Codegen.Let_syntax in
  let%map i32 = Codegen.use_context Llvm.i32_type in
  Llvm.function_type i32 (Array.of_list [])
;;
