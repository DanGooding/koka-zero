open! Core

module Callable = struct
  type t =
    | Code_pointer of Llvm.llvalue
    | Function_object_pointer of Llvm.llvalue
end

type t = 
  | Maybe_tagged of Llvm.llvalue
  | Code_pointer of Llvm.llvalue

(* this relies on the alignment of functions being greater than one byte *)
let tag () =
  let open Codegen.Let_syntax in
  let%map i64 = Codegen.use_context Llvm.i64_type in
  Llvm.const_int i64 1
;;

let compile_use_callable
      t
      ~compile_use_code_pointer
      ~compile_use_function_object_pointer
  =
  let open Codegen.Let_syntax in
  match t with
  | Code_pointer code_address -> compile_use_code_pointer code_address
  | Maybe_tagged t ->
  let%bind i64 = Codegen.use_context Llvm.i64_type in
  let%bind pointer_type = Types.pointer in
  let%bind tag = tag () in
  let%bind ptr_value =
    Codegen.use_builder (Llvm.build_ptrtoint t i64 "ptr_value")
  in
  let%bind tag_bit =
    Codegen.use_builder (Llvm.build_and ptr_value tag "tag_bit")
  in
  let%bind tag_is_set =
    Codegen.use_builder (Llvm.build_icmp Eq tag_bit tag "tag_is_set")
  in
  Helpers.compile_conditional
    tag_is_set
    ~compile_true:(fun () ->
      let%bind code_pointer =
        Codegen.use_builder (Llvm.build_xor ptr_value tag "code_pointer_value")
      in
      let%bind code_pointer =
        Codegen.use_builder
          (Llvm.build_inttoptr code_pointer pointer_type "code_pointer")
      in
      compile_use_code_pointer code_pointer)
    ~compile_false:(fun () -> compile_use_function_object_pointer t)
;;

let compile_wrap_callable (callable : Callable.t) =
  let open Codegen.Let_syntax in
  match callable with
  | Function_object_pointer ptr -> return ptr
  | Code_pointer code_addr ->
    let%bind i64 = Codegen.use_context Llvm.i64_type in
    let%bind pointer_type = Types.pointer in
    let%bind tag = tag () in
    let%bind ptr_value =
      Codegen.use_builder (Llvm.build_ptrtoint code_addr i64 "ptr_value")
    in
    let%bind tagged_value =
      Codegen.use_builder (Llvm.build_or ptr_value tag "tagged_value")
    in
    Codegen.use_builder (Llvm.build_inttoptr tagged_value pointer_type "tagged")
;;
