open! Core
open! Import

val bool : Llvm.lltype Codegen.t
val int : Llvm.lltype Codegen.t
val pointer : Llvm.lltype Codegen.t

(** prompt marker *)
val marker : Llvm.lltype Codegen.t

(** effect label *)
val label : Llvm.lltype Codegen.t

(** A function object holds a code address, and the captured free-variables
    [{ ptr code; [ptr x num_captured] }] *)
val closure_struct : num_captured:int -> Llvm.lltype Codegen.t

(** type of the binary's entry point: [i32 main()] *)
val main_function : Llvm.lltype Codegen.t
