open! Core

module Callable : sig
  (** a [Callable] is a representation of a function which we are directly able to call
  *)
  type t =
    | Code_pointer of Llvm.llvalue (* pointer to the code *)
    | Function_object_pointer of Llvm.llvalue (* pointer to a [Types.closure] *)
end

(** an unambiguous representation of a function, suitable for passing around polymorphically
*)
type t =
  | Maybe_tagged of Llvm.llvalue
  (* code pointers are unboxed, but are tagged to distinguish from function object pointers.
     TODO: the optimiser is probably terrible with these tagged function pointers. We should really
     be able to tell if a pointer is to code or the heap.
  *)
  | Code_pointer of Llvm.llvalue
(* [Code_pointer] stores an untagged code pointer *)

val compile_use_callable
  :  t
  -> compile_use_code_pointer:(Llvm.llvalue -> Ctl_repr.t Codegen.t)
  -> compile_use_function_object_pointer:(Llvm.llvalue -> Ctl_repr.t Codegen.t)
  -> Ctl_repr.t Codegen.t

val compile_wrap_callable : Callable.t -> Llvm.llvalue Codegen.t
val compile_wrap : t -> Llvm.llvalue Codegen.t
