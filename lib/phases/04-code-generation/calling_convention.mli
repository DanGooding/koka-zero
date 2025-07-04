open! Core
open! Import
module Type = Evidence_passing_syntax.Type

(** Compile a call to a function, with known arguments and return type. *)
val compile_call
  :  code_pointer:Llvm.llvalue
  -> function_repr:Value_repr.Unpacked.Function.t
  -> args:Ctl_repr.t list
  -> return_type:Type.t
  -> Ctl_repr.t Codegen.t

(** Compile a tail-call to a function, returning the return llvalue. *)
val compile_tail_call
  :  code_pointer:Llvm.llvalue
  -> function_repr:Value_repr.Unpacked.Function.t
  -> args:Ctl_repr.t list
  -> return_type:Type.t
  -> return_value_pointer:Context.Return_value_pointer.t
  -> Llvm.llvalue Codegen.t

(** Constructs a llvm function with the expected type and parameters,
    and builds a [Context.t] that is available in it's body.
    Leaves the body of the function uninitialised. *)
val make_function_and_context
  :  params:(Parameter.t * Type.t) list
  -> symbol_name:Symbol_name.t
  -> self:Variable.t option
  -> return_type:Type.t
  -> captured_shape:Context.Closure.Shape.t option
  -> toplevel:Context.Toplevel.t
  -> (Llvm.llvalue * Context.t) Codegen.t
