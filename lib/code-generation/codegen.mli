open Core
open Koka_zero_util
open Import

(** monad encapsulating the mutable state of llvm code generation. It holds
    references to the llcontext, llmodule and llbuilder *)
type 'a t

include Monad.S with type 'a t := 'a t
include Monad_utils.S with type 'a t := 'a t

(* TODO: assert_valid{module,function} *)

val run_then_write_module
  :  module_id:string
  -> filename:string
  -> unit t
  -> unit Or_codegen_error.t

val run_then_dump_module : module_id:string -> unit t -> unit Or_codegen_error.t
val use_builder : (Llvm.llbuilder -> 'a) -> 'a t
val use_context : (Llvm.llcontext -> 'a) -> 'a t
val use_module : (Llvm.llmodule -> 'a) -> 'a t

(* discorage this, as cached values become stale. - better to have
   [use_current_block] *)
val current_block : Llvm.llbasicblock t

(** fail due to an error which should have been caught in type inference*)
val impossible_error : string -> 'a t

(** fail due to attempting to compile a not-yet-implemented feature *)
val unsupported_feature_error : string -> 'a t
