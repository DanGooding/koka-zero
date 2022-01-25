open Core
open Koka_zero_util
open Import

(** monad encapsulating the mutable state of llvm code generation. It holds
    references to the llcontext, llmodule and llbuilder *)
type 'a t

include Monad.S with type 'a t := 'a t
include Monad_utils.S with type 'a t := 'a t

val run_then_write_module
  :  module_id:string
  -> filename:string
  -> unit t
  -> unit Or_codegen_error.t

val run_then_dump_module : module_id:string -> unit t -> unit Or_codegen_error.t
val use_builder : (Llvm.llbuilder -> 'a) -> 'a t
val use_context : (Llvm.llcontext -> 'a) -> 'a t
val use_module : (Llvm.llmodule -> 'a) -> 'a t

(** non-raising replacement for [Llvm.insertion_block] *)
val insertion_block : Llvm.llbasicblock option t

(** replacement for [Llvm.insertion_block] which makes the possible exception
    explicit, and produces a more descriptive error *)
val insertion_block_exn : Llvm.llbasicblock t

(** runs llvm verifier over the module, failing with a [Codegen.verifier_error]
    if it it is found to be invalid *)
val check_module_valid : unit t

(** run a computation locally within a given basic block, saving the previous
    insertion point and restoring it afterwards (if the builder was previously
    inserting) *)
val within_block : Llvm.llbasicblock -> f:(unit -> unit t) -> unit t

(** fail due to an error which should have been caught in type inference*)
val impossible_error : string -> 'a t

(** fail due to attempting to compile a not-yet-implemented feature *)
val unsupported_feature_error : string -> 'a t

(** create a unique symbol for a local name, by prefixing with its outer
    function's symbol, and adding a unique suffix *)
val symbol_of_local_name
  :  containing:Symbol_name.t
  -> Variable.t
  -> Symbol_name.t t

(** create a unique symbol for an anonymous local funciton, in the same way as
    [symbol_of_local_name] *)
val symbol_of_local_anonymous : containing:Symbol_name.t -> Symbol_name.t t
