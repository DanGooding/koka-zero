open Core
open Import

(** monad encapsulating the mutable state of llvm code generation. It holds
    references to the llcontext, llmodule and llbuilder *)
type 'a t

include Monad.S with type 'a t := 'a t

val run_then_write_module
  :  module_id:string
  -> filename:string
  -> unit t
  -> unit Or_codegen_error.t

val run_then_dump_module : module_id:string -> unit t -> unit Or_codegen_error.t

(* TODO: obviously this can leak the mutable state, but that is clear abuse of
   the API. Perhaps I could wrap every Llvm function, but that would be very
   time consuming *)
(* TODO: is requiring the function to be pure too restrictive? *)
val use_builder : (Llvm.llbuilder -> 'a) -> 'a t
val use_context : (Llvm.llcontext -> 'a) -> 'a t
val use_module : (Llvm.llmodule -> 'a) -> 'a t

(* discorage this, as cached values become stale. - better to have
   [use_current_block] *)
val current_block : Llvm.llbasicblock t

(** fail with the given error message *)
val codegen_error : string -> 'a t
