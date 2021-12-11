(* TODO: is the aim to annotate the tree, or just to check it and then forget
   about types *)
open Core
open Koka_zero_util

val infer_type
  :  Minimal_syntax.Program.t
  -> (Type.Mono.t * Effect.t, Static_error.t) Result.t
