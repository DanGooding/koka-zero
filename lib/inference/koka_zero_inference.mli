open Core
open Koka_zero_util
module Type = Type
module Minimal_syntax = Minimal_syntax

(* TODO: this should interface with [Koka_zero_parsing] *)
val infer_type
  :  Minimal_syntax.Program.t
  -> (Type.Mono.t * Effect.t, Static_error.t) Result.t
