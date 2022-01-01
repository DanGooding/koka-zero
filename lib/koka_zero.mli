open Core
open Koka_zero_util

(* TODO: expose syntax abstractly? *)
module Minimal_syntax = Koka_zero_inference.Minimal_syntax
module Static_error = Static_error

val parse_channel
  :  ?filename:string
  -> in_channel
  -> (Minimal_syntax.Program.t, Koka_zero_util.Static_error.t) Result.t

val parse_string
  :  ?filename:string
  -> string
  -> (Minimal_syntax.Program.t, Koka_zero_util.Static_error.t) Result.t
