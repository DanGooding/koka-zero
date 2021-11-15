open Core
open Koka_zero_parsing
open Koka_zero_util

(* TODO: expose syntax abstractly? *)
module Syntax = Syntax
module Static_error = Static_error

val parse_channel
  :  ?filename:string
  -> in_channel
  -> (Syntax.program, Static_error.t) Result.t

val parse_string
  :  ?filename:string
  -> string
  -> (Syntax.program, Static_error.t) Result.t
