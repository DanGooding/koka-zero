open Core
module Syntax = Syntax

val parse_channel
  :  ?filename:string
  -> in_channel
  -> (Syntax.program, Koka_zero_util.Static_error.t) Result.t

val parse_string
  :  ?filename:string
  -> string
  -> (Syntax.program, Koka_zero_util.Static_error.t) Result.t
