open Core

(* TODO: expose syntax abstractly? *)
module Syntax = Syntax

val parse_channel
  :  ?filename:string
  -> in_channel
  -> (Syntax.program, string) Result.t

val parse_string
  :  ?filename:string
  -> string
  -> (Syntax.program, string) Result.t
