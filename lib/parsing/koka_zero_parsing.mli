open Core
open Import

val parse_channel
  :  ?filename:string
  -> In_channel.t
  -> Koka_zero_inference.Minimal_syntax.Program.t Or_static_error.t

val parse_string
  :  ?filename:string
  -> string
  -> Koka_zero_inference.Minimal_syntax.Program.t Or_static_error.t

(** exposes the separate stages of parsing, for testing *)
module Private : sig
  module Syntax = Syntax

  val parse_channel_to_syntax
    :  ?filename:string
    -> In_channel.t
    -> Syntax.program Or_static_error.t

  val parse_string_to_syntax
    :  ?filename:string
    -> string
    -> Syntax.program Or_static_error.t

  val simplify
    :  Syntax.program
    -> Koka_zero_inference.Minimal_syntax.Program.t Or_static_error.t
end
