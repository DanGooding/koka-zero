open Core

val parse_channel : in_channel -> (Syntax.program, string) Result.t
val parse_string : string -> (Syntax.program, string) Result.t
