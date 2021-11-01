open Core

val parse_channel : in_channel -> Syntax.program Or_error.t
val parse_string : string -> Syntax.program Or_error.t
