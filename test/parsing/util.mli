open! Core
open! Import

(** parse a koka program using [Private.parse_string_to_syntax], and print a
    sexp of the ast *)
val print_parse_to_syntax_result : string -> Syntax.program Or_static_error.t

(** simplify a program using [Private.simplify] and print a sexp of the minimal
    ast. This is intended to be passed the result of
    [print_parse_to_syntax_result] *)
val print_simplification_result : Syntax.program Or_static_error.t -> unit
