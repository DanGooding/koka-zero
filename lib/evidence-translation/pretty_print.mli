open Evidence_passing_syntax

(** print an expression to equivalent OCaml code (which can then be run through
    ocamlformat) *)
val print_expr : Expr.t -> string

val print_program : Program.t -> string
