open Evidence_passing_syntax

(** print an expression to equivalent OCaml code, which can then be run through
    ocamlformat for inspection. The produced code will not typecheck and is not
    intended for execution *)
val print_expr : Expr.t -> string

val print_program : Program.t -> string
