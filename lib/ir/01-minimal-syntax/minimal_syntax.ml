open! Core
open! Import

module Expr = struct
  type t =
    | Value of value
    | Let of Variable.t * value * t
    | Let_mono of Variable.t * t * t
    | Application of t * t list
    | Construction of Constructor.t * t list
    | Seq of t * t
    | If_then_else of t * t * t
    | Operator of t * Operator.t * t
    | Unary_operator of Operator.Unary.t * t
    | Impure_built_in of impure_built_in
  [@@deriving sexp_of]

  and value =
    | Variable of Variable.t
    | Lambda of lambda
    | Fix_lambda of fix_lambda
    | Literal of Literal.t
    | Handler of handler
  [@@deriving sexp_of]

  and lambda = Parameter.t list * t [@@deriving sexp_of]
  and fix_lambda = Variable.t * lambda [@@deriving sexp_of]

  and handler =
    { operations : (Operation_shape.t * op_handler) Variable.Map.t
    ; return_clause : op_handler option
    }
  [@@deriving sexp_of]

  and op_handler =
    { op_argument : Parameter.t
    ; op_body : t
    }
  [@@deriving sexp_of]

  and impure_built_in =
    | Impure_println
    | Impure_print_int of
        { value : t
        ; newline : bool
        }
    | Impure_read_int
  [@@deriving sexp_of]
end

module Decl = struct
  module Fun = struct
    type t = Expr.fix_lambda [@@deriving sexp_of]
  end

  type t =
    | Fun of Fun.t
    | Effect of Effect_decl.t
  [@@deriving sexp_of]
end

module Program = struct
  type t = { declarations : Decl.t list } [@@deriving sexp_of]

  (** {[
        fun entry_point() {
          with handler {
            fun println(_)     { impure_println(); };
            fun println-int(x) { impure_print_int(x, newline=true); };
            fun print-int(x)   { impure_print_int(x, newline=false); };
            fun read-int(_)    { impure_read_int(); };
          };
          main();
          ()
        }
      ]} *)
  let entry_point =
    let console_handler =
      let println_clause =
        { Expr.op_argument = Parameter.Wildcard
        ; op_body = Expr.Impure_built_in Expr.Impure_println
        }
      in
      let println_int_clause =
        let arg = Variable.of_language_internal "x" in
        let value = Expr.Value (Expr.Variable arg) in
        let newline = true in
        { Expr.op_argument = Parameter.Variable arg
        ; op_body =
            Expr.Impure_built_in (Expr.Impure_print_int { value; newline })
        }
      in
      let print_int_clause =
        let arg = Variable.of_language_internal "x" in
        let value = Expr.Value (Expr.Variable arg) in
        let newline = false in
        { Expr.op_argument = Parameter.Variable arg
        ; op_body =
            Expr.Impure_built_in (Expr.Impure_print_int { value; newline })
        }
      in
      let read_int_clause =
        { Expr.op_argument = Parameter.Wildcard
        ; op_body = Expr.Impure_built_in Expr.Impure_read_int
        }
      in
      let operations =
        [ Keyword.println, (Operation_shape.Fun, println_clause)
        ; Keyword.println_int, (Operation_shape.Fun, println_int_clause)
        ; Keyword.print_int, (Operation_shape.Fun, print_int_clause)
        ; Keyword.read_int, (Operation_shape.Fun, read_int_clause)
        ]
        |> Variable.Map.of_alist_exn
      in
      { Expr.operations; return_clause = None }
    in
    ( Keyword.entry_point
    , ( []
      , Expr.Seq
          ( Expr.Application
              ( Expr.Value (Expr.Handler console_handler)
              , [ Expr.Value (Expr.Variable Keyword.main) ] )
          , Expr.Value (Expr.Literal Literal.Unit) ) ) )
  ;;
end
