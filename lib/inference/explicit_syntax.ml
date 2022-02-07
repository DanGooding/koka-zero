open Core
module Literal = Minimal_syntax.Literal
module Operator = Minimal_syntax.Operator
module Keyword = Minimal_syntax.Keyword
module Parameter = Minimal_syntax.Parameter

module Expr = struct
  module T = struct
    type t =
      | Value of value
      | Let of Variable.t * value * t
      | Application of t * t list
      | Seq of t * t
      | If_then_else of t * t * t
      | Operator of t * Operator.t * t
      | Unary_operator of Operator.Unary.t * t
      | Impure_built_in of impure_built_in
    [@@deriving sexp]

    and value =
      | Variable of Variable.t
      | Lambda of lambda
      | Fix_lambda of fix_lambda
      | Literal of Literal.t
      | Perform of perform
      | Handler of handler
    [@@deriving sexp]

    and lambda = Parameter.t list * t [@@deriving sexp]

    and fix_lambda = Variable.t * lambda [@@deriving sexp]

    and perform =
      { operation : Variable.t
      ; performed_effect : Effect.Label.t
      }
    [@@deriving sexp]

    and handler =
      { handled_effect : Effect.Label.t
      ; operations : op_handler Variable.Map.t
      ; return_clause : op_handler option
      }
    [@@deriving sexp]

    and op_handler =
      { op_argument : Parameter.t
      ; op_body : t
      }
    [@@deriving sexp]

    and impure_built_in =
      | Impure_print_int of t
      | Impure_read_int
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T
end

module Decl = struct
  module Effect = Minimal_syntax.Decl.Effect

  module Fun = struct
    type t = Expr.fix_lambda [@@deriving sexp]
  end

  module T = struct
    type t =
      | Fun of Fun.t
      | Effect of Effect.t
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T
end

module Program = struct
  type t = { declarations : Decl.t list } [@@deriving sexp]
end
