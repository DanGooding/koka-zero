open! Core
open! Import

module Expr = struct
  type t =
    | Value of value
    | Let of Variable.t * value * t
    | Let_mono of Variable.t * t * t
    | Application of t * t list
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
    | Perform of perform
    | Handler of handler
  [@@deriving sexp_of]

  and lambda = Parameter.t list * t [@@deriving sexp_of]
  and fix_lambda = Variable.t * lambda [@@deriving sexp_of]

  and perform =
    { operation : Variable.t
    ; performed_effect : Effect.Label.t
    }
  [@@deriving sexp_of]

  and handler =
    { handled_effect : Effect.Label.t
    ; operations : (Operation_shape.t * op_handler) Variable.Map.t
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
end
