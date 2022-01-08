open Core
module Literal = Minimal_syntax.Literal
module Operator = Minimal_syntax.Operator
module Variable = Minimal_syntax.Variable

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
    [@@deriving sexp]

    and value =
      | Variable of Variable.t
      | Lambda of lambda
      | Fix_lambda of fix_lambda
      | Literal of Literal.t
      | Perform of perform
      | Handler of handler
    [@@deriving sexp]

    and lambda = Variable.t list * t [@@deriving sexp]

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
      { op_argument : Variable.t
      ; op_body : t
      }
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T
end

module Decl = struct
  module Effect = struct
    module Operation = struct
      type t =
        { argument : Type.Mono.t
        ; answer : Type.Mono.t
        }
      [@@deriving sexp]
    end

    type t =
      { name : Effect.Label.t
      ; operations : Operation.t Variable.Map.t
      }
    [@@deriving sexp]
  end

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
  type t =
    { declarations : Decl.t list
    ; has_entry_point : bool
    }
  [@@deriving sexp]
end
