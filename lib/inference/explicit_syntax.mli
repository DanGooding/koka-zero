module Literal = Minimal_syntax.Literal
module Operator = Minimal_syntax.Operator
module Keyword = Minimal_syntax.Keyword

module Expr : sig
  type t =
    | Value of value
    | Let of Variable.t * value * t (** [Let] provides polymorphic binding *)
    | Application of t * t list
    | Seq of t * t
        (** evaluates first expression, then second. both may be of any type *)
    | If_then_else of t * t * t
    | Operator of t * Operator.t * t
    | Unary_operator of Operator.Unary.t * t
  [@@deriving sexp]

  and value =
    | Variable of Variable.t
        (** guaranteed to stand for a value, not an operation name *)
    | Lambda of lambda
    | Fix_lambda of fix_lambda
    | Literal of Literal.t
    | Perform of perform
    | Handler of handler
        (** takes a nullary funciton to be called under this handler *)
  [@@deriving sexp]

  (** monomorphic binding *)
  and lambda = Variable.t list * t [@@deriving sexp]

  (** lambda which knows its own name *)
  and fix_lambda = Variable.t * lambda [@@deriving sexp]

  (** awaits one argument then performs the specified operation *)
  and perform =
    { operation : Variable.t
    ; performed_effect : Effect.Label.t
    }
  [@@deriving sexp]

  (** an effect handler *)
  and handler =
    { handled_effect : Effect.Label.t
    ; operations : op_handler Variable.Map.t
    ; return_clause : op_handler option
    }
  [@@deriving sexp]

  (** handler clause for a single operation - part of a [handler] *)
  and op_handler =
    { op_argument : Variable.t
    ; op_body : t
    }
  [@@deriving sexp]
end

module Decl : sig
  module Effect = Minimal_syntax.Decl.Effect

  module Fun : sig
    (** toplevel function - implicitly generalised *)
    type t = Expr.fix_lambda [@@deriving sexp]
  end

  type t =
    | Fun of Fun.t
    | Effect of Effect.t
  [@@deriving sexp]
end

module Program : sig
  type t =
    { declarations : Decl.t list
    ; has_entry_point : bool
    }
  [@@deriving sexp]
end
