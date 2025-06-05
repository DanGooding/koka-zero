open! Core
open! Import

module Expr : sig
  type t =
    | Value of value
    | Let of Variable.t * value * t (** [Let] provides polymorphic binding *)
    | Let_mono of Variable.t * t * t
    (** [Let_mono] provides monomorphic binding *)
    | Application of t * t list
    | Seq of t * t
    (** evaluates first expression, then second. both may be of any type *)
    | If_then_else of t * t * t
    | Operator of t * Operator.t * t
    | Unary_operator of Operator.Unary.t * t
    | Impure_built_in of impure_built_in
  [@@deriving sexp_of]

  and value =
    | Variable of Variable.t
    (** guaranteed to stand for a value, not an operation name *)
    | Lambda of lambda
    | Fix_lambda of fix_lambda
    | Literal of Literal.t
    | Perform of perform
    | Handler of handler
    (** takes a nullary funciton to be called under this handler *)
  [@@deriving sexp_of]

  (** monomorphic binding *)
  and lambda = Parameter.t list * t [@@deriving sexp_of]

  (** lambda which knows its own name *)
  and fix_lambda = Variable.t * lambda [@@deriving sexp_of]

  (** awaits one argument then performs the specified operation *)
  and perform =
    { operation : Variable.t
    ; performed_effect : Effect.Label.t
    }
  [@@deriving sexp_of]

  (** an effect handler *)
  and handler =
    { handled_effect : Effect.Label.t
    ; operations : (Operation_shape.t * op_handler) Variable.Map.t
    ; return_clause : op_handler option
    }
  [@@deriving sexp_of]

  (** handler clause for a single operation - part of a [handler] *)
  and op_handler =
    { op_argument : Parameter.t
    ; op_body : t
    }
  [@@deriving sexp_of]

  (** interaction with the outside world *)
  and impure_built_in =
    | Impure_println
    | Impure_print_int of
        { value : t
        ; newline : bool
        }
    | Impure_read_int
  [@@deriving sexp_of]
end

module Decl : sig
  module Fun : sig
    (** toplevel function - implicitly generalised *)
    type t = Expr.fix_lambda [@@deriving sexp_of]
  end

  type t =
    | Fun of Fun.t
    | Effect of Effect_decl.t
  [@@deriving sexp_of]
end

module Program : sig
  type t = { declarations : Decl.t list } [@@deriving sexp_of]
end
