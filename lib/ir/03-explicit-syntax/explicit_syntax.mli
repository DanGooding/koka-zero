open! Core
open! Import

module Expr : sig
  type 'e t =
    | Value of 'e value
    | Let of Variable.t * 'e value * 'e t
    (** [Let] provides polymorphic binding *)
    | Let_mono of Parameter.t * 'e t * 'e t
    (** [Let_mono] provides monomorphic binding *)
    | Application of 'e t * 'e t list * 'e
    (** ['e] is the effect produced by this application *)
    | Construction of Constructor.t * 'e t list
    | Seq of 'e t * 'e t
    (** evaluates first expression, then second. both may be of any type *)
    | If_then_else of 'e t * 'e t * 'e t
    | Match of 'e t * Pattern.Scrutinee.t * (Pattern.t * 'e t) list
    | Operator of 'e t * Operator.t * 'e t
    | Unary_operator of Operator.Unary.t * 'e t
    | Impure_built_in of 'e impure_built_in
  [@@deriving sexp_of]

  and 'e value =
    | Variable of Variable.t
    (** guaranteed to stand for a value, not an operation name *)
    | Lambda of 'e lambda
    | Fix_lambda of 'e fix_lambda
    | Literal of Literal.t
    | Perform of perform
    | Handler of 'e handler
    (** takes a nullary funciton to be called under this handler *)
  [@@deriving sexp_of]

  (** monomorphic binding *)
  and 'e lambda = Parameter.t list * 'e t [@@deriving sexp_of]

  (** lambda which knows its own name *)
  and 'e fix_lambda = Variable.t * 'e lambda [@@deriving sexp_of]

  (** awaits one argument then performs the specified operation *)
  and perform =
    { operation : Variable.t
    ; performed_effect : Effect.Label.t
    }
  [@@deriving sexp_of]

  (** an effect handler *)
  and 'e handler =
    { handled_effect : Effect.Label.t
    ; operations : (Operation_shape.t * 'e op_handler) Variable.Map.t
    ; return_clause : 'e op_handler option
    }
  [@@deriving sexp_of]

  (** handler clause for a single operation - part of a [handler] *)
  and 'e op_handler =
    { op_argument : Parameter.t
    ; op_body : 'e t
    }
  [@@deriving sexp_of]

  (** interaction with the outside world *)
  and 'e impure_built_in =
    | Impure_println
    | Impure_print_int of
        { value : 'e t
        ; newline : bool
        }
    | Impure_read_int
  [@@deriving sexp_of]

  val map_effect : 'e t -> f:('e -> 'f) -> 'f t
end

module Decl : sig
  module Fun : sig
    (** toplevel function - implicitly generalised *)
    type 'e t = 'e Expr.fix_lambda [@@deriving sexp_of]
  end

  type 'e t =
    | Fun of 'e Fun.t
    | Effect of Effect_decl.t
  [@@deriving sexp_of]

  val map_effect : 'e t -> f:('e -> 'f) -> 'f t
end

module Program : sig
  type 'e t = { declarations : 'e Decl.t list } [@@deriving sexp_of]

  val map_effect : 'e t -> f:('e -> 'f) -> 'f t
end
