open! Core
open! Import

module Expr : sig
  type t =
    | Value of value
    | Let of Variable.t * value * t
    (** [Let] provides polymorphic binding over total terms, although I
        currently restrict this to values for the evidence translation step
        (this may be unnecessary) *)
    | Let_mono of Variable.t * t * t
    (** [Let_mono] provides monomorphic binding, [Let_mono(x, e_x, e)] is
        equivalent to [(fun x -> e) e_x], but can be implemented more
        efficiently. There is no value restriction of [e_x], since it is not
        generalised *)
    | Application of t * t list
    | Construction of Constructor.t * t list
    | Seq of t * t
    (** evaluates first expression, then second. both may be of any type *)
    | If_then_else of t * t * t
    | Match of t * (Pattern.t * t) list
    | Operator of t * Operator.t * t
    | Unary_operator of Operator.Unary.t * t
    | Impure_built_in of impure_built_in
  [@@deriving sexp_of]

  (** expressions which can't reduce/evaluate *)
  and value =
    | Variable of Variable.t
    | Lambda of lambda
    | Fix_lambda of fix_lambda
    | Literal of Literal.t
    | Handler of handler
    (** takes a nullary funciton to be called under this handler *)
  [@@deriving sexp_of]

  (** monomorphic binding *)
  and lambda = Parameter.t list * t [@@deriving sexp_of]

  (** lambda which knows its own name *)
  and fix_lambda = Variable.t * lambda [@@deriving sexp_of]

  (** an effect handler *)
  and handler =
    { operations : (Operation_shape.t * op_handler) Variable.Map.t
    ; return_clause : op_handler option
    }
  [@@deriving sexp_of]

  (** handler clause for a single operation - part of a [handler] *)
  and op_handler =
    { op_argument : Parameter.t
    ; op_body : t
    }
  [@@deriving sexp_of]

  (** language builtins which actually perform 'external' side effects. These
      are not directly exposed to user code, but are called from toplevel
      operation handlers wrapping `main()`. They notably do not have the side
      effect of the operation they perform *)
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

  (** wrapper funciton which calls the user's main function, which is appended
      to programs. It may provide toplevel handers for e.g. console/io/exn/div
      effects. *)
  val entry_point : Decl.Fun.t
end
