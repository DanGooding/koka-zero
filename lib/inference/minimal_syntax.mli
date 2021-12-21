open Core

module Literal : sig
  type t =
    | Int of int
    | Bool of bool
    | Unit
  [@@deriving sexp]
end

module Operator : sig
  module Int : sig
    type t =
      | Plus
      | Minus
      | Times
      | Divide
      | Modulo
      | Equals
      | Less_than
    [@@deriving sexp]
  end

  module Bool : sig
    module Unary : sig
      type t = Not [@@deriving sexp]
    end

    type t =
      | And
      | Or
    [@@deriving sexp]
  end

  module Unary : sig
    type t = Bool of Bool.Unary.t [@@deriving sexp]
  end

  type t =
    | Int of Int.t
    | Bool of Bool.t
  [@@deriving sexp]
end

(* TODO: work out identifier/var_id/wildcard etc.*)
module Variable : Identifiable.S

module Keyword : sig
  (* TODO: ensure resume is not used in a first-class way *)
  val resume : Variable.t
end

module Expr : sig
  type t =
    | Variable of Variable.t
    | Let of Variable.t * t * t
    | Lambda of lambda
    | Fix_lambda of Variable.t * lambda
    | Application of t * t list
    | Literal of Literal.t
    | If_then_else of t * t * t
    | Operator of t * Operator.t * t
    | Unary_operator of Operator.Unary.t * t
    | Handle of handler * t (** evaluates its suject under the handler *)
  [@@deriving sexp]

  and lambda = Variable.t list * t [@@deriving sexp]

  (** an effect handler *)
  and handler =
    { operations : op_handler Variable.Map.t
    ; return_clause : op_handler option
    }
  [@@deriving sexp]

  (** handler clause for a single operation - part of a [handler] *)
  and op_handler =
    { op_argument : Variable.t
          (* TODO: extend to multiple args (requires checking against
             declaration) *)
    ; op_body : t
    }
  [@@deriving sexp]
end

module Effect_decl : sig
  module Operation : sig
    type t =
      { (* TODO: different shapes (fun/var/ctl/except) *)
        argument : Type.Mono.t
            (* TODO: should annotations be separate, then converted into
               types? *)
      ; answer : Type.Mono.t
      }
    [@@deriving sexp]
  end

  type t =
    { (* TODO: should be defined here / or should just use [Variable]? *)
      name : Effect.Label.t
    ; operations : Operation.t Variable.Map.t
    }
  [@@deriving sexp]
end

module Program : sig
  type t =
    { effect_declarations : Effect_decl.t list
    ; body : Expr.t
    }
  [@@deriving sexp]
  (* TODO: add toplevel functions, main as entry point *)
end
