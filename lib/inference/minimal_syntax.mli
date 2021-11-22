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

module Variable : Identifiable.S
(* TODO: work out identifier/var_id/wildcard etc.*)

module Expr : sig
  type t =
    | Variable of Variable.t
    | Let of Variable.t * t * t
    | Lambda of Variable.t * t
    | Fix of Variable.t * t
    (* TODO: syntactically, `fix` can only wrap a lambda - perhaps enforce
       this? *)
    | Application of t * t
    | Literal of Literal.t
    | If_then_else of t * t * t
    | Operator of t * Operator.t * t
    | Unary_operator of Operator.Unary.t * t
  [@@deriving sexp]
  (* TODO: operators, handlers *)
end
