open Core

module Literal : sig
  type t =
    | Int of int
    | Bool of bool
    | Unit
  [@@deriving sexp]
end

(* TODO should be [Identifiable] externally *)
module Variable = Identifiable (* TODO: work out identifier/var_id/etc.*)

module Expr : sig
  type t =
    | Literal of Literal.t
    | Variable of Variable.t
    | Let of Variable.t * t * t
    | Lambda of Variable.t * t
    | Application of t * t
    | Fix of t (* TODO: or of lambda? *)
    | If_then_else of t * t * t
  [@@deriving sexp]
  (* TODO: operators, handlers *)
end
