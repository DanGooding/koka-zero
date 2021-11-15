open Core

module Literal : sig
  type t =
    | Int of int
    | Bool of bool
    | Unit
  [@@deriving sexp]
end

module Variable : Identifiable
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
  [@@deriving sexp]
  (* TODO: operators, handlers *)
end
