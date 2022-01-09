open Koka_zero_evidence_translation
open Import

module Marker : sig
  type t = int [@@deriving sexp]
end

type t =
  | Primitive of primitive
  | Closure of closure
  | Pure of t
  | Yield of
      { marker : Marker.t
      ; op_clause : t (* TODO: is this at least a closure? *)
      ; resumption : t
      }
  | Effect_label of Effect_label.t
  | Marker of Marker.t
  | Hnd of hnd
  | Evidence of evidence
  | Evidence_vector of evidence_vector
[@@deriving sexp]

and primitive =
  | Int of int
  | Bool of bool
  | Unit
[@@deriving sexp]

and function_ =
  | Lambda of Evidence_passing_syntax.Expr.lambda
  | Fix_lambda of Evidence_passing_syntax.Expr.fix_lambda
[@@deriving sexp]

and closure = function_ * context

and hnd =
  { handled_effect : Effect_label.t
  ; operation_clauses : t Variable.Map.t
        (* TODO: are these at least closures? *)
  ; return_clause : t option
  }
[@@deriving sexp]

and evidence =
  { marker : Marker.t
  ; handler : hnd
  }
[@@deriving sexp]

and evidence_vector =
  | Nil
  | Cons of
      { label : Effect_label.t
      ; evidence : evidence
      ; tail : evidence_vector
      }
[@@deriving sexp]

and context = t Variable.Map.t [@@deriving sexp]

val empty_context : context
