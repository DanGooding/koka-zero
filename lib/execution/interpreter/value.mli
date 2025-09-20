open! Core
open! Import

module Marker : sig
  type t = int [@@deriving sexp_of]
end

type t =
  | Primitive of primitive
  | Closure of closure
  | Ctl of ctl
  | Effect_label of Effect_label.t
  | Marker of Marker.t
  | Op of op
  | Hnd of hnd
  | Evidence of evidence
  | Evidence_vector of evidence_vector
[@@deriving sexp_of]

(** control monad *)
and ctl =
  | Pure of t
  | Yield of
      { marker : Marker.t
      ; op_clause : t
      ; resumption : t
      }
[@@deriving sexp_of]

(** language level primitives *)
and primitive =
  | Int of int
  | Bool of bool
  | Unit
[@@deriving sexp_of]

and function_ =
  | Lambda of Evidence_passing_syntax.Expr.lambda
  | Fix_lambda of Evidence_passing_syntax.Expr.fix_lambda
[@@deriving sexp_of]

(** function and captured environment *)
and closure = function_ * context

(** operation clause *)
and op =
  | Op_normal of closure
  | Op_tail of closure
[@@deriving sexp_of]

(** handler (not the function, just the clauses) *)
and hnd =
  { handled_effect : Effect_label.t
  ; operation_clauses : op Variable.Map.t
  }
[@@deriving sexp_of]

(** entry in an evidence vector *)
and evidence =
  { marker : Marker.t
  ; handler : hnd
  ; handler_site_vector : evidence_vector
  }
[@@deriving sexp_of]

and evidence_vector =
  | Evv_nil
  | Evv_cons of
      { label : Effect_label.t
      ; evidence : evidence
      ; tail : evidence_vector
      }
[@@deriving sexp_of]

(** environment *)
and context = t Variable.Map.t [@@deriving sexp_of]

val empty_context : context
