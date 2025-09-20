open Core
open Import

module Marker = struct
  type t = int [@@deriving sexp_of]
end

type t =
  | Primitive of primitive
  | List of t list
  | Closure of closure
  | Ctl of ctl
  | Effect_label of Effect_label.t
  | Marker of Marker.t
  | Op of op
  | Hnd of hnd
  | Evidence of evidence
  | Evidence_vector of evidence_vector
[@@deriving sexp_of]

and ctl =
  | Pure of t
  | Yield of
      { marker : Marker.t
      ; op_clause : t
      ; resumption : t
      }
[@@deriving sexp_of]

and primitive =
  | Int of int
  | Bool of bool
  | Unit
[@@deriving sexp_of]

and function_ =
  | Lambda of Evidence_passing_syntax.Expr.lambda
  | Fix_lambda of Evidence_passing_syntax.Expr.fix_lambda
[@@deriving sexp_of]

and closure = function_ * context

and op =
  | Op_normal of closure
  | Op_tail of closure
[@@deriving sexp_of]

and hnd =
  { handled_effect : Effect_label.t
  ; operation_clauses : op Variable.Map.t
  }
[@@deriving sexp_of]

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

and context = t Variable.Map.t [@@deriving sexp_of]

let empty_context = Variable.Map.empty
