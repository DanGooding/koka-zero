open Core
open Koka_zero_evidence_translation
open Import

module Marker = struct
  type t = int [@@deriving sexp]
end

module T = struct
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
  [@@deriving sexp]

  and ctl =
    | Pure of t
    | Yield of
        { marker : Marker.t
        ; op_clause : t (* TODO: is this at least a closure? *)
        ; resumption : t
        }
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

  and op =
    | Op_normal of closure
    | Op_tail of closure
  [@@deriving sexp]

  and hnd =
    { handled_effect : Effect_label.t
    ; operation_clauses : op Variable.Map.t
          (* TODO: return_clause omitted for now *)
    }
  [@@deriving sexp]

  and evidence =
    { marker : Marker.t
    ; handler : hnd
    ; handler_site_vector : evidence_vector
    }
  [@@deriving sexp]

  and evidence_vector =
    | Evv_nil
    | Evv_cons of
        { label : Effect_label.t
        ; evidence : evidence
        ; tail : evidence_vector
        }
  [@@deriving sexp]

  and context = t Variable.Map.t [@@deriving sexp]
end (* disable "fragile-match" for generated code *) [@warning "-4"]

include T

let empty_context = Variable.Map.empty
