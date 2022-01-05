open Core

(* TODO: should variable be a variant to allow generation of fresh ones? *)
module Variable = Koka_zero_inference.Minmal_syntax.Variable
module Literal = Koka_zero_inference.Minimal_syntax.Literal
module Operator = Koka_zero_inference.Minimal_syntax.Operator

(* TODO: do I want types? definitely want effects - for evidence vector
   lookup *)

module Marker : sig
  type t = int [@@deriving sexp]
end

module Expr : sig
  type t =
    | Variable of Variable.t
    (* | Let, or convert to lamdas somehow? *)
    | Lambda of lambda
    | Fix_lambda of fix_lambda
    | Application of t list * t
    | Literal of Literal.t
    | If_then_else of t * t * t
    | Operator of t * Operator.t * t
    | Unary_operator of t * Operator.Unary.t
    (* TODO: these are all internal... *)
    (* TODO: this is just an AST, these are just type constructors *)
    (* TODO: need pattern matching on these *)
    | Construct_pure of t
    | Construct_yield of
        { marker : t
        ; op_clause : t
        ; resumption : t
        }
    | Match_ctl of
        { subject : t
        ; pure_branch : lambda (* gets one argument *)
        ; yield_branch : lambda (* gets one argument per field in Yield *)
        }
    (* TODO: note evidence vectors are not first class - can have more sensible
       primitives if desired *)
    (* evaluates to a new unique marker *)
    | Fresh_marker
    | Markers_equal of t * t
    | Cons_evidence_vector of
        { label : t
        ; marker : t
        ; handler : t
              (* ; old_evidence : t *)
              (* seems to only be for tail resumptions *)
        ; vector_tail : t
        }
    | Lookup_evidence of
        { label : t
        ; vector : t
        }
    | Get_evidence_marker : t (* evidence entry -> marker *)
    | Get_evidence_handler : t (* evidence entry -> handler*)
    (* TODO: constructor for Hnd (handler = record of operations) even though
       this is never done at runtime *)
    (* primitive to get an operation from a handler's runtime representation *)
    | Select_operation of int (* TODO: should this be a name instead? *) * t
  [@@deriving sexp]

  and lambda = Variable.t list * t [@@deriving sexp]

  and fix_lambda = Variable.t * lambda [@@deriving sexp]
end
