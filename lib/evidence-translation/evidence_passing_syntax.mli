open Import
module Literal = Koka_zero_inference.Minimal_syntax.Literal
module Operator = Koka_zero_inference.Minimal_syntax.Operator

module Expr : sig
  type t =
    | Variable of Variable.t
    | Lambda of lambda
    | Fix_lambda of fix_lambda
    | Application of t * t list
    | Literal of Literal.t
    | If_then_else of t * t * t
    | Operator of t * Operator.t * t
    | Unary_operator of Operator.Unary.t * t
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
    | Fresh_marker (** evaluates to a new unique marker *)
    | Markers_equal of t * t
    | Construct_handler of
        { handled_effect : Effect.Label.t
        ; operation_clauses : t Variable.Map.t
        ; return_clause : t option
        } (** constructor for a `Hnd`, passed to [handler] *)
    | Effect_label of Effect.Label.t
        (** literal effect label - passed to [handler]/[perform] *)
    (* TODO: note evidence vectors are not first class - can have more sensible
       primitives if desired *)
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
    | Get_evidence_marker of t (* evidence entry -> marker *)
    | Get_evidence_handler of t (* evidence entry -> handler *)
    | Select_operation of Effect.Label.t * Variable.t * t
        (** primitive to get an operation from a handler's runtime
            representation **)
  [@@deriving sexp]

  and lambda = Variable.t list * t [@@deriving sexp]

  and fix_lambda = Variable.t * lambda [@@deriving sexp]
end

module Program : sig
  module Effect_decl : sig
    type t =
      { name : Koka_zero_inference.Effect.Label.t
      ; operations : Variable.Set.t
      }
    [@@deriving sexp]
  end

  module Fun_decl : sig
    type t = Expr.fix_lambda [@@deriving sexp]
  end

  type t =
    { effect_declarations : Effect_decl.t list
    ; fun_declarations : Fun_decl.t list
    ; has_entry_point : bool (* TODO: do NOT attempt a module system! *)
    }
  [@@deriving sexp]
end
