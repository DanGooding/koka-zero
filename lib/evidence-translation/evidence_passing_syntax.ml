open Core
module Variable = Koka_zero_inference.Minimal_syntax.Variable
module Literal = Koka_zero_inference.Minimal_syntax.Literal
module Operator = Koka_zero_inference.Minimal_syntax.Operator

module Expr = struct
  module T = struct
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
          ; pure_branch : lambda
          ; yield_branch : lambda
          }
      | Fresh_marker
      | Markers_equal of t * t
      | Construct_handler of
          { handled_effect : Koka_zero_inference.Effect.Label.t
          ; operation_clauses : t Variable.Map.t
          ; return_clause : t option
          }
      | Effect_label of Koka_zero_inference.Effect.Label.t
      | Cons_evidence_vector of
          { label : t
          ; marker : t
          ; handler : t
          ; vector_tail : t
          }
      | Lookup_evidence of
          { label : t
          ; vector : t
          }
      | Get_evidence_marker of t
      | Get_evidence_handler of t
      | Select_operation of Koka_zero_inference.Effect.Label.t * Variable.t * t
    (* TODO: perhaps this should be a function? (otherwise it needs to be
       wrapped at every usage) - TODO: or should this already be changed to an
       index into a record? *)
    [@@deriving sexp]

    and lambda = Variable.t list * t [@@deriving sexp]

    and fix_lambda = Variable.t * lambda [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T
end

module Program = struct
  module Effect_decl = struct
    type t =
      { name : Koka_zero_inference.Effect.Label.t
      ; operations : Variable.Set.t
      }
    [@@deriving sexp]
  end

  module Fun_decl = struct
    type t = Expr.fix_lambda [@@deriving sexp]
  end

  type t =
    { effect_declarations : Effect_decl.t list
    ; fun_declarations : Fun_decl.t list
    ; has_main : bool
    }
  [@@deriving sexp]
end
