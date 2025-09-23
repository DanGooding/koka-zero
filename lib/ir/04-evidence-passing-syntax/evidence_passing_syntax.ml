open Core
open Import

module Type = struct
  type t =
    | Ctl
    | Pure
  [@@deriving sexp_of, equal]
end

module Expr = struct
  type t =
    | Variable of Variable.t
    | Let of Parameter.t * Type.t * t * t
    | Lambda of lambda
    | Fix_lambda of fix_lambda
    | Application of t * (t * Type.t) list * Type.t
    | Construction of Constructor.t * t list
    | Tuple_construction of t list
    | Literal of Literal.t
    | If_then_else of t * t * t
    | Match of t * Pattern.Scrutinee.t * (Pattern.t * t) list
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
        ; pure_branch : Parameter.t * t
        ; yield_branch : Variable.t * Variable.t * Variable.t * t
        }
    | Match_ctl_pure of
        { subject : t
        ; pure_branch : Parameter.t * t
        }
    | Fresh_marker
    | Markers_equal of t * t
    | Effect_label of Effect.Label.t
    | Construct_op_normal of t
    | Construct_op_tail of t
    | Match_op of
        { subject : t
        ; normal_branch : Variable.t * t
        ; tail_branch : Variable.t * t
        }
    | Construct_handler of
        { handled_effect : Effect.Label.t
        ; operation_clauses : t Variable.Map.t
        }
    | Select_operation of Effect.Label.t * Variable.t * t
    | Nil_evidence_vector
    | Cons_evidence_vector of
        { label : t
        ; marker : t
        ; handler : t
        ; handler_site_vector : t
        ; vector_tail : t
        }
    | Lookup_evidence of
        { label : t
        ; vector : t
        }
    | Get_evidence_marker of t
    | Get_evidence_handler of t
    | Get_evidence_handler_site_vector of t
    | Impure_built_in of impure_built_in
  [@@deriving sexp_of]

  and lambda = (Parameter.t * Type.t) list * Type.t * t [@@deriving sexp_of]
  and fix_lambda = Variable.t * lambda [@@deriving sexp_of]

  and impure_built_in =
    | Impure_println
    | Impure_print_int of
        { value : t
        ; newline : bool
        }
    | Impure_read_int
  [@@deriving sexp_of]
end

module Program = struct
  module Effect_decl = struct
    type t =
      { name : Effect.Label.t
      ; operations : Variable.Set.t
      }
    [@@deriving sexp_of]
  end

  module Fun_decl = struct
    type t = Expr.fix_lambda [@@deriving sexp_of]
  end

  type t =
    { effect_declarations : Effect_decl.t list
    ; fun_declarations : Fun_decl.t list
    ; entry_expr : Expr.t
    }
  [@@deriving sexp_of]
end
