open! Core
open Import

module Type : sig
  type t =
    | Ctl
    (** either a Pure value or a Yield. this is the result of calling any effectful function.
        The contents can be extracted by Match_ctl *)
    | Pure (** simply a value, can be used directly *)
  [@@deriving sexp_of, equal]
end

module Expr : sig
  type t =
    | Variable of Variable.t
    | Let of Parameter.t * Type.t * t * t
    (** local binding [let y: t = x in e] *)
    | Lambda of lambda
    | Fix_lambda of fix_lambda
    | Application of t * (t * Type.t) list * Type.t
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
        ; pure_branch : Variable.t * t (* gets one argument *)
        ; yield_branch : Variable.t * Variable.t * Variable.t * t
          (* gets one argument per field in Yield *)
        }
    | Fresh_marker (** evaluates to a new unique marker *)
    | Markers_equal of t * t
    | Effect_label of Effect.Label.t
    (** literal effect label - passed to [handler]/[perform] *)
    | Construct_op_normal of t (** constructor for an operation *)
    | Construct_op_tail of t (** constructor for a tail resumptive operation *)
    | Match_op of
        { subject : t
        ; normal_branch : Variable.t * t
        ; tail_branch : Variable.t * t
        }
    | Construct_handler of
        { handled_effect : Effect.Label.t
        ; operation_clauses : t Variable.Map.t
        } (** constructor for a `Hnd`, passed to [handler] *)
    | Select_operation of Effect.Label.t * Variable.t * t
    (** primitive to get an operation from a handler's runtime
        representation **)
    (* TODO: note evidence vectors are not first class - can have more sensible
       primitives if desired *)
    | Nil_evidence_vector
    | Cons_evidence_vector of
        { label : t
        ; marker : t
        ; handler : t
        ; handler_site_vector : t
          (** evidence vector at handler - used for evaluating tail
              resumptive operations in-place *)
        ; vector_tail : t
        }
    | Lookup_evidence of
        { label : t
        ; vector : t
        }
    | Get_evidence_marker of t (* evidence entry -> marker *)
    | Get_evidence_handler of t (* evidence entry -> handler *)
    | Get_evidence_handler_site_vector of t
    (* evidence entry -> evidence vector *)
    | Impure_built_in of impure_built_in
  [@@deriving sexp_of]

  and lambda = (Parameter.t * Type.t) list * Type.t * t [@@deriving sexp_of]
  and fix_lambda = Variable.t * lambda [@@deriving sexp_of]

  (** interaction with the outside world *)
  and impure_built_in =
    | Impure_println
    | Impure_print_int of
        { value : t
        ; newline : bool
        }
    | Impure_read_int
  [@@deriving sexp_of]
end

module Program : sig
  module Effect_decl : sig
    type t =
      { name : Effect.Label.t
      ; operations : Variable.Set.t
      }
    [@@deriving sexp_of]
  end

  module Fun_decl : sig
    type t = Expr.fix_lambda [@@deriving sexp_of]
  end

  type t =
    { effect_declarations : Effect_decl.t list
    ; fun_declarations : Fun_decl.t list
    ; entry_expr : Expr.t
      (** expression to run the program: runs entry-point with an empty
          evidence vector *)
    }
  [@@deriving sexp_of]
end
