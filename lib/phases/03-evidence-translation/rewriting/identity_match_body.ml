open! Core
open! Import
open Evidence_passing_syntax

let remove_identity_match =
  Modified.original_for_none (fun expr ->
    match (expr : Expr.t) with
    | Match_ctl_pure { subject; pure_branch = x, Construct_pure (Variable x') }
      when [%equal: Variable.t] x x' -> Some subject
    | Match_ctl_pure _ -> None
    | Match_ctl
        { subject
        ; pure_branch = x, Construct_pure (Variable x')
        ; yield_branch =
            ( m
            , c
            , k
            , Construct_yield
                { marker = Variable m'
                ; op_clause = Variable c'
                ; resumption = Variable k'
                } )
        }
      when [%equal: Variable.t * Variable.t * Variable.t * Variable.t]
             (x, m, c, k)
             (x', m', c', k') -> Some subject
    | Match_ctl _ -> None
    | Match_op
        { subject
        ; normal_branch = n, Construct_op_normal (Variable n')
        ; tail_branch = t, Construct_op_tail (Variable t')
        }
      when [%equal: Variable.t * Variable.t] (n, t) (n', t') -> Some subject
    | Match_op _ -> None
    | Fresh_marker | Nil_evidence_vector | Variable _
    | Let (_, _, _, _)
    | Lambda _ | Fix_lambda _
    | Application (_, _, _)
    | Construction _ | Literal _
    | If_then_else (_, _, _)
    | Operator (_, _, _)
    | Unary_operator (_, _)
    | Construct_pure _ | Construct_yield _
    | Markers_equal (_, _)
    | Effect_label _
    | Construct_op_normal _
    | Construct_op_tail _
    | Construct_handler _
    | Select_operation _
    | Cons_evidence_vector _
    | Lookup_evidence _
    | Get_evidence_marker _
    | Get_evidence_handler _
    | Get_evidence_handler_site_vector _
    | Impure_built_in _ -> None)
;;
