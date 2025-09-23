open! Core
open! Import
open Evidence_passing_syntax

let rec case_reconstructs_parameter ((parameter, body) : Parameter.t * Expr.t)
  : bool
  =
  match parameter with
  | Wildcard -> false
  | Variable v ->
    (match[@warning "-4"] body with
     | Variable v' when [%equal: Variable.t] v v' -> true
     | _ -> false)
  | Tuple params ->
    (match[@warning "-4"] body with
     | Tuple_construction args ->
       (match List.zip params args with
        | Unequal_lengths -> false
        | Ok pairs -> List.for_all pairs ~f:case_reconstructs_parameter)
     | _ -> false)
;;

let case_reconstructs_pattern ((pattern, body) : Pattern.t * Expr.t) : bool =
  match pattern with
  | Parameter ((Wildcard | Variable _ | Tuple _) as parameter) ->
    (* This case matches all paramter variants, but for some reason
       the fragile-match warning complains if we don't list them all out. *)
    case_reconstructs_parameter (parameter, body)
  | Literal lit ->
    (match[@warning "-4"] body with
     | Literal lit' when [%equal: Literal.t] lit lit' -> true
     | _ -> false)
  | Construction (List_nil, []) ->
    (match[@warning "-4"] body with
     | Construction (List_nil, []) -> true
     | _ -> false)
  | Construction (List_cons, [ Variable head; Variable tail ]) ->
    (match[@warning "-4"] body with
     | Construction (List_cons, [ Variable head'; Variable tail' ])
       when [%equal: Variable.t * Variable.t] (head, tail) (head', tail') ->
       true
     | _ -> false)
  | Construction ((List_nil | List_cons), _) ->
    raise_s
      [%message
        "invalid number of arguments for constructor" (pattern : Pattern.t)]
;;

let variable_pairs_equal (pairs : (Variable.t * Variable.t) list) : bool =
  List.for_all pairs ~f:(fun (v, v') -> [%equal: Variable.t] v v')
;;

let remove_identity_match =
  Modified.original_for_none (fun expr ->
    match (expr : Expr.t) with
    | Match_ctl_pure { subject; pure_branch = x, Construct_pure x' }
      when case_reconstructs_parameter (x, x') -> Some subject
    | Match_ctl_pure _ -> None
    | Match_ctl
        { subject
        ; pure_branch = x, Construct_pure x'
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
      when case_reconstructs_parameter (x, x')
           && variable_pairs_equal [ m, m'; c, c'; k, k' ] -> Some subject
    | Match_ctl _ -> None
    | Match_op
        { subject
        ; normal_branch = n, Construct_op_normal (Variable n')
        ; tail_branch = t, Construct_op_tail (Variable t')
        }
      when variable_pairs_equal [ n, n'; t, t' ] -> Some subject
    | Match (subject, _, cases)
      when List.for_all cases ~f:case_reconstructs_pattern -> Some subject
    | Match _ -> None
    | Match_op _ -> None
    | Fresh_marker | Nil_evidence_vector | Variable _
    | Let (_, _, _, _)
    | Lambda _ | Fix_lambda _
    | Application (_, _, _)
    | Construction _ | Tuple_construction _ | Literal _
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
