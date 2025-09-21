open! Core
open! Import
open Evidence_passing_syntax

let map_branch_body (param, body) ~f = param, f body

let map_branch_body3 (param, param', param'', body) ~f =
  param, param', param'', f body
;;

let rec add_pure_at_tail (expr : Expr.t) : Expr.t =
  match expr with
  | Let (p, t, subject, body) -> Let (p, t, subject, add_pure_at_tail body)
  | If_then_else (cond, yes, no) ->
    If_then_else (cond, add_pure_at_tail yes, add_pure_at_tail no)
  | Match_ctl { subject; pure_branch; yield_branch } ->
    let pure_branch = map_branch_body pure_branch ~f:add_pure_at_tail in
    let yield_branch = map_branch_body3 yield_branch ~f:add_pure_at_tail in
    Match_ctl { subject; pure_branch; yield_branch }
  | Match_ctl_pure { subject; pure_branch } ->
    let pure_branch = map_branch_body pure_branch ~f:add_pure_at_tail in
    Match_ctl_pure { subject; pure_branch }
  | Match_op { subject; normal_branch; tail_branch } ->
    let normal_branch = map_branch_body normal_branch ~f:add_pure_at_tail in
    let tail_branch = map_branch_body tail_branch ~f:add_pure_at_tail in
    Match_op { subject; normal_branch; tail_branch }
  | Match (subject, scrutinee, cases) ->
    let cases =
      List.map cases ~f:(fun (pattern, body) -> pattern, add_pure_at_tail body)
    in
    Match (subject, scrutinee, cases)
  | Fresh_marker | Nil_evidence_vector | Variable _ | Lambda _ | Fix_lambda _
  | Application (_, _, _)
  | Construction (_, _)
  | Literal _
  | Operator (_, _, _)
  | Unary_operator (_, _)
  | Construct_pure _ | Construct_yield _
  | Markers_equal (_, _)
  | Effect_label _
  | Construct_op_normal _
  | Construct_op_tail _
  | Construct_handler _
  | Select_operation (_, _, _)
  | Cons_evidence_vector _
  | Lookup_evidence _
  | Get_evidence_marker _
  | Get_evidence_handler _
  | Get_evidence_handler_site_vector _
  | Impure_built_in _ -> Construct_pure expr
;;

let sink_pure : Expr.t -> Expr.t Modified.t =
  Modified.original_for_none (function [@warning "-4"]
    | Expr.Construct_pure inner ->
      let inner' = add_pure_at_tail inner in
      (match (inner' : Expr.t) with
       | Construct_pure _ -> (* failed to sink `Pure` any further *) None
       | _ -> Some inner')
    | _ -> None)
;;
