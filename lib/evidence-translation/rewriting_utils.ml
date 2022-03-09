open Core
open Evidence_passing_syntax

(** apply the given rewrite at every node. This performs one iteration of non
    overlapping rewrites, so should be repeated until no change is observed *)
let rec apply_everywhere
    : rewrite:(Expr.t -> Expr.t Modified.t) -> Expr.t -> Expr.t Modified.t
  =
 fun ~rewrite e ->
  match Modified.inspect (rewrite e) with
  | `Modified e' -> Modified.modified e'
  | `Unchanged ->
    (* no change at [e], try a subtree *)
    apply_everywhere_below ~rewrite e

(** apply rewrite everywhere in the given expression except the root *)
and apply_everywhere_below
    : rewrite:(Expr.t -> Expr.t Modified.t) -> Expr.t -> Expr.t Modified.t
  =
 fun ~rewrite e ->
  let open Modified.Let_syntax in
  match e with
  | Expr.Variable _ ->
    Modified.original e (* TODO: have to be careful to not use wrong [e]*)
  | Expr.Let (v, subject, body) ->
    let%bind subject = apply_everywhere ~rewrite subject in
    let%map body = apply_everywhere ~rewrite body in
    Expr.Let (v, subject, body)
  | Expr.Lambda lambda ->
    let%map lambda = apply_everywhere_to_lambda ~rewrite lambda in
    Expr.Lambda lambda
  | Expr.Fix_lambda fix_lambda ->
    let%map fix_lambda = apply_everywhere_to_fix_lambda ~rewrite fix_lambda in
    Expr.Fix_lambda fix_lambda
  | Expr.Application (f, args) ->
    let%bind f = apply_everywhere ~rewrite f in
    let%map args =
      List.map args ~f:(apply_everywhere ~rewrite) |> Modified.all
    in
    Expr.Application (f, args)
  | Expr.Literal _ -> Modified.original e
  | Expr.If_then_else (cond, yes, no) ->
    let%bind cond = apply_everywhere ~rewrite cond in
    let%bind yes = apply_everywhere ~rewrite yes in
    let%map no = apply_everywhere ~rewrite no in
    Expr.If_then_else (cond, yes, no)
  | Expr.Operator (left, op, right) ->
    let%bind left = apply_everywhere ~rewrite left in
    let%map right = apply_everywhere ~rewrite right in
    Expr.Operator (left, op, right)
  | Expr.Unary_operator (op, e) ->
    let%map e = apply_everywhere ~rewrite e in
    Expr.Unary_operator (op, e)
  | Expr.Construct_pure e ->
    let%map e = apply_everywhere ~rewrite e in
    Expr.Construct_pure e
  | Expr.Construct_yield { marker; op_clause; resumption } ->
    let%bind marker = apply_everywhere ~rewrite marker in
    let%bind op_clause = apply_everywhere ~rewrite op_clause in
    let%map resumption = apply_everywhere ~rewrite resumption in
    Expr.Construct_yield { marker; op_clause; resumption }
  | Expr.Match_ctl { subject; pure_branch; yield_branch } ->
    let%bind subject = apply_everywhere ~rewrite subject in
    let x, pure_body = pure_branch in
    let%bind pure_body = apply_everywhere ~rewrite pure_body in
    let pure_branch = x, pure_body in
    let marker, op_clause, resumption, yield_body = yield_branch in
    let%map yield_body = apply_everywhere ~rewrite yield_body in
    let yield_branch = marker, op_clause, resumption, yield_body in
    Expr.Match_ctl { subject; pure_branch; yield_branch }
  | Expr.Fresh_marker -> Modified.original e
  | Expr.Markers_equal _ -> Modified.original e
  | Expr.Effect_label _ -> Modified.original e
  | Expr.Construct_op_normal e ->
    let%map e = apply_everywhere ~rewrite e in
    Expr.Construct_op_normal e
  | Expr.Construct_op_tail e ->
    let%map e = apply_everywhere ~rewrite e in
    Expr.Construct_op_tail e
  | Expr.Match_op { subject; normal_branch; tail_branch } ->
    let%bind subject = apply_everywhere ~rewrite subject in
    let x, normal_body = normal_branch in
    let%bind normal_body = apply_everywhere ~rewrite normal_body in
    let normal_branch = x, normal_body in
    let x, tail_body = tail_branch in
    let%map tail_body = apply_everywhere ~rewrite tail_body in
    let tail_branch = x, tail_body in
    Expr.Match_op { subject; normal_branch; tail_branch }
  | Expr.Construct_handler { handled_effect; operation_clauses } ->
    let%map operation_clauses =
      Map.map operation_clauses ~f:(apply_everywhere ~rewrite)
      |> Modified.all_map
    in
    Expr.Construct_handler { handled_effect; operation_clauses }
  | Expr.Select_operation (label, name, e) ->
    let%map e = apply_everywhere ~rewrite e in
    Expr.Select_operation (label, name, e)
  | Expr.Nil_evidence_vector -> Modified.original e
  | Expr.Cons_evidence_vector
      { label; marker; handler; handler_site_vector; vector_tail } ->
    let%bind label = apply_everywhere ~rewrite label in
    let%bind marker = apply_everywhere ~rewrite marker in
    let%bind handler = apply_everywhere ~rewrite handler in
    let%bind handler_site_vector =
      apply_everywhere ~rewrite handler_site_vector
    in
    let%map vector_tail = apply_everywhere ~rewrite vector_tail in
    Expr.Cons_evidence_vector
      { label; marker; handler; handler_site_vector; vector_tail }
  | Expr.Lookup_evidence { label; vector } ->
    let%bind label = apply_everywhere ~rewrite label in
    let%map vector = apply_everywhere ~rewrite vector in
    Expr.Lookup_evidence { label; vector }
  | Expr.Get_evidence_marker e ->
    let%map e = apply_everywhere ~rewrite e in
    Expr.Get_evidence_marker e
  | Expr.Get_evidence_handler e ->
    let%map e = apply_everywhere ~rewrite e in
    Expr.Get_evidence_handler e
  | Expr.Get_evidence_handler_site_vector e ->
    let%map e = apply_everywhere ~rewrite e in
    Expr.Get_evidence_handler_site_vector e
  | Expr.Impure_built_in impure ->
    let%map impure = apply_everywhere_to_impure_built_in ~rewrite impure in
    Expr.Impure_built_in impure

and apply_everywhere_to_impure_built_in
    :  rewrite:(Expr.t -> Expr.t Modified.t) -> Expr.impure_built_in
    -> Expr.impure_built_in Modified.t
  =
 fun ~rewrite impure ->
  let open Modified.Let_syntax in
  match impure with
  | Expr.Impure_print_int e ->
    let%map e = apply_everywhere ~rewrite e in
    Expr.Impure_print_int e
  | Expr.Impure_read_int -> Modified.original impure

and apply_everywhere_to_lambda
    :  rewrite:(Expr.t -> Expr.t Modified.t) -> Expr.lambda
    -> Expr.lambda Modified.t
  =
 fun ~rewrite (ps, body) ->
  let open Modified.Let_syntax in
  let%map body = apply_everywhere ~rewrite body in
  ps, body

and apply_everywhere_to_fix_lambda
    :  rewrite:(Expr.t -> Expr.t Modified.t) -> Expr.fix_lambda
    -> Expr.fix_lambda Modified.t
  =
 fun ~rewrite (f, lambda) ->
  let open Modified.Let_syntax in
  let%map lambda = apply_everywhere_to_lambda ~rewrite lambda in
  f, lambda
;;

let apply_everywhere_to_program
    : rewrite:(Expr.t -> Expr.t Modified.t) -> Program.t -> Program.t Modified.t
  =
 fun ~rewrite { Program.effect_declarations; fun_declarations } ->
  let open Modified.Let_syntax in
  let%map fun_declarations =
    List.map fun_declarations ~f:(apply_everywhere_to_fix_lambda ~rewrite)
    |> Modified.all
  in
  { Program.effect_declarations; fun_declarations }
;;
