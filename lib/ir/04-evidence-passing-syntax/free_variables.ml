open Core
open Import
open Evidence_passing_syntax

let rec free_in_expr : Expr.t -> Variable.Set.t = function
  | Expr.Variable v -> Variable.Set.singleton v
  | Expr.Let (p, _type, subject, body) ->
    let subject_free = free_in_expr subject in
    let body_free = free_in_bindings_set (Parameter.bound_variables p) body in
    Set.union subject_free body_free
  | Expr.Lambda lambda -> free_in_lambda lambda
  | Expr.Fix_lambda fix_lambda -> free_in_fix_lambda fix_lambda
  | Expr.Application (e_f, e_args, _ty) ->
    free_in_exprs (e_f :: List.map e_args ~f:(fun (param, _type) -> param))
  | Expr.Construction (_, args) -> free_in_exprs args
  | Expr.Literal _ -> Variable.Set.empty
  | Expr.If_then_else (e_cond, e_yes, e_no) ->
    free_in_exprs [ e_cond; e_yes; e_no ]
  | Expr.Match (subject, _, cases) ->
    let subject_free = free_in_expr subject in
    let case_free =
      List.map cases ~f:(fun (pattern, body) ->
        let pattern_bound = Pattern.bound_variables pattern in
        free_in_bindings_set pattern_bound body)
    in
    Variable.Set.union_list (subject_free :: case_free)
  | Expr.Operator (e_left, _, e_right) -> free_in_exprs [ e_left; e_right ]
  | Expr.Unary_operator (_, e) -> free_in_expr e
  | Expr.Construct_pure e -> free_in_expr e
  | Expr.Construct_yield { marker; op_clause; resumption } ->
    free_in_exprs [ marker; op_clause; resumption ]
  | Expr.Match_ctl { subject; pure_branch; yield_branch } ->
    let subject_free = free_in_expr subject in
    let pure_free =
      let x, pure_body = pure_branch in
      free_in_binding x pure_body
    in
    let yield_free =
      let marker, op_clause, resumption, yield_body = yield_branch in
      free_in_bindings [ marker; op_clause; resumption ] yield_body
    in
    Variable.Set.union_list [ subject_free; pure_free; yield_free ]
  | Expr.Match_ctl_pure { subject; pure_branch } ->
    let subject_free = free_in_expr subject in
    let pure_free =
      let x, pure_body = pure_branch in
      free_in_binding x pure_body
    in
    Variable.Set.union_list [ subject_free; pure_free ]
  | Expr.Fresh_marker | Expr.Markers_equal (_, _) -> Variable.Set.empty
  | Expr.Effect_label _ -> Variable.Set.empty
  | Expr.Construct_op_normal e -> free_in_expr e
  | Expr.Construct_op_tail e -> free_in_expr e
  | Expr.Match_op { subject; normal_branch; tail_branch } ->
    let subject_free = free_in_expr subject in
    let normal_free =
      let x, normal_body = normal_branch in
      free_in_binding x normal_body
    in
    let tail_free =
      let x, tail_body = tail_branch in
      free_in_binding x tail_body
    in
    Variable.Set.union_list [ subject_free; normal_free; tail_free ]
  | Expr.Construct_handler { handled_effect = _; operation_clauses } ->
    Map.data operation_clauses |> free_in_exprs
  | Expr.Select_operation (_label, _op_name, e) -> free_in_expr e
  | Expr.Nil_evidence_vector -> Variable.Set.empty
  | Expr.Cons_evidence_vector
      { label; marker; handler; handler_site_vector; vector_tail } ->
    free_in_exprs [ label; marker; handler; handler_site_vector; vector_tail ]
  | Expr.Lookup_evidence { label; vector } -> free_in_exprs [ label; vector ]
  | Expr.Get_evidence_marker e -> free_in_expr e
  | Expr.Get_evidence_handler e -> free_in_expr e
  | Expr.Get_evidence_handler_site_vector e -> free_in_expr e
  | Expr.Impure_built_in impure -> free_in_impure_built_in impure

and free_in_exprs : Expr.t list -> Variable.Set.t =
  fun es -> List.map es ~f:free_in_expr |> Variable.Set.union_list

(** [free_in_bindings vs e] gives the free varaibles of [e] which aren't in [vs]
*)
and free_in_bindings : Variable.t list -> Expr.t -> Variable.Set.t =
  fun vs e -> free_in_bindings_set (Variable.Set.of_list vs) e

and free_in_bindings_set vs e =
  let e_free = free_in_expr e in
  Set.diff e_free vs

(** [free_in_binding v e] gives the free varaibles of [e] except for [v] *)
and free_in_binding : Variable.t -> Expr.t -> Variable.Set.t =
  fun v e ->
  let e_free = free_in_expr e in
  Set.remove e_free v

and free_in_lambda : Expr.lambda -> Variable.Set.t =
  fun (params, _ty, body) ->
  let bindings =
    List.map params ~f:(fun (param, _type) -> Parameter.bound_variables param)
    |> Variable.Set.union_list
  in
  free_in_bindings_set bindings body

and free_in_fix_lambda : Expr.fix_lambda -> Variable.Set.t =
  fun (name, lambda) ->
  let lambda_free = free_in_lambda lambda in
  Set.remove lambda_free name

and free_in_impure_built_in : Expr.impure_built_in -> Variable.Set.t = function
  | Expr.Impure_println -> Variable.Set.empty
  | Expr.Impure_print_int { value; newline = _ } -> free_in_expr value
  | Expr.Impure_read_int -> Variable.Set.empty
;;
