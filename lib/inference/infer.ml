open Core
open Minimal_syntax

let infer_literal : Literal.t -> Type.Primitive.t = function
  | Literal.Int _ -> Type.Primitive.Int
  | Literal.Bool _ -> Type.Primitive.Bool
  | Literal.Unit -> Type.Primitive.Unit
;;

let operand_type : Operator.t -> Type.Primitive.t = function
  | Operator.Int _ -> Type.Primitive.Int
  | Operator.Bool _ -> Type.Primitive.Bool
;;

let operator_result_type : Operator.t -> Type.Primitive.t = function
  | Operator.Bool Operator.Bool.(And | Or) -> Type.Primitive.Bool
  | Operator.Int Operator.Int.(Plus | Minus | Times | Divide | Modulo) ->
    Type.Primitive.Int
  | Operator.Int Operator.Int.(Equals | Less_than) -> Type.Primitive.Bool
;;

let unary_operand_type : Operator.Unary.t -> Type.Primitive.t = function
  | Operator.Unary.Bool Operator.Bool.Unary.Not -> Type.Primitive.Bool
;;

let unary_operator_result_type : Operator.Unary.t -> Type.Primitive.t = function
  | Operator.Unary.Bool Operator.Bool.Unary.Not -> Type.Primitive.Bool
;;

let rec infer : Context.t -> Expr.t -> (Type.Mono.t * Effect.t) Inference.t =
 fun env expr ->
  let open Inference.Let_syntax in
  match expr with
  | Expr.Literal lit ->
    Type.Mono.Primitive (infer_literal lit) |> Inference.with_any_effect
  | Expr.Variable var ->
    (match Context.find env var with
    | None ->
      let message = sprintf "unbound variable: %s" (Variable.to_string var) in
      Inference.type_error message
    | Some t ->
      (match t with
      | Type.Mono t -> Inference.with_any_effect (Type.Mono t)
      | Type.Poly s ->
        let%bind t = Inference.instantiate s in
        Inference.with_any_effect t))
  | Expr.Application (expr_f, expr_arg) ->
    let%bind t_f, eff_f = infer env expr_f in
    let%bind t_arg, eff_arg = infer env expr_arg in
    let%bind t_result = Inference.fresh_metavariable in
    let t_result = Type.Mono.Metavariable t_result in
    let%bind () =
      Inference.unify t_f (Type.Mono.Arrow (t_arg, eff_f, t_result))
    in
    let%map () = Inference.unify_effects eff_f eff_arg in
    t_result, eff_f
  | Expr.If_then_else (expr_cond, expr_yes, expr_no) ->
    let%bind t_cond, eff_cond = infer env expr_cond in
    let%bind () =
      Inference.unify t_cond (Type.Mono.Primitive Type.Primitive.Bool)
    in
    let%bind t_yes, eff_yes = infer env expr_yes in
    let%bind t_no, eff_no = infer env expr_no in
    let%bind () = Inference.unify t_yes t_no in
    (* TODO: just unifying all the effects and only touching the types must be a
       common pattern - extract it? *)
    let%bind () = Inference.unify_effects eff_cond eff_yes in
    let%bind () = Inference.unify_effects eff_yes eff_no in
    t_yes, eff_yes
  | Expr.Lambda (x, expr_body) ->
    let%bind t_x = Inference.fresh_metavariable in
    let t_x = Type.Mono.Metavariable t_x in
    let env' = Context.extend env ~var:x ~type_:(Type.Mono t_x) in
    let%map t_body, eff_body = infer env' expr_body in
    Type.Mono.Arrow (t_x, eff_body, t_body) |> Inference.with_any_effect
  (* TODO: does fix require care, or is it easy? *)
  | Expr.Fix (x, e) ->
    let%bind t_x = Inference.fresh_metavariable in
    let t_x = Type.Mono.Metavariable t_x in
    let env' = Context.extend env ~var:x ~type_:(Type.Mono t_x) in
    let%bind t_e = infer env' e in
    let%map () = Inference.unify t_x t_e in
    t_x
  | Expr.Let (x, expr_subject, expr_body) ->
    let%bind t_subject, eff_subject = infer env expr_subject in
    (* generalise, or don't if not pure TODO: is this the right thing? *)
    let%bind (t_subject : Type.t), eff_subject =
      Inference.generalise (t_subject, eff_subject) ~in_:env
    in
    let env' = Context.extend env ~var:x ~type_:t_subject in
    let%bind t_body, eff_body = infer env' expr_body in
    (* this is needed since we mustn't hide any of [expr_subject]'s effects (if
       it happens to be non total) *)
    let%map () = Inference.unify_effects eff_subject eff_body in
    t_body, eff_body
  | Expr.Operator (expr_l, op, expr_r) ->
    let%bind t_l, eff_l = infer env expr_l in
    let%bind t_r, eff_r = infer env expr_r in
    let t_operand = operand_type op |> Type.Mono.Primitive in
    let t_result = operator_result_type op |> Type.Mono.Primitive in
    let%bind () = Inference.unify t_l t_operand in
    let%bind () = Inference.unify t_r t_operand in
    let%map () = Inference.unify_effects eff_l eff_r in
    t_result, eff_l
  | Expr.Unary_operator (uop, e_arg) ->
    let%bind t_arg, eff_arg = infer env e_arg in
    let t_operand = unary_operand_type uop |> Type.Mono.Primitive in
    let t_result = unary_operator_result_type uop |> Type.Mono.Primitive in
    let%map () = Inference.unify t_operand t_arg in
    t_result, eff_arg
  | Expr.Handle (_, _) -> failwith "not implemented"
;;

let infer_type e =
  let open Result.Let_syntax in
  let%map (t, eff), substitution = Inference.run (infer Context.empty e) in
  (* TODO: convert to a type without metavariables (not possible for [Expr]s
     without generalising here, but should/will be) *)
  Substitution.apply_to_mono substitution t, Substitution.apply_to_effect eff
;;
