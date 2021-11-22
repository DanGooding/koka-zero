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

let rec infer : Context.t -> Expr.t -> Type.Mono.t Inference.t =
 fun env e ->
  let open Inference.Let_syntax in
  match e with
  | Expr.Literal lit -> Type.Mono.Primitive (infer_literal lit) |> return
  | Expr.Variable var ->
    (match Context.find env var with
    | None ->
      let message = sprintf "unbound variable: %s" (Variable.to_string var) in
      Inference.type_error message
    | Some t ->
      (match t with
      | Type.Mono t -> return t
      | Type.Poly s -> Inference.instantiate s))
  | Expr.Application (e_f, e_arg) ->
    let%bind t_f = infer env e_f in
    let%bind t_arg = infer env e_arg in
    let%bind t_result = Inference.fresh_metavariable in
    let t_result = Type.Mono.Metavariable t_result in
    let%map () = Inference.unify t_f (Type.Mono.Arrow (t_arg, t_result)) in
    t_result
  | Expr.If_then_else (e_cond, e_yes, e_no) ->
    let%bind t_cond = infer env e_cond in
    let%bind () =
      Inference.unify t_cond (Type.Mono.Primitive Type.Primitive.Bool)
    in
    let%bind t_yes = infer env e_yes in
    let%bind t_no = infer env e_no in
    let%map () = Inference.unify t_yes t_no in
    t_yes
  | Expr.Lambda (x, e) ->
    let%bind t_x = Inference.fresh_metavariable in
    let t_x = Type.Mono.Metavariable t_x in
    let env' = Context.extend env ~var:x ~type_:(Type.Mono t_x) in
    let%map t_e = infer env' e in
    Type.Mono.Arrow (t_x, t_e)
  | Expr.Fix (x, e) ->
    let%bind t_x = Inference.fresh_metavariable in
    let t_x = Type.Mono.Metavariable t_x in
    let env' = Context.extend env ~var:x ~type_:(Type.Mono t_x) in
    let%bind t_e = infer env' e in
    let%map () = Inference.unify t_x t_e in
    t_x
  | Expr.Let (x, e_subject, e_body) ->
    let%bind t_subject = infer env e_subject in
    let%bind p_subject = Inference.generalise t_subject ~in_:env in
    let env' = Context.extend env ~var:x ~type_:(Type.Poly p_subject) in
    infer env' e_body
  | Expr.Operator (e_l, op, e_r) ->
    let%bind t_l = infer env e_l in
    let%bind t_r = infer env e_r in
    let t_operand = operand_type op |> Type.Mono.Primitive in
    let t_result = operator_result_type op |> Type.Mono.Primitive in
    let%bind () = Inference.unify t_l t_operand in
    let%map () = Inference.unify t_r t_operand in
    t_result
  | Expr.Unary_operator (uop, e) ->
    let%bind t_e = infer env e in
    let t_operand = unary_operand_type uop |> Type.Mono.Primitive in
    let t_result = unary_operator_result_type uop |> Type.Mono.Primitive in
    let%map () = Inference.unify t_operand t_e in
    t_result
;;

let infer_type e =
  let open Result.Let_syntax in
  let%map t, substitution = Inference.run (infer Context.empty e) in
  (* TODO: convert to a type without metavariables (not possible for [Expr]s
     without generalising here, but should/will be) *)
  Substitution.apply_to_mono substitution t
;;
