open Core
open Minimal_syntax

let infer_literal : Literal.t -> Type.Primitive.t = function
  | Literal.Int _ -> Type.Primitive.Int
  | Literal.Bool _ -> Type.Primitive.Bool
  | Literal.Unit -> Type.Primitive.Unit
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
      | Type.Poly s -> Inference.instantiate s)
      (* TODO: instantiation occurs within the Infer monad *))
  | Expr.Application (e_f, e_arg) ->
    let%bind t_f = infer env e_f in
    let%bind t_arg = infer env e_arg in
    let%bind t_result = Inference.fresh_metavariable in
    let t_result = Type.Mono.Metavariable t_result in
    let%map () = Inference.unify t_f (Type.Mono.Arrow (t_arg, t_result)) in
    (* TODO: definitely need to backsubstitute over the result - here we give a
       metavariable as the type *)
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
;;

let infer_type e =
  let open Result.Let_syntax in
  let%map t, substitution = Inference.run (infer Context.empty e) in
  (* TODO: is returning a [Mono.t] expected *)
  Substitution.apply_to_mono substitution t
;;
