open Core
open Import
open Koka_zero_evidence_translation.Evidence_passing_syntax

let runtime_error message = Result.Error { Runtime_error.message }

let type_error ~(expected : string) actual =
  let message =
    sprintf
      "type error: expected %s, got: %s"
      expected
      (Value.sexp_of_t actual |> Sexp.to_string_hum)
  in
  runtime_error message
;;

let rec eval_expr : Expr.t -> env:Value.context -> Value.t Or_runtime_error.t =
 fun expr ~env ->
  let open Result.Let_syntax in
  match expr with
  | Expr.Variable name ->
    (match Variable.Map.find env name with
    | Some v -> return v
    | None ->
      let message =
        sprintf "unbound variable %s" (Variable.to_string_user name)
      in
      runtime_error message)
  | Expr.Lambda lambda ->
    let%map closure = eval_lambda lambda ~env in
    Value.Closure closure
  | Expr.Fix_lambda fix_lambda ->
    let%map closure = eval_fix_lambda fix_lambda ~env in
    Value.Closure closure
  | Expr.Application (e_f, e_args) ->
    let%bind closure =
      match%bind eval_expr e_f ~env with
      | Value.Closure closure -> return closure
      | e -> type_error e ~expected:"closure"
      [@@warning "-4"]
    in
    let%bind v_args = List.map e_args ~f:(eval_expr ~env) |> Result.all in
    let f, f_env = closure in
    let lambda, f_env =
      match f with
      | Value.Lambda lambda -> lambda, f_env
      | Value.Fix_lambda (f_name, lambda) ->
        lambda, Map.set f_env ~key:f_name ~data:(Value.Closure closure)
    in
    let xs, e_body = lambda in
    let%bind xs_to_args =
      match List.zip xs v_args with
      | List.Or_unequal_lengths.Ok xs_to_args -> return xs_to_args
      | List.Or_unequal_lengths.Unequal_lengths ->
        let message =
          sprintf
            "wrong number of arguments for function: got %d, expecting %d"
            (List.length v_args)
            (List.length xs)
        in
        runtime_error message
    in
    let f_body_env =
      List.fold xs_to_args ~init:f_env ~f:(fun f_body_env (x, v) ->
          Map.set f_body_env ~key:x ~data:v)
    in
    eval_expr e_body ~env:f_body_env
  | Expr.Literal lit -> eval_literal lit |> Value.Primitive |> return
  | Expr.If_then_else (e_cond, e_yes, e_no) ->
    let%bind cond =
      match%bind eval_expr e_cond ~env with
      | Value.Primitive (Value.Bool b) -> return b
      | e -> type_error ~expected:"bool" e
      [@@warning "-4"]
    in
    let e_body = if cond then e_yes else e_no in
    eval_expr e_body ~env
  | _ -> failwith "not implemented"
 [@@warning "-4"]

and eval_lambda
    : Expr.lambda -> env:Value.context -> Value.closure Or_runtime_error.t
  =
 fun lambda ~env ->
  let open Result.Let_syntax in
  (Value.Lambda lambda, env) |> return

and eval_fix_lambda
    : Expr.fix_lambda -> env:Value.context -> Value.closure Or_runtime_error.t
  =
 fun fix_lambda ~env ->
  let open Result.Let_syntax in
  (Value.Fix_lambda fix_lambda, env) |> return

and eval_literal : Literal.t -> Value.primitive = function
  | Literal.Int i -> Value.Int i
  | Literal.Bool b -> Value.Bool b
  | Literal.Unit -> Value.Unit
;;
