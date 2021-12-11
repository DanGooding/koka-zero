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

let lookup_effect_label_for_handler
    :  effect_env:Effect_signature.Context.t -> Expr.handler
    -> Effect.Label.t Inference.t
  =
 fun ~effect_env handler ->
  let open Inference.Let_syntax in
  let signature = Effect_signature.t_of_handler handler in
  match Effect_signature.Context.find effect_env signature with
  | Some lab_handled -> return lab_handled
  | None ->
    (* TODO: use actual to_string to give nice output *)
    let signature_str =
      Effect_signature.sexp_of_t signature |> Sexp.to_string_hum
    in
    let message =
      sprintf "handler does not match any effect: %s" signature_str
    in
    Inference.type_error message
;;

(** attempt to add a binding to the environment, giving a type error if [var] is
    not shadowable *)
let add_binding
    : env:Context.t -> var:Variable.t -> type_:Type.t -> Context.t Inference.t
  =
 fun ~env ~var ~type_ ->
  let open Inference.Let_syntax in
  match Context.extend env ~var ~type_ with
  | Some env' -> return env'
  | None ->
    let message = sprintf "cannot shadow '%s'" (Variable.to_string var) in
    Inference.type_error message
;;

let rec infer
    :  env:Context.t -> effect_env:Effect_signature.Context.t -> Expr.t
    -> (Type.Mono.t * Effect.t) Inference.t
  =
 fun ~env ~effect_env expr ->
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
      | Type.Mono t -> Inference.with_any_effect t
      | Type.Poly s ->
        let%bind t = Inference.instantiate s in
        Inference.with_any_effect t))
  | Expr.Application (expr_f, expr_arg) ->
    let%bind t_f, eff_f = infer ~env ~effect_env expr_f in
    let%bind t_argument, eff_arg = infer ~env ~effect_env expr_arg in
    let%bind t_result = Inference.fresh_metavariable in
    let t_result = Type.Mono.Metavariable t_result in
    let%bind () =
      Inference.unify t_f (Type.Mono.Arrow (t_argument, eff_f, t_result))
    in
    let%map () = Inference.unify_effects eff_f eff_arg in
    t_result, eff_f
  | Expr.If_then_else (expr_cond, expr_yes, expr_no) ->
    let%bind t_cond, eff_cond = infer ~env ~effect_env expr_cond in
    let%bind () =
      Inference.unify t_cond (Type.Mono.Primitive Type.Primitive.Bool)
    in
    let%bind t_yes, eff_yes = infer ~env ~effect_env expr_yes in
    let%bind t_no, eff_no = infer ~env ~effect_env expr_no in
    let%bind () = Inference.unify t_yes t_no in
    (* TODO: just unifying all the effects and only touching the types must be a
       common pattern - extract it? *)
    let%bind () = Inference.unify_effects eff_cond eff_yes in
    let%map () = Inference.unify_effects eff_yes eff_no in
    t_yes, eff_yes
  | Expr.Lambda (x, expr_body) ->
    let%bind t_x = Inference.fresh_metavariable in
    let t_x = Type.Mono.Metavariable t_x in
    let%bind env' = add_binding ~env ~var:x ~type_:(Type.Mono t_x) in
    let%bind t_body, eff_body = infer ~env:env' ~effect_env expr_body in
    let t = Type.Mono.Arrow (t_x, eff_body, t_body) in
    Inference.with_any_effect t
  | Expr.Fix (x, e) ->
    let%bind t_x = Inference.fresh_metavariable in
    let t_x = Type.Mono.Metavariable t_x in
    let%bind env' = add_binding ~env ~var:x ~type_:(Type.Mono t_x) in
    let%bind t_e, eff_e = infer ~env:env' ~effect_env e in
    let%map () = Inference.unify t_x t_e in
    (* TODO: if tracking divergence: unify eff_e with <div|fresh> *)
    t_x, eff_e
  | Expr.Let (x, expr_subject, expr_body) ->
    let%bind t_subject, eff_subject = infer ~env ~effect_env expr_subject in
    (* generalise, or don't if not pure TODO: is this the right thing? *)
    let%bind (t_subject : Type.t), eff_subject =
      Inference.generalise (t_subject, eff_subject) ~in_:env
    in
    let%bind env' = add_binding ~env ~var:x ~type_:t_subject in
    let%bind t_body, eff_body = infer ~env:env' ~effect_env expr_body in
    (* this is needed since we mustn't hide any of [expr_subject]'s effects (if
       it happens to be non total) *)
    let%map () = Inference.unify_effects eff_subject eff_body in
    t_body, eff_body
  | Expr.Operator (expr_l, op, expr_r) ->
    let%bind t_l, eff_l = infer ~env ~effect_env expr_l in
    let%bind t_r, eff_r = infer ~env ~effect_env expr_r in
    let t_operand = operand_type op |> Type.Mono.Primitive in
    let t_result = operator_result_type op |> Type.Mono.Primitive in
    let%bind () = Inference.unify t_l t_operand in
    let%bind () = Inference.unify t_r t_operand in
    let%map () = Inference.unify_effects eff_l eff_r in
    t_result, eff_l
  | Expr.Unary_operator (uop, expr_arg) ->
    let%bind t_argument, eff_arg = infer ~env ~effect_env expr_arg in
    let t_operand = unary_operand_type uop |> Type.Mono.Primitive in
    let t_result = unary_operator_result_type uop |> Type.Mono.Primitive in
    let%map () = Inference.unify t_operand t_argument in
    t_result, eff_arg
  | Expr.Handle (handler, e_subject) ->
    let { Expr.operations; return_clause } = handler in
    let%bind lab_handled =
      lookup_effect_label_for_handler ~effect_env handler
    in
    let%bind t_subject, eff_subject = infer ~env ~effect_env e_subject in
    (* eff_subject ~ <lab_handled|eff_rest> *)
    let%bind eff_rest_meta = Inference.fresh_effect_metavariable in
    let effect_pre_handle =
      let labels = Effect.Label.Multiset.of_list [ lab_handled ] in
      let tail = Some (Effect.Row.Tail.Metavariable eff_rest_meta) in
      Effect.Row { Effect.Row.labels; tail }
    in
    let eff_rest = Effect.Metavariable eff_rest_meta in
    let%bind () = Inference.unify_effects eff_subject effect_pre_handle in
    let%bind t_handler_result = Inference.fresh_metavariable in
    let t_handler_result = Type.Mono.Metavariable t_handler_result in
    let%bind () =
      match return_clause with
      | None -> return ()
      | Some return_clause ->
        (* `return` clause has type `t_subject -> t_handler_result` *)
        infer_operation_clause
          ~eff_rest
          ~env
          ~effect_env
          ~t_handler_result
          ~t_argument:t_subject
          return_clause
    in
    (* check each op body has the right type *)
    let%map () =
      Map.mapi operations ~f:(fun ~key:name ~data:handler ->
          infer_operation
            ~lab_handled
            ~eff_rest
            ~env
            ~effect_env
            ~t_handler_result
            ~name
            handler)
      |> Inference.sequence_map_units
    in
    t_handler_result, eff_rest

(** Infer and check an operation handler's type *)
and infer_operation
    :  lab_handled:Effect.Label.t -> eff_rest:Effect.t -> env:Context.t
    -> effect_env:Effect_signature.Context.t -> t_handler_result:Type.Mono.t
    -> name:Variable.t -> Expr.op_handler -> unit Inference.t
  =
 fun ~lab_handled ~eff_rest ~env ~effect_env ~t_handler_result ~name op ->
  let { Expr.op_argument; _ } = op in
  let t_argument, t_result =
    (* TODO: this is horrible code - plus it will erroneously fail due to
       shadowing *)
    match Context.find_exn env name with
    (* operations must be delcared to have a single effect (the one they belong
       to) *)
    | Type.Mono
        (Type.Mono.Arrow
          ( t_argument
          , Effect.Row { Effect.Row.labels = [ label ]; tail = None }
          , t_result )) ->
      if Effect.Label.(lab_handled = label)
      then t_argument, t_result
      else assert false
    | _ -> assert false
  in
  let t_resume =
    Type.Mono (Type.Mono.Arrow (t_result, eff_rest, t_handler_result))
  in
  let env_with_resume =
    (* TODO: need to prevent escape of non first-class resume *)
    match Context.extend env ~var:Keyword.resume ~type_:t_resume with
    | Some env -> env
    | None ->
      raise_s [%message "`resume` must be shadowable - can nest handlers"]
  in
  infer_operation_clause
    ~eff_rest
    ~env:env_with_resume
    ~effect_env
    ~t_handler_result
    ~t_argument
    op

(** Infer and check an operartion clause's body's type

    This is a helper function which applies to any unnamed body, so works for
    both operations and `return`. Notably it does not add `resume` to the
    context. *)
and infer_operation_clause
    :  eff_rest:Effect.t -> env:Context.t
    -> effect_env:Effect_signature.Context.t -> t_handler_result:Type.Mono.t
    -> t_argument:Type.Mono.t -> Expr.op_handler -> unit Inference.t
  =
 fun ~eff_rest ~env ~effect_env ~t_handler_result ~t_argument op ->
  let open Inference.Let_syntax in
  let { Expr.op_argument; op_body } = op in
  let env' =
    Context.extend env ~var:op_argument ~type_:(Type.Mono t_argument)
  in
  let%bind t_result, eff_result = infer ~env:env' ~effect_env op_body in
  let%bind () = Inference.unify t_result t_handler_result in
  Inference.unify_effects eff_result eff_rest
;;

let infer_program : Program.t -> (Type.Mono.t * Effect.t) Inference.t =
 fun { Program.effect_declarations; body } ->
  let open Inference.Let_syntax in
  let%bind env =
    (* add all operation names to the context *)
    List.fold
      effect_declarations
      ~init:(return Context.empty)
      ~f:(fun env declaration ->
        let { Effect_decl.name = label; operations } = declaration in
        Map.fold operations ~init:env ~f:(fun ~key:op_name ~data:op env ->
            let%bind env = env in
            let { Effect_decl.Operation.argument; result } = op in
            let eff = Effect.closed_singleton label in
            let type_ = Type.Mono (Type.Mono.Arrow (argument, eff, result)) in
            match Context.extend env ~shadowable:false ~var:op_name ~type_ with
            | Some env' -> return env'
            | None ->
              let message =
                sprintf
                  "operation names must be unique: '%s' is reused"
                  (Variable.to_string op_name)
              in
              Inference.type_error message))
  in
  let effect_env =
    List.fold
      effect_declarations
      ~init:Effect_signature.Context.empty
      ~f:Effect_signature.Context.extend_decl
  in
  infer ~env ~effect_env body
;;

let infer_type program =
  let%map.Result (t, eff), substitution =
    Inference.run (infer_program program)
  in
  (* TODO: convert to a type without metavariables (not possible for [Expr]s
     without generalising here, but should/will be) *)
  ( Substitution.apply_to_mono substitution t
  , Substitution.apply_to_effect substitution eff )
;;
