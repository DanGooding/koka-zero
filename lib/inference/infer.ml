open Core
open Minimal_syntax
open Koka_zero_util

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
  | Operator.Int
      Operator.Int.(
        ( Equals
        | Not_equal
        | Less_than
        | Less_equal
        | Greater_than
        | Greater_equal )) -> Type.Primitive.Bool
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

(* TODO: perhaps a reader monad for environment? *)
(** attempt to add a binding to the environment, giving a type error if [var] is
    not shadowable *)
let add_binding
    : env:Context.t -> var:Variable.t -> type_:Type.t -> Context.t Inference.t
  =
 fun ~env ~var ~type_ ->
  let open Inference.Let_syntax in
  match Context.extend env ~var ~type_ with
  | `Ok env' -> return env'
  | `Cannot_shadow ->
    let message = sprintf "cannot shadow '%s'" (Variable.to_string var) in
    Inference.type_error message
;;

(** infer the type and effect of an expression. These are local and may contain
    known metavariables *)
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
  | Expr.Application (expr_f, expr_args) ->
    let%bind t_f, eff_f = infer ~env ~effect_env expr_f in
    let%bind (arg_ts_effs : (Type.Mono.t * Effect.t) list) =
      List.map expr_args ~f:(fun expr_arg -> infer ~env ~effect_env expr_arg)
      |> Inference.sequence
    in
    let t_args, eff_args = List.unzip arg_ts_effs in
    let%bind t_result = Inference.fresh_metavariable in
    let t_result = Type.Mono.Metavariable t_result in
    let%bind () =
      Inference.unify t_f (Type.Mono.Arrow (t_args, eff_f, t_result))
    in
    (* unify all effects *)
    let%bind eff_args_combined = Inference.fresh_effect_metavariable in
    let eff_args_combined = Effect.Metavariable eff_args_combined in
    let%bind () =
      List.map eff_args ~f:(fun eff_arg ->
          Inference.unify_effects eff_arg eff_args_combined)
      |> Inference.sequence_units
    in
    let%map () = Inference.unify_effects eff_f eff_args_combined in
    t_result, eff_f
  | Expr.Lambda lambda ->
    let%bind t = infer_lambda ~env ~effect_env lambda in
    Inference.with_any_effect t
  | Expr.Fix_lambda fix_lambda ->
    let%bind t = infer_fix_lambda ~env ~effect_env fix_lambda in
    Inference.with_any_effect t
  | Expr.Let (x, expr_subject, expr_body) ->
    let%bind t_subject, eff_subject = infer ~env ~effect_env expr_subject in
    let%bind (t_subject : Type.Poly.t) =
      (* forces eff_subject to be total *)
      Inference.generalise t_subject eff_subject ~in_:env
    in
    let%bind env' = add_binding ~env ~var:x ~type_:(Type.Poly t_subject) in
    let%map t_body, eff_body = infer ~env:env' ~effect_env expr_body in
    t_body, eff_body
  | Expr.Seq (e1, e2) ->
    let%bind _t1, eff1 = infer ~env ~effect_env e1 in
    let%bind t2, eff2 = infer ~env ~effect_env e2 in
    let%map () = Inference.unify_effects eff1 eff2 in
    t2, eff1
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
  | Expr.Handler handler ->
    let { Expr.operations; return_clause } = handler in
    let%bind lab_handled =
      lookup_effect_label_for_handler ~effect_env handler
    in
    let%bind t_subject_meta = Inference.fresh_metavariable in
    let t_subject = Type.Mono.Metavariable t_subject_meta in
    let%bind eff_rest_meta = Inference.fresh_effect_metavariable in
    let eff_rest = Effect.Metavariable eff_rest_meta in
    let%bind t_handler_result_meta = Inference.fresh_metavariable in
    let t_handler_result = Type.Mono.Metavariable t_handler_result_meta in
    let%bind () =
      match return_clause with
      (* no `return` clause - handler result is just subject *)
      | None -> Inference.unify t_subject t_handler_result
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
    let%bind () =
      Map.mapi operations ~f:(fun ~key:name ~data:op_handler ->
          infer_operation
            ~lab_handled
            ~eff_rest
            ~env
            ~effect_env
            ~t_handler_result
            ~name
            op_handler)
      |> Inference.sequence_map_units
    in
    (* <lab_handled|eff_rest> *)
    let eff_pre_handle =
      let labels = Effect.Label.Multiset.of_list [ lab_handled ] in
      let tail = Some (Effect.Row.Tail.Metavariable eff_rest_meta) in
      Effect.Row { Effect.Row.labels; tail }
    in
    let t_action =
      (* () -> <lab_handled|eff_rest> t_subject *)
      Type.Mono.Arrow ([], eff_pre_handle, t_subject)
    in
    let t_handler =
      Type.Mono.Arrow ([ t_action ], eff_rest, t_handler_result)
    in
    Inference.with_any_effect t_handler

(** infer the type of a lambda. Since lambdas are values, they are inherently
    total (have no effect) *)
and infer_lambda
    :  Expr.lambda -> env:Context.t -> effect_env:Effect_signature.Context.t
    -> Type.Mono.t Inference.t
  =
 fun (xs, expr_body) ~env ~effect_env ->
  let open Inference.Let_syntax in
  let%bind (xs_to_ts : (Variable.t * Type.Mono.t) list) =
    List.map xs ~f:(fun x ->
        let%map t_x = Inference.fresh_metavariable in
        let t_x = Type.Mono.Metavariable t_x in
        x, t_x)
    |> Inference.sequence
  in
  let%bind () =
    match Variable.Map.of_alist xs_to_ts with
    | `Ok _ -> return ()
    | `Duplicate_key v ->
      let message = sprintf "duplicate parameter: %s" (Variable.to_string v) in
      Inference.type_error message
  in
  (* add each parameter to the environment *)
  let%bind env' =
    List.fold xs_to_ts ~init:(return env) ~f:(fun env (x, t_x) ->
        let%bind env = env in
        add_binding ~env ~var:x ~type_:(Type.Mono t_x))
  in
  let%map t_body, eff_body = infer ~env:env' ~effect_env expr_body in
  let t_xs = List.map xs_to_ts ~f:(fun (_x, t_x) -> t_x) in
  let t = Type.Mono.Arrow (t_xs, eff_body, t_body) in
  t

(** infer the type of a fix-wrapped lambda. Since these are values, they are
    inherently total *)
and infer_fix_lambda
    :  Expr.fix_lambda -> env:Context.t -> effect_env:Effect_signature.Context.t
    -> Type.Mono.t Inference.t
  =
 fun (f, lambda) ~env ~effect_env ->
  let open Inference.Let_syntax in
  let xs, _e_body = lambda in
  (* expect `e` (which can refer to itself as `f`) to have type: *)
  (* `t_f_args -> eff_f t_f_result | <>` *)
  let%bind (t_f_args : Type.Mono.t list) =
    List.map xs ~f:(fun _x ->
        let%map t_x = Inference.fresh_metavariable in
        Type.Mono.Metavariable t_x)
    |> Inference.sequence
  in
  let%bind eff_f = Inference.fresh_effect_metavariable in
  let%bind t_f_result = Inference.fresh_metavariable in
  let eff_f = Effect.Metavariable eff_f in
  let t_f_result = Type.Mono.Metavariable t_f_result in
  (* TODO: once track divergence, arrow's effect should be <div|eff_f> *)
  let t_f = Type.Mono.Arrow (t_f_args, eff_f, t_f_result) in
  let%bind env' = add_binding ~env ~var:f ~type_:(Type.Mono t_f) in
  let%bind t_e = infer_lambda ~env:env' ~effect_env lambda in
  let%map () = Inference.unify t_f t_e in
  t_f

(** Infer and check an operation handler's type *)
and infer_operation
    :  lab_handled:Effect.Label.t -> eff_rest:Effect.t -> env:Context.t
    -> effect_env:Effect_signature.Context.t -> t_handler_result:Type.Mono.t
    -> name:Variable.t -> Expr.op_handler -> unit Inference.t
  =
 fun ~lab_handled ~eff_rest ~env ~effect_env ~t_handler_result ~name op_handler ->
  (* TODO: rather than unifying to check the type here, use a different context
     containing the declarations *)
  (* TODO: using inference here is not ideal - since failure is not a type
     error, but a programmer one *)
  let open Inference.Let_syntax in
  (* lookup operation's [name] and unify with `t_argument -> <lab_handled>
     t_answer` *)
  let%bind t_op, eff_op = infer ~env ~effect_env (Expr.Variable name) in
  let%bind t_argument = Inference.fresh_metavariable in
  let t_argument = Type.Mono.Metavariable t_argument in
  let%bind t_answer = Inference.fresh_metavariable in
  let t_answer = Type.Mono.Metavariable t_answer in
  let eff_lab_handled = Effect.Row (Effect.Row.closed_singleton lab_handled) in
  let t_op_expected =
    Type.Mono.Arrow ([ t_argument ], eff_lab_handled, t_answer)
  in
  let%bind () = Inference.unify t_op t_op_expected in
  let%bind () = Inference.unify_effects eff_op Effect.total in
  let t_resume =
    Type.Mono (Type.Mono.Arrow ([ t_answer ], eff_rest, t_handler_result))
  in
  let env_with_resume =
    (* TODO: need to prevent escape of non first-class resume *)
    match Context.extend env ~var:Keyword.resume ~type_:t_resume with
    | `Ok env_with_resume -> env_with_resume
    | `Cannot_shadow ->
      raise_s [%message "`resume` must be shadowable - can nest handlers"]
  in
  infer_operation_clause
    ~eff_rest
    ~env:env_with_resume
    ~effect_env
    ~t_handler_result
    ~t_argument
    op_handler

(** Infer and check an operartion clause's body's type

    This is a helper function which applies to any unnamed body, so works for
    both operations and `return`. Notably it does not add `resume` to the
    context. *)
and infer_operation_clause
    :  eff_rest:Effect.t -> env:Context.t
    -> effect_env:Effect_signature.Context.t -> t_handler_result:Type.Mono.t
    -> t_argument:Type.Mono.t -> Expr.op_handler -> unit Inference.t
  =
 fun ~eff_rest ~env ~effect_env ~t_handler_result ~t_argument op_handler ->
  let open Inference.Let_syntax in
  let { Expr.op_argument; op_body } = op_handler in
  let%bind env' =
    add_binding ~env ~var:op_argument ~type_:(Type.Mono t_argument)
  in
  let%bind t_result, eff_result = infer ~env:env' ~effect_env op_body in
  (* unifying with [t_handler_result] rather than e.g. [t_answer] because
     [resume] is explicit - so the result of op_body is what is returned from
     the handler (perhaps via some more returns from [resume]) *)
  let%bind () = Inference.unify t_result t_handler_result in
  Inference.unify_effects eff_result eff_rest
;;

(** add an effect's operations to the context *)
let bind_operations
    : Context.t -> declaration:Decl.Effect.t -> Context.t Inference.t
  =
 fun env ~declaration ->
  let open Inference.Let_syntax in
  let { Decl.Effect.name = label; operations } = declaration in
  Map.fold operations ~init:(return env) ~f:(fun ~key:op_name ~data:op env ->
      let%bind env = env in
      let { Decl.Effect.Operation.argument; answer } = op in
      (* `forall eff_rest. argument -> <label|eff_rest> answer` *)
      let%bind eff_rest = Inference.fresh_effect_variable in
      let tail = Some (Effect.Row.Tail.Variable eff_rest) in
      let labels = Effect.Label.Multiset.of_list [ label ] in
      let eff = Effect.Row { Effect.Row.labels; tail } in
      let monotype = Type.Mono.Arrow ([ argument ], eff, answer) in
      let forall_bound = Type.Variable.Set.empty in
      let forall_bound_effects = Effect.Variable.Set.singleton eff_rest in
      let type_ =
        Type.Poly { Type.Poly.forall_bound; forall_bound_effects; monotype }
      in
      match Context.extend_unshadowable env ~var:op_name ~type_ with
      | `Ok env' -> return env'
      | `Cannot_shadow ->
        let message =
          sprintf
            "operation names must be unique: '%s' is reused"
            (Variable.to_string op_name)
        in
        Inference.type_error message)
;;

(** add an effect's signature to the effect environment *)
let bind_effect_signature
    :  Effect_signature.Context.t -> declaration:Decl.Effect.t
    -> Effect_signature.Context.t Inference.t
  =
 fun effect_env ~declaration ->
  let open Inference.Let_syntax in
  match Effect_signature.Context.extend_decl effect_env declaration with
  | `Ok effect_env -> return effect_env
  | `Duplicate ->
    let { Decl.Effect.name; _ } = declaration in
    let message =
      sprintf "effect '%s' is already defined" (Effect.Label.to_string name)
    in
    Inference.type_error message
;;

(** run inference on a function declaration, adding it (generalised) to the
    environment if well typed *)
let infer_fun_decl
    :  Decl.Fun.t -> env:Context.t -> effect_env:Effect_signature.Context.t
    -> Context.t Inference.t
  =
 fun f ~env ~effect_env ->
  let open Inference.Let_syntax in
  let f_name, _lambda = f in
  let%bind t_f = infer_fix_lambda ~env ~effect_env f in
  let%bind (t_f : Type.Poly.t) =
    Inference.generalise t_f Effect.total ~in_:env
  in
  add_binding ~env ~var:f_name ~type_:(Type.Poly t_f)
;;

(** check an effect declaration, adding its signature and operations to the
    contexts if correct *)
let infer_effect_decl
    :  Decl.Effect.t -> env:Context.t -> effect_env:Effect_signature.Context.t
    -> (Context.t * Effect_signature.Context.t) Inference.t
  =
 fun declaration ~env ~effect_env ->
  let open Inference.Let_syntax in
  let%bind env =
    (* add all operation names to the context *)
    bind_operations env ~declaration
  in
  let%map effect_env = bind_effect_signature effect_env ~declaration in
  env, effect_env
;;

let infer_decl
    :  Decl.t -> env:Context.t -> effect_env:Effect_signature.Context.t
    -> (Context.t * Effect_signature.Context.t) Inference.t
  =
 fun declaration ~env ~effect_env ->
  let open Inference.Let_syntax in
  match declaration with
  | Decl.Fun f ->
    let%map env = infer_fun_decl f ~env ~effect_env in
    env, effect_env
  | Decl.Effect e -> infer_effect_decl e ~env ~effect_env
;;

let infer_decls
    :  Decl.t list -> env:Context.t -> effect_env:Effect_signature.Context.t
    -> (Context.t * Effect_signature.Context.t) Inference.t
  =
 fun declarations ~env ~effect_env ->
  let open Inference.Let_syntax in
  (* importantly this is a left fold *)
  List.fold
    declarations
    ~init:(return (env, effect_env))
    ~f:(fun envs declaration ->
      let%bind env, effect_env = envs in
      infer_decl declaration ~env ~effect_env)
;;

let infer_expr_toplevel
    :  Expr.t -> declarations:Decl.t list
    -> (Type.Mono.t * Effect.t) Or_static_error.t
  =
 fun expr ~declarations ->
  let%map.Result (t, eff), substitution =
    Inference.run
      (let env = Context.empty in
       let effect_env = Effect_signature.Context.empty in
       let%bind.Inference env, effect_env =
         infer_decls ~env ~effect_env declarations
       in
       infer ~env ~effect_env expr)
  in
  (* convert to a type with only unknown metavariables *)
  ( Substitution.apply_to_mono substitution t
  , Substitution.apply_to_effect substitution eff )
;;

let check_program : Program.t -> unit Or_static_error.t =
 fun { Program.declarations; has_main } ->
  let env = Context.empty in
  let effect_env = Effect_signature.Context.empty in
  let declarations =
    if has_main
    then declarations @ [ Decl.Fun Program.entry_point ]
    else declarations
  in
  let%map.Result (_env, _effect_env), _substitution =
    Inference.run (infer_decls declarations ~env ~effect_env)
  in
  ()
;;
