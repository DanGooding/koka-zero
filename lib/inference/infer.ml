open Core
open Koka_zero_util
module Min = Minimal_syntax
module Expl = Explicit_syntax

let infer_literal : Min.Literal.t -> Type.Primitive.t * Expl.Literal.t
  = function
  | Min.Literal.Int i -> Type.Primitive.Int, Expl.Literal.Int i
  | Min.Literal.Bool b -> Type.Primitive.Bool, Expl.Literal.Bool b
  | Min.Literal.Unit -> Type.Primitive.Unit, Expl.Literal.Unit
;;

let operand_type : Min.Operator.t -> Type.Primitive.t = function
  | Min.Operator.Int _ -> Type.Primitive.Int
  | Min.Operator.Bool _ -> Type.Primitive.Bool
;;

let operator_result_type : Min.Operator.t -> Type.Primitive.t = function
  | Min.Operator.Bool Min.Operator.Bool.(And | Or) -> Type.Primitive.Bool
  | Min.Operator.Int Min.Operator.Int.(Plus | Minus | Times | Divide | Modulo)
    -> Type.Primitive.Int
  | Min.Operator.Int
      Min.Operator.Int.(
        ( Equals
        | Not_equal
        | Less_than
        | Less_equal
        | Greater_than
        | Greater_equal )) -> Type.Primitive.Bool
;;

let convert_operator : Min.Operator.t -> Expl.Operator.t = fun op -> op

let unary_operand_type : Min.Operator.Unary.t -> Type.Primitive.t = function
  | Min.Operator.Unary.Bool Min.Operator.Bool.Unary.Not -> Type.Primitive.Bool
;;

let unary_operator_result_type : Min.Operator.Unary.t -> Type.Primitive.t
  = function
  | Min.Operator.Unary.Bool Min.Operator.Bool.Unary.Not -> Type.Primitive.Bool
;;

let convert_unary_operator : Min.Operator.Unary.t -> Expl.Operator.Unary.t =
 fun op -> op
;;

let lookup_effect_label_for_handler
    :  effect_env:Effect_signature.Context.t -> Min.Expr.handler
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
    let message = sprintf "cannot shadow '%s'" (Variable.to_string_user var) in
    Inference.type_error message
;;

(** infer the type and effect of an expression, and convert it to explicit form.
    These are local and may contain known metavariables *)
let rec infer
    :  Min.Expr.t -> env:Context.t -> effect_env:Effect_signature.Context.t
    -> (Type.Mono.t * Effect.t * Expl.Expr.t) Inference.t
  =
 fun expr ~env ~effect_env ->
  let open Inference.Let_syntax in
  match expr with
  | Min.Expr.Value v ->
    let%bind t, v' = infer_value ~env ~effect_env v in
    let%map eff = Inference.fresh_effect_metavariable in
    let eff = Effect.Metavariable eff in
    t, eff, Expl.Expr.Value v'
  | Min.Expr.Application (expr_f, expr_args) ->
    let%bind t_f, eff_f, expr_f' = infer ~env ~effect_env expr_f in
    let%bind (arg_ts_effs : (Type.Mono.t * Effect.t * Expl.Expr.t) list) =
      List.map expr_args ~f:(infer ~env ~effect_env) |> Inference.all
    in
    let t_args, eff_args, expr_args' = List.unzip3 arg_ts_effs in
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
      |> Inference.all_unit
    in
    let%map () = Inference.unify_effects eff_f eff_args_combined in
    t_result, eff_f, Expl.Expr.Application (expr_f', expr_args')
  | Min.Expr.Let (x, val_subject, expr_body) ->
    let%bind t_subject, val_subject' =
      infer_value ~env ~effect_env val_subject
    in
    let%bind (t_subject : Type.Poly.t) =
      Inference.generalise_total t_subject ~env
    in
    let%bind env' = add_binding ~env ~var:x ~type_:(Type.Poly t_subject) in
    let%map t_body, eff_body, expr_body' =
      infer ~env:env' ~effect_env expr_body
    in
    t_body, eff_body, Expl.Expr.Let (x, val_subject', expr_body')
  | Min.Expr.Seq (expr1, expr2) ->
    let%bind _t1, eff1, expr1' = infer ~env ~effect_env expr1 in
    let%bind t2, eff2, expr2' = infer ~env ~effect_env expr2 in
    let%map () = Inference.unify_effects eff1 eff2 in
    t2, eff1, Expl.Expr.Seq (expr1', expr2')
  | Min.Expr.If_then_else (expr_cond, expr_yes, expr_no) ->
    let%bind t_cond, eff_cond, expr_cond' = infer ~env ~effect_env expr_cond in
    let%bind () =
      Inference.unify t_cond (Type.Mono.Primitive Type.Primitive.Bool)
    in
    let%bind t_yes, eff_yes, expr_yes' = infer ~env ~effect_env expr_yes in
    let%bind t_no, eff_no, expr_no' = infer ~env ~effect_env expr_no in
    let%bind () = Inference.unify t_yes t_no in
    let%bind () = Inference.unify_effects eff_cond eff_yes in
    let%map () = Inference.unify_effects eff_yes eff_no in
    t_yes, eff_yes, Expl.Expr.If_then_else (expr_cond', expr_yes', expr_no')
  | Min.Expr.Operator (expr_l, op, expr_r) ->
    let%bind t_l, eff_l, expr_l' = infer ~env ~effect_env expr_l in
    let%bind t_r, eff_r, expr_r' = infer ~env ~effect_env expr_r in
    let op' = convert_operator op in
    let t_operand = operand_type op |> Type.Mono.Primitive in
    let t_result = operator_result_type op |> Type.Mono.Primitive in
    let%bind () = Inference.unify t_l t_operand in
    let%bind () = Inference.unify t_r t_operand in
    let%map () = Inference.unify_effects eff_l eff_r in
    t_result, eff_l, Expl.Expr.Operator (expr_l', op', expr_r')
  | Min.Expr.Unary_operator (uop, expr_arg) ->
    let%bind t_argument, eff_arg, expr_arg' = infer ~env ~effect_env expr_arg in
    let uop' = convert_unary_operator uop in
    let t_operand = unary_operand_type uop |> Type.Mono.Primitive in
    let t_result = unary_operator_result_type uop |> Type.Mono.Primitive in
    let%map () = Inference.unify t_operand t_argument in
    t_result, eff_arg, Expl.Expr.Unary_operator (uop', expr_arg')
  | Min.Expr.Impure_built_in impure ->
    let%map t, eff, impure' = infer_impure_built_in ~env ~effect_env impure in
    t, eff, Expl.Expr.Impure_built_in impure'

(** infer the type of a value - values don't reduce, so can't have any effects *)
and infer_value
    :  Min.Expr.value -> env:Context.t -> effect_env:Effect_signature.Context.t
    -> (Type.Mono.t * Expl.Expr.value) Inference.t
  =
 fun v ~env ~effect_env ->
  let open Inference.Let_syntax in
  match v with
  | Min.Expr.Literal lit ->
    let t, lit' = infer_literal lit in
    return (Type.Mono.Primitive t, Expl.Expr.Literal lit')
  | Min.Expr.Variable var ->
    (match Context.find env var with
    | None ->
      let message =
        sprintf "unbound variable: %s" (Variable.to_string_user var)
      in
      Inference.type_error message
    | Some (Context.Binding.Value t) ->
      let%map t = Inference.instantiate_type t in
      t, Expl.Expr.Variable var
    | Some (Context.Binding.Operation (performed_effect, t)) ->
      let%map t = Inference.instantiate_type t in
      let perform = { Expl.Expr.operation = var; performed_effect } in
      t, Expl.Expr.Perform perform)
  | Min.Expr.Lambda lambda ->
    let%map t, lambda' = infer_lambda ~env ~effect_env lambda in
    t, Expl.Expr.Lambda lambda'
  | Min.Expr.Fix_lambda fix_lambda ->
    let%map t, fix_lambda' = infer_fix_lambda ~env ~effect_env fix_lambda in
    t, Expl.Expr.Fix_lambda fix_lambda'
  | Min.Expr.Handler handler ->
    let%map t, handler' = infer_handler ~env ~effect_env handler in
    t, Expl.Expr.Handler handler'

(** infer the type of a lambda. Since lambdas are values, they are inherently
    total (have no effect) *)
and infer_lambda
    :  Min.Expr.lambda -> env:Context.t -> effect_env:Effect_signature.Context.t
    -> (Type.Mono.t * Expl.Expr.lambda) Inference.t
  =
 fun (xs, expr_body) ~env ~effect_env ->
  let open Inference.Let_syntax in
  let%bind (xs_to_ts : (Variable.t * Type.Mono.t) list) =
    List.map xs ~f:(fun x ->
        let%map t_x = Inference.fresh_metavariable in
        let t_x = Type.Mono.Metavariable t_x in
        x, t_x)
    |> Inference.all
  in
  let%bind () =
    match Variable.Map.of_alist xs_to_ts with
    | `Ok _ -> return ()
    | `Duplicate_key v ->
      let message =
        sprintf "duplicate parameter: %s" (Variable.to_string_user v)
      in
      Inference.type_error message
  in
  (* add each parameter to the environment *)
  let%bind env' =
    Inference.list_fold xs_to_ts ~init:env ~f:(fun env (x, t_x) ->
        add_binding ~env ~var:x ~type_:(Type.Mono t_x))
  in
  let%map t_body, eff_body, expr_body' =
    infer ~env:env' ~effect_env expr_body
  in
  let t_xs = List.map xs_to_ts ~f:(fun (_x, t_x) -> t_x) in
  let t = Type.Mono.Arrow (t_xs, eff_body, t_body) in
  t, (xs, expr_body')

(** infer the type of a fix-wrapped lambda. Since these are values, they are
    inherently total *)
and infer_fix_lambda
    :  Min.Expr.fix_lambda -> env:Context.t
    -> effect_env:Effect_signature.Context.t
    -> (Type.Mono.t * Expl.Expr.fix_lambda) Inference.t
  =
 fun (f, lambda) ~env ~effect_env ->
  let open Inference.Let_syntax in
  let xs, _e_body = lambda in
  let%bind () =
    if List.mem xs f ~equal:Variable.equal
    then (
      let message =
        sprintf
          "recursive function's name is shadowed by own parameter %s"
          (Variable.to_string_user f)
      in
      Inference.type_error message)
    else return ()
  in
  (* expect `e` (which can refer to itself as `f`) to have type: *)
  (* `t_f_args -> eff_f t_f_result | <>` *)
  let%bind (t_f_args : Type.Mono.t list) =
    List.map xs ~f:(fun _x ->
        let%map t_x = Inference.fresh_metavariable in
        Type.Mono.Metavariable t_x)
    |> Inference.all
  in
  let%bind eff_f = Inference.fresh_effect_metavariable in
  let%bind t_f_result = Inference.fresh_metavariable in
  let eff_f = Effect.Metavariable eff_f in
  let t_f_result = Type.Mono.Metavariable t_f_result in
  (* TODO: once track divergence, arrow's effect should be <div|eff_f> *)
  let t_f = Type.Mono.Arrow (t_f_args, eff_f, t_f_result) in
  let%bind env' = add_binding ~env ~var:f ~type_:(Type.Mono t_f) in
  let%bind t_e, lambda' = infer_lambda ~env:env' ~effect_env lambda in
  let%map () = Inference.unify t_f t_e in
  t_f, (f, lambda')

and infer_impure_built_in
    :  Min.Expr.impure_built_in -> env:Context.t
    -> effect_env:Effect_signature.Context.t
    -> (Type.Mono.t * Effect.t * Expl.Expr.impure_built_in) Inference.t
  =
 fun impure ~env ~effect_env ->
  let open Inference.Let_syntax in
  match impure with
  | Min.Expr.Impure_print_int expr_arg ->
    let%bind t_arg, eff_arg, expr_arg' = infer ~env ~effect_env expr_arg in
    let%map () =
      Inference.unify t_arg (Type.Mono.Primitive Type.Primitive.Int)
    in
    let t_result = Type.Mono.Primitive Type.Primitive.Unit in
    t_result, eff_arg, Expl.Expr.Impure_print_int expr_arg'
  | Min.Expr.Impure_read_int ->
    let t_result = Type.Mono.Primitive Type.Primitive.Int in
    let%map eff = Inference.fresh_effect_metavariable in
    let eff = Effect.Metavariable eff in
    t_result, eff, Expl.Expr.Impure_read_int

and infer_handler
    :  Min.Expr.handler -> env:Context.t
    -> effect_env:Effect_signature.Context.t
    -> (Type.Mono.t * Expl.Expr.handler) Inference.t
  =
 fun handler ~env ~effect_env ->
  let open Inference.Let_syntax in
  let { Min.Expr.operations; return_clause } = handler in
  let%bind lab_handled = lookup_effect_label_for_handler ~effect_env handler in
  let%bind t_subject_meta = Inference.fresh_metavariable in
  let t_subject = Type.Mono.Metavariable t_subject_meta in
  let%bind eff_rest_meta = Inference.fresh_effect_metavariable in
  let eff_rest = Effect.Metavariable eff_rest_meta in
  let%bind t_handler_result_meta = Inference.fresh_metavariable in
  let t_handler_result = Type.Mono.Metavariable t_handler_result_meta in
  let%bind return_clause' =
    match return_clause with
    (* no `return` clause - handler result is just subject *)
    | None ->
      let%map () = Inference.unify t_subject t_handler_result in
      None
    | Some return_clause ->
      (* `return` clause has type `t_subject -> t_handler_result` *)
      let%map return_clause' =
        infer_operation_clause
          ~eff_rest
          ~env
          ~effect_env
          ~t_handler_result
          ~t_argument:t_subject
          return_clause
      in
      Some return_clause'
  in
  (* check each op body has the right type *)
  let%map (operations' : Expl.Expr.op_handler Variable.Map.t) =
    Map.mapi operations ~f:(fun ~key:name ~data:op_handler ->
        infer_operation
          ~lab_handled
          ~eff_rest
          ~env
          ~effect_env
          ~t_handler_result
          ~name
          op_handler)
    |> Inference.all_map
  in
  (* <lab_handled|eff_rest> *)
  let eff_pre_handle =
    let labels = Effect.Label.Multiset.Non_empty.of_list_exn [ lab_handled ] in
    let tail = Effect.Row.Tail.Metavariable eff_rest_meta in
    Effect.Row (Effect.Row.Open (labels, tail))
  in
  let t_action =
    (* () -> <lab_handled|eff_rest> t_subject *)
    Type.Mono.Arrow ([], eff_pre_handle, t_subject)
  in
  let t_handler = Type.Mono.Arrow ([ t_action ], eff_rest, t_handler_result) in
  let handler' =
    { Expl.Expr.handled_effect = lab_handled
    ; operations = operations'
    ; return_clause = return_clause'
    }
  in
  t_handler, handler'

(** Infer and check an operation handler's type *)
and infer_operation
    :  lab_handled:Effect.Label.t -> eff_rest:Effect.t -> env:Context.t
    -> effect_env:Effect_signature.Context.t -> t_handler_result:Type.Mono.t
    -> name:Variable.t -> Min.Expr.op_handler
    -> Expl.Expr.op_handler Inference.t
  =
 fun ~lab_handled ~eff_rest ~env ~effect_env ~t_handler_result ~name op_handler ->
  (* TODO: rather than unifying to check the type here, use a different context
     containing the declarations *)
  (* TODO: using inference here is not ideal - since failure is not a type
     error, but a programmer one *)
  let open Inference.Let_syntax in
  (* lookup operation's [name] and unify with `t_argument -> <lab_handled>
     t_answer` *)
  let%bind t_op, _name =
    infer_value ~env ~effect_env (Min.Expr.Variable name)
  in
  let%bind t_argument = Inference.fresh_metavariable in
  let t_argument = Type.Mono.Metavariable t_argument in
  let%bind t_answer = Inference.fresh_metavariable in
  let t_answer = Type.Mono.Metavariable t_answer in
  let eff_lab_handled = Effect.Row (Effect.Row.closed_singleton lab_handled) in
  let t_op_expected =
    Type.Mono.Arrow ([ t_argument ], eff_lab_handled, t_answer)
  in
  let%bind () = Inference.unify t_op t_op_expected in
  let t_resume =
    Type.Mono (Type.Mono.Arrow ([ t_answer ], eff_rest, t_handler_result))
  in
  let env_with_resume =
    (* TODO: need to prevent escape of non first-class resume *)
    match Context.extend env ~var:Min.Keyword.resume ~type_:t_resume with
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
    -> t_argument:Type.Mono.t -> Min.Expr.op_handler
    -> Expl.Expr.op_handler Inference.t
  =
 fun ~eff_rest ~env ~effect_env ~t_handler_result ~t_argument op_handler ->
  let open Inference.Let_syntax in
  let { Min.Expr.op_argument; op_body } = op_handler in
  let%bind env' =
    add_binding ~env ~var:op_argument ~type_:(Type.Mono t_argument)
  in
  let%bind t_result, eff_result, op_body' =
    infer ~env:env' ~effect_env op_body
  in
  (* unifying with [t_handler_result] rather than e.g. [t_answer] because
     [resume] is explicit - so the result of op_body is what is returned from
     the handler (perhaps via some more returns from [resume]) *)
  let%bind () = Inference.unify t_result t_handler_result in
  let%map () = Inference.unify_effects eff_result eff_rest in
  { Expl.Expr.op_argument; op_body = op_body' }
;;

(** add an effect's operations to the context *)
let bind_operations
    : Context.t -> declaration:Min.Decl.Effect.t -> Context.t Inference.t
  =
 fun env ~declaration ->
  let open Inference.Let_syntax in
  let { Min.Decl.Effect.name = label; operations } = declaration in
  Inference.map_fold operations ~init:env ~f:(fun ~key:op_name ~data:op env ->
      let { Min.Decl.Effect.Operation.argument; answer } = op in
      (* `forall eff_rest. argument -> <label|eff_rest> answer` *)
      let%bind eff_rest = Inference.fresh_effect_variable in
      let tail = Effect.Row.Tail.Variable eff_rest in
      let labels = Effect.Label.Multiset.Non_empty.of_list_exn [ label ] in
      let eff = Effect.Row (Effect.Row.Open (labels, tail)) in
      let monotype = Type.Mono.Arrow ([ argument ], eff, answer) in
      let forall_bound = Type.Variable.Set.empty in
      let forall_bound_effects = Effect.Variable.Set.singleton eff_rest in
      let type_ =
        Type.Poly { Type.Poly.forall_bound; forall_bound_effects; monotype }
      in
      match Context.extend_operation env ~var:op_name ~label ~type_ with
      | `Ok env' -> return env'
      | `Cannot_shadow ->
        let message =
          sprintf
            "operation names must be unique: '%s' is reused"
            (Variable.to_string_user op_name)
        in
        Inference.type_error message)
;;

(** add an effect's signature to the effect environment *)
let bind_effect_signature
    :  Effect_signature.Context.t -> declaration:Min.Decl.Effect.t
    -> Effect_signature.Context.t Inference.t
  =
 fun effect_env ~declaration ->
  let open Inference.Let_syntax in
  match Effect_signature.Context.extend_decl effect_env declaration with
  | `Ok effect_env -> return effect_env
  | `Duplicate ->
    let { Min.Decl.Effect.name; _ } = declaration in
    let message =
      sprintf "effect '%s' is already defined" (Effect.Label.to_string name)
    in
    Inference.type_error message
;;

let convert_effect_decl : Min.Decl.Effect.t -> Expl.Decl.Effect.t =
 fun decl -> decl
;;

(** check an effect declaration, adding its signature and operations to the
    contexts if correct *)
let infer_effect_decl
    :  Min.Decl.Effect.t -> env:Context.t
    -> effect_env:Effect_signature.Context.t
    -> (Context.t * Effect_signature.Context.t * Expl.Decl.Effect.t) Inference.t
  =
 fun declaration ~env ~effect_env ->
  let open Inference.Let_syntax in
  let%bind env =
    (* add all operation names to the context *)
    bind_operations env ~declaration
  in
  let%map effect_env = bind_effect_signature effect_env ~declaration in
  let declaration' = convert_effect_decl declaration in
  env, effect_env, declaration'
;;

(** run inference on a function declaration, adding it (generalised) to the
    environment if well typed *)
let infer_fun_decl
    :  Min.Decl.Fun.t -> env:Context.t -> effect_env:Effect_signature.Context.t
    -> (Context.t * Expl.Decl.Fun.t) Inference.t
  =
 fun f ~env ~effect_env ->
  let open Inference.Let_syntax in
  let f_name, _lambda = f in
  let%bind t_f, f' = infer_fix_lambda ~env ~effect_env f in
  let%bind (t_f : Type.Poly.t) = Inference.generalise_total t_f ~env in
  let%map env' = add_binding ~env ~var:f_name ~type_:(Type.Poly t_f) in
  env', f'
;;

let infer_decl
    :  Min.Decl.t -> env:Context.t -> effect_env:Effect_signature.Context.t
    -> (Context.t * Effect_signature.Context.t * Expl.Decl.t) Inference.t
  =
 fun declaration ~env ~effect_env ->
  let open Inference.Let_syntax in
  match declaration with
  | Min.Decl.Fun f ->
    let%map env', f' = infer_fun_decl f ~env ~effect_env in
    env', effect_env, Expl.Decl.Fun f'
  | Min.Decl.Effect e ->
    let%map env', effect_env', e' = infer_effect_decl e ~env ~effect_env in
    env', effect_env', Expl.Decl.Effect e'
;;

let infer_decls
    :  Min.Decl.t list -> env:Context.t -> effect_env:Effect_signature.Context.t
    -> (Context.t * Effect_signature.Context.t * Expl.Decl.t list) Inference.t
  =
 fun declarations ~env ~effect_env ->
  let open Inference.Let_syntax in
  (* importantly this is a left fold *)
  let%map env', effect_env', declarations_rev' =
    Inference.list_fold
      declarations
      ~init:(env, effect_env, [])
      ~f:(fun (env, effect_env, declarations_rev) declaration ->
        let%map env', effect_env', declaration' =
          infer_decl declaration ~env ~effect_env
        in
        env', effect_env', declaration' :: declarations_rev)
  in
  let declarations' = List.rev declarations_rev' in
  env', effect_env', declarations'
;;

let infer_expr_toplevel
    :  Min.Expr.t -> declarations:Min.Decl.t list
    -> (Type.Mono.t * Effect.t * Expl.Expr.t) Or_static_error.t
  =
 fun expr ~declarations ->
  let%map.Result (t, eff, expr'), substitution =
    Inference.run
      (let env = Context.empty in
       let effect_env = Effect_signature.Context.empty in
       let%bind.Inference env, effect_env, _declarations' =
         infer_decls ~env ~effect_env declarations
       in
       infer ~env ~effect_env expr)
  in
  (* convert to a type with only unknown metavariables *)
  ( Substitution.apply_to_mono substitution t
  , Substitution.apply_to_effect substitution eff
  , expr' )
;;

let infer_program : Min.Program.t -> Explicit_syntax.Program.t Or_static_error.t
  =
 fun { Min.Program.declarations } ->
  let declarations =
    [ Min.Decl.Effect Minimal_syntax.Decl.Effect.console ]
    @ declarations
    @ [ Min.Decl.Fun Min.Program.entry_point ]
  in
  let%map.Result (_env, _effect_env, declarations'), _substitution =
    let env = Context.empty in
    let effect_env = Effect_signature.Context.empty in
    Inference.run (infer_decls declarations ~env ~effect_env)
  in
  { Expl.Program.declarations = declarations' }
;;

let infer_program_without_main
    : Min.Program.t -> Explicit_syntax.Program.t Or_static_error.t
  =
 fun { Min.Program.declarations } ->
  let%map.Result (_env, _effect_env, declarations'), _substitution =
    let env = Context.empty in
    let effect_env = Effect_signature.Context.empty in
    Inference.run (infer_decls declarations ~env ~effect_env)
  in
  { Expl.Program.declarations = declarations' }
;;
