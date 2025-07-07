open! Core
open! Import
module Min = Minimal_syntax
module Expl = Explicit_syntax

type t =
  { metavariables : Metavariables.t
  ; constraints : Constraints.t
  }

let create () =
  let metavariables = Metavariables.create () in
  let constraints = Constraints.create ~metavariables in
  { metavariables; constraints }
;;

let union_effects t effects_ ~level =
  let open Result.Let_syntax in
  let overall_effect = Metavariables.fresh_effect t.metavariables ~level in
  let%map () =
    List.map effects_ ~f:(fun effect_ ->
      Constraints.constrain_effect_at_most t.constraints effect_ overall_effect)
    |> Result.all_unit
  in
  overall_effect
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

let error_if_cannot_shadow result ~name =
  match result with
  | `Cannot_shadow ->
    Or_error.error_s [%message "cannot shadow" (name : Variable.t)]
  | `Ok result -> Ok result
;;

let instantiate (t : t) (poly : Type.Poly.t) ~level =
  let fresh_types = Type.Metavariable.Table.create () in
  let fresh_effects = Effect.Metavariable.Table.create () in
  let rec instantiate_aux (type_ : Type.Mono.t) : Type.Mono.t =
    match type_ with
    | Primitive p -> Primitive p
    | Variable v -> Variable v
    | Arrow (args, effect_, result) ->
      let args = List.map args ~f:instantiate_aux in
      let effect_ = instantiate_effect_aux effect_ in
      let result = instantiate_aux result in
      Arrow (args, effect_, result)
    | Metavariable meta when poly.forall_bound meta ->
      (match Hashtbl.find fresh_types meta with
       | Some meta' -> Metavariable meta'
       | None ->
         let meta' =
           Metavariables.fresh_type_metavariable t.metavariables ~level
         in
         Hashtbl.add_exn fresh_types ~key:meta ~data:meta';
         (* freshen bounds to [level] *)
         let bounds = Constraints.get_type_bounds t.constraints meta in
         Option.iter bounds ~f:(fun { Bounds.lower_bounds; upper_bounds } ->
           let lower_bounds = List.map lower_bounds ~f:instantiate_aux in
           let upper_bounds = List.map upper_bounds ~f:instantiate_aux in
           Constraints.add_fresh_type_exn
             t.constraints
             meta'
             { Bounds.lower_bounds; upper_bounds });
         Metavariable meta')
    | Metavariable meta -> Metavariable meta
  and instantiate_effect_aux (effect_ : Effect.t) : Effect.t =
    match effect_ with
    | Labels labels -> Labels labels
    | Unknown unknown -> Unknown (instantiate_unknown_effect_aux unknown)
    | Handled (labels, unknown) ->
      Handled (labels, instantiate_unknown_effect_aux unknown)
  and instantiate_unknown_effect_aux (effect_ : Effect.Unknown.t)
    : Effect.Unknown.t
    =
    match effect_ with
    | Variable v -> Variable v
    | Metavariable meta when poly.forall_bound_effects meta ->
      (match Hashtbl.find fresh_effects meta with
       | Some meta' -> Metavariable meta'
       | None ->
         let meta' =
           Metavariables.fresh_effect_metavariable t.metavariables ~level
         in
         Hashtbl.add_exn fresh_effects ~key:meta ~data:meta';
         (* freshen bounds to [level] *)
         let bounds = Constraints.get_effect_bounds t.constraints meta in
         Option.iter bounds ~f:(fun { Bounds.lower_bounds; upper_bounds } ->
           let lower_bounds = List.map lower_bounds ~f:instantiate_effect_aux in
           let upper_bounds = List.map upper_bounds ~f:instantiate_effect_aux in
           Constraints.add_fresh_effect_exn
             t.constraints
             meta'
             { Bounds.lower_bounds; upper_bounds });
         Metavariable meta')
    | Metavariable meta -> Metavariable meta
  in
  instantiate_aux poly.monotype
;;

let lookup_effect_for_handler
      (handler : Min.Expr.handler)
      ~(effect_env : Effect_signature.Context.t)
  : (Effect.Label.t * Operation_shape.t Variable.Map.t) Or_error.t
  =
  let open Result.Let_syntax in
  let signature = Effect_signature.t_of_handler handler in
  match Effect_signature.Context.find effect_env signature with
  | Some (lab_handled, operation_shapes) ->
    return (lab_handled, operation_shapes)
  | None ->
    (* TODO: use actual to_string to give nice output *)
    let signature_str =
      Effect_signature.sexp_of_t signature |> Sexp.to_string_hum
    in
    Or_error.errorf "handler does not match any effect: %s" signature_str
;;

let check_handler_shape
      ~(handler : Operation_shape.t)
      ~(declaration : Operation_shape.t)
      ~name
  : unit Or_error.t
  =
  let open Result.Let_syntax in
  if Operation_shape.can_implement ~handler ~declaration
  then return ()
  else
    Or_error.errorf
      "cannot handle operation `%s` declared as `%s` with `%s` clause"
      (Variable.to_string_user name)
      (Operation_shape.to_string declaration)
      (Operation_shape.to_string handler)
;;

(** determine the type and effect of an expression, raising if we encounter a type-error.
    This will update [constraints], and won't fully solve them. *)
let rec infer_expr
          (t : t)
          (expr : Min.Expr.t)
          ~(env : Context.t)
          ~(level : int)
          ~(effect_env : Effect_signature.Context.t)
  : (Expl.Expr.t * Type.Mono.t * Effect.t) Or_error.t
  =
  let open Result.Let_syntax in
  match expr with
  | Value value ->
    let%map value, type_ = infer_value t value ~env ~level ~effect_env in
    let effect_ = Effect.Labels Effect.Label.Set.empty in
    Expl.Expr.Value value, type_, effect_
  | Let (var, subject, body) ->
    let local_level = level + 1 in
    let%bind subject, type_subject =
      infer_value t subject ~env ~level:local_level ~effect_env
    in
    let poly_subject =
      (* generalise only metavariables introduced within [subject] *)
      Type.generalise
        type_subject
        ~should_generalise_type_metavariable:(fun meta ->
          Metavariables.type_level_exn t.metavariables meta >= local_level)
        ~should_generalise_effect_metavariable:(fun meta ->
          Metavariables.effect_level_exn t.metavariables meta >= local_level)
    in
    let%bind env =
      Context.extend env ~var ~type_:(Poly poly_subject)
      |> error_if_cannot_shadow ~name:var
    in
    let%map body, type_, effect_ = infer_expr t body ~env ~level ~effect_env in
    Expl.Expr.Let (var, subject, body), type_, effect_
  | Let_mono (var, subject, body) ->
    let%bind subject, type_subject, effect_subject =
      infer_expr t subject ~env ~level ~effect_env
    in
    let%bind env =
      Context.extend env ~var ~type_:(Mono type_subject)
      |> error_if_cannot_shadow ~name:var
    in
    let%bind body, type_body, effect_body =
      infer_expr t body ~env ~level ~effect_env
    in
    let%map overall_effect =
      union_effects t [ effect_subject; effect_body ] ~level
    in
    Expl.Expr.Let_mono (var, subject, body), type_body, overall_effect
  | Application (f, args) ->
    let%bind args_and_types_and_effects =
      List.map args ~f:(fun arg -> infer_expr t arg ~env ~level ~effect_env)
      |> Result.all
    in
    let args, arg_types, arg_effects = List.unzip3 args_and_types_and_effects in
    let%bind f, function_type, function_expr_effect =
      infer_expr t f ~env ~level ~effect_env
    in
    let result : Type.Mono.t =
      Metavariables.fresh_type t.metavariables ~level
    in
    let%bind overall_effect =
      union_effects t (function_expr_effect :: arg_effects) ~level
    in
    let%map () =
      Constraints.constrain_type_at_most
        t.constraints
        function_type
        (Arrow (arg_types, overall_effect, result))
    in
    Expl.Expr.Application (f, args), result, overall_effect
  | Seq (first, second) ->
    let%bind first, _type, first_effect =
      infer_expr t first ~env ~level ~effect_env
    in
    let%bind second, second_type, second_effect =
      infer_expr t second ~env ~level ~effect_env
    in
    let%map overall_effect =
      union_effects t [ first_effect; second_effect ] ~level
    in
    Expl.Expr.Seq (first, second), second_type, overall_effect
  | If_then_else (e_cond, e_true, e_false) ->
    let%bind e_cond, type_cond, effect_cond =
      infer_expr t e_cond ~env ~level ~effect_env
    in
    let%bind e_true, type_true, effect_true =
      infer_expr t e_true ~env ~level ~effect_env
    in
    let%bind e_false, type_false, effect_false =
      infer_expr t e_false ~env ~level ~effect_env
    in
    let%bind () =
      Constraints.constrain_type_at_most
        t.constraints
        type_cond
        (Primitive Bool)
    in
    let result_type = Metavariables.fresh_type t.metavariables ~level in
    let%bind () =
      Constraints.constrain_type_at_most t.constraints type_true result_type
    in
    let%bind () =
      Constraints.constrain_type_at_most t.constraints type_false result_type
    in
    let%map overall_effect =
      union_effects t [ effect_cond; effect_true; effect_false ] ~level
    in
    ( Expl.Expr.If_then_else (e_cond, e_true, e_false)
    , result_type
    , overall_effect )
  | Operator (left, op, right) ->
    let arg_type = operand_type op |> Type.Mono.Primitive in
    let result_type = operator_result_type op |> Type.Mono.Primitive in
    let%bind left, left_type, left_effect =
      infer_expr t left ~env ~level ~effect_env
    in
    let%bind right, right_type, right_effect =
      infer_expr t right ~env ~level ~effect_env
    in
    let%bind () =
      Constraints.constrain_type_at_most t.constraints left_type arg_type
    in
    let%bind () =
      Constraints.constrain_type_at_most t.constraints right_type arg_type
    in
    let%map overall_effect =
      union_effects t [ left_effect; right_effect ] ~level
    in
    Expl.Expr.Operator (left, op, right), result_type, overall_effect
  | Unary_operator (uop, arg) ->
    let op_arg_type = unary_operand_type uop |> Type.Mono.Primitive in
    let result_type = unary_operator_result_type uop |> Type.Mono.Primitive in
    let%bind arg, arg_type, effect_ =
      infer_expr t arg ~env ~level ~effect_env
    in
    let%map () =
      Constraints.constrain_type_at_most t.constraints arg_type op_arg_type
    in
    Expl.Expr.Unary_operator (uop, arg), result_type, effect_
  | Impure_built_in impure_built_in ->
    let%map impure_built_in, type_, effect_ =
      infer_impure_built_in t impure_built_in ~env ~level ~effect_env
    in
    Expl.Expr.Impure_built_in impure_built_in, type_, effect_

and infer_value (t : t) (value : Min.Expr.value) ~env ~level ~effect_env
  : (Expl.Expr.value * Type.Mono.t) Or_error.t
  =
  let open Result.Let_syntax in
  match value with
  | Variable name ->
    (match (Context.find env name : Context.Binding.t option) with
     | None -> raise_s [%message "name not found" (name : Variable.t)]
     | Some (Value type_) ->
       let type_ =
         match (type_ : Type.t) with
         | Mono mono -> mono
         | Poly poly -> instantiate t poly ~level
       in
       return (Expl.Expr.Variable name, type_)
     | Some (Operation { argument; label; answer }) ->
       let type_ =
         Type.Mono.Arrow
           ([ argument ], Labels (Effect.Label.Set.singleton label), answer)
       in
       let expr =
         Expl.Expr.Perform { operation = name; performed_effect = label }
       in
       return (expr, type_))
  | Lambda lambda ->
    let%map lambda, type_ = infer_lambda t lambda ~env ~level ~effect_env in
    Expl.Expr.Lambda lambda, type_
  | Fix_lambda (var, lambda) ->
    let meta_self = Metavariables.fresh_type t.metavariables ~level in
    let%bind env =
      Context.extend env ~var ~type_:(Mono meta_self)
      |> error_if_cannot_shadow ~name:var
    in
    let%bind lambda, lambda_type =
      infer_lambda t lambda ~env ~level ~effect_env
    in
    (* [lambda] should be usable as [f_self] *)
    let%map () =
      Constraints.constrain_type_at_most t.constraints lambda_type meta_self
    in
    Expl.Expr.Fix_lambda (var, lambda), lambda_type
  | Literal lit -> return (Expl.Expr.Literal lit, type_literal lit)
  | Handler handler ->
    let%map handler, type_ = infer_handler t handler ~env ~level ~effect_env in
    Expl.Expr.Handler handler, type_

and infer_lambda t ((params, body) : Min.Expr.lambda) ~env ~level ~effect_env
  : (Expl.Expr.lambda * Type.Mono.t) Or_error.t
  =
  let open Result.Let_syntax in
  let param_metas =
    List.map params ~f:(fun param ->
      param, Metavariables.fresh_type t.metavariables ~level)
  in
  let%bind env =
    List.fold param_metas ~init:(return env) ~f:(fun env (param, meta) ->
      match (param : Parameter.t) with
      | Wildcard -> env
      | Variable name ->
        let%bind env = env in
        Context.extend env ~var:name ~type_:(Mono meta)
        |> error_if_cannot_shadow ~name)
  in
  let%map body, result, effect_ = infer_expr t body ~env ~level ~effect_env in
  let param_types = List.map param_metas ~f:(fun (_, meta) -> meta) in
  (params, body), Type.Mono.Arrow (param_types, effect_, result)

and type_literal (lit : Literal.t) : Type.Mono.t =
  match lit with
  | Int _ -> Primitive Int
  | Bool _ -> Primitive Bool
  | Unit -> Primitive Unit

and infer_impure_built_in
      t
      (impure_built_in : Min.Expr.impure_built_in)
      ~env
      ~level
      ~effect_env
  : (Expl.Expr.impure_built_in * Type.Mono.t * Effect.t) Or_error.t
  =
  let open Result.Let_syntax in
  match impure_built_in with
  (* these have no actual effect - they are used to implement the [console] effect's
     operations, and not exposed directly to user code *)
  | Impure_println ->
    return
      ( Expl.Expr.Impure_println
      , Type.Mono.Primitive Unit
      , Effect.Labels Effect.Label.Set.empty )
  | Impure_read_int ->
    return
      ( Expl.Expr.Impure_read_int
      , Type.Mono.Primitive Int
      , Effect.Labels Effect.Label.Set.empty )
  | Impure_print_int { value; newline } ->
    let%bind value, type_value, effect_ =
      infer_expr t value ~env ~level ~effect_env
    in
    let%map () =
      Constraints.constrain_type_at_most
        t.constraints
        type_value
        (Primitive Int)
    in
    ( Expl.Expr.Impure_print_int { value; newline }
    , Type.Mono.Primitive Unit
    , effect_ )

and infer_handler
      t
      (handler : Min.Expr.handler)
      ~env
      ~level
      ~(effect_env : Effect_signature.Context.t)
  : (Expl.Expr.handler * Type.Mono.t) Or_error.t
  =
  let open Result.Let_syntax in
  let%bind label, operation_shapes =
    lookup_effect_for_handler handler ~effect_env
  in
  let { Min.Expr.operations; return_clause } = handler in
  (* handler will have type:
    (() -> e subject
    ) -> f handler_result

    where return_clause : subject -> f handler_result
    and op clauses have type 
    fun: (argument) -> f answer
    ctl: (argument, resume : answer -> f handler_result) -> f handler_result
    and
    f >= Handled({label}, e)
  *)
  let t_subject = Metavariables.fresh_type t.metavariables ~level in
  let eff_subject =
    Metavariables.fresh_effect_metavariable t.metavariables ~level
  in
  let t_handler_result = Metavariables.fresh_type t.metavariables ~level in
  let eff_handler = Metavariables.fresh_effect t.metavariables ~level in
  let%bind () =
    Constraints.constrain_effect_at_most
      t.constraints
      (Handled (Effect.Label.Set.singleton label, Metavariable eff_subject))
      eff_handler
  in
  let%bind return_clause =
    match return_clause with
    | None ->
      let%map () =
        Constraints.constrain_type_at_most
          t.constraints
          t_subject
          t_handler_result
      in
      None
    | Some (return_clause : Min.Expr.op_handler) ->
      let%bind env =
        match (return_clause.op_argument : Parameter.t) with
        | Wildcard -> return env
        | Variable var ->
          Context.extend env ~var ~type_:(Mono t_subject)
          |> error_if_cannot_shadow ~name:var
      in
      let%bind op_body, t_return_clause, eff_return_clause =
        infer_expr t return_clause.op_body ~env ~level ~effect_env
      in
      let%bind () =
        Constraints.constrain_type_at_most
          t.constraints
          t_return_clause
          t_handler_result
      in
      let%map () =
        Constraints.constrain_effect_at_most
          t.constraints
          eff_return_clause
          eff_handler
      in
      Some { Expl.Expr.op_argument = return_clause.op_argument; op_body }
  in
  let%map
      (operations : (Operation_shape.t * Expl.Expr.op_handler) Variable.Map.t)
    =
    Map.mapi operations ~f:(fun ~key:name ~data:(handler_shape, op_handler) ->
      let declared_shape = Map.find_exn operation_shapes name in
      let%bind () =
        check_handler_shape
          ~handler:handler_shape
          ~declaration:declared_shape
          ~name
      in
      (* the subject wants a function [a -> <l> b] at the call-site
         the clause will be a function [A -> f B]
         where A -> <> B <= a - <l> b

         need a,b separately, check: A -> F B <= a -> fresh() b
         then eff_handler >= F
      *)
      let%bind t_argument, t_answer =
        match Context.find env name with
        | Some (Operation { argument; answer; label = _ }) ->
          return (argument, answer)
        | Some (Value _) ->
          raise_s [%message "operation shadowed by value" (name : Variable.t)]
        | None ->
          Or_error.error_s
            [%message "operation not in scope" (name : Variable.t)]
      in
      let%map op_handler =
        match (handler_shape : Operation_shape.t) with
        | Fun ->
          infer_operation_clause
            t
            op_handler
            ~t_argument
            ~t_answer
            ~eff_handler
            ~env
            ~level
            ~effect_env
        | Control ->
          let t_resume =
            Type.Mono.Arrow ([ t_answer ], eff_handler, t_handler_result)
          in
          let env =
            match
              Context.extend env ~var:Keyword.resume ~type_:(Mono t_resume)
            with
            | `Ok env_with_resume -> env_with_resume
            | `Cannot_shadow ->
              raise_s
                [%message "`resume` must be shadowable - can nest handlers"]
          in
          infer_operation_clause
            t
            op_handler
            ~t_argument
            ~t_answer
            ~eff_handler
            ~env
            ~level
            ~effect_env
      in
      handler_shape, op_handler)
    |> Map.fold ~init:(return Variable.Map.empty) ~f:(fun ~key ~data map ->
      let%bind data = data in
      let%map map = map in
      Map.add_exn map ~key ~data)
  in
  ( { Expl.Expr.operations; return_clause; handled_effect = label }
  , Type.Mono.Arrow
      ( [ Arrow ([], Unknown (Metavariable eff_subject), t_subject) ]
      , eff_handler
      , t_handler_result ) )

and infer_operation_clause
      t
      (op_handler : Min.Expr.op_handler)
      ~(t_argument : Type.Mono.t)
      ~(t_answer : Type.Mono.t)
      ~(eff_handler : Effect.t)
      ~env
      ~level
      ~effect_env
  : Expl.Expr.op_handler Or_error.t
  =
  let open Result.Let_syntax in
  let%bind env =
    match (op_handler.op_argument : Parameter.t) with
    | Wildcard -> return env
    | Variable var ->
      Context.extend env ~var ~type_:(Mono t_argument)
      |> error_if_cannot_shadow ~name:var
  in
  let%bind op_body, t_body, eff_op_clause =
    infer_expr t op_handler.op_body ~env ~level ~effect_env
  in
  let%bind () =
    Constraints.constrain_type_at_most t.constraints t_body t_answer
  in
  let%map () =
    Constraints.constrain_effect_at_most t.constraints eff_op_clause eff_handler
  in
  { Expl.Expr.op_body; op_argument = op_handler.op_argument }
;;

(** add an effect's operations to the context *)
let bind_operations (env : Context.t) ~(declaration : Effect_decl.t)
  : Context.t Or_error.t
  =
  let open Result.Let_syntax in
  let { Effect_decl.name = label; operations } = declaration in
  Map.to_alist operations
  |> List.fold ~init:(return env) ~f:(fun env (op_name, op) ->
    let%bind env = env in
    let { Effect_decl.Operation.shape = _; argument; answer } = op in
    match
      Context.extend_operation env ~var:op_name ~argument ~answer ~label
    with
    | `Ok env' -> return env'
    | `Cannot_shadow ->
      Or_error.errorf
        "operation names must be unique: '%s' is reused"
        (Variable.to_string_user op_name))
;;

let%expect_test "inference for a simple function" =
  let expr : Min.Expr.t =
    Let
      ( Variable.of_user "id"
      , Lambda
          ( [ Variable (Variable.of_user "a") ]
          , Value (Variable (Variable.of_user "a")) )
      , Value
          (Lambda
             ( [ Variable (Variable.of_user "x")
               ; Variable (Variable.of_user "f")
               ; Variable (Variable.of_user "y")
               ]
             , Operator
                 ( Application
                     ( Value (Variable (Variable.of_user "id"))
                     , [ Value (Variable (Variable.of_user "x")) ] )
                 , Int Plus
                 , Application
                     ( Application
                         ( Value (Variable (Variable.of_user "id"))
                         , [ Value (Variable (Variable.of_user "f")) ] )
                     , [ Value (Variable (Variable.of_user "y")) ] ) ) )) )
  in
  let inference = create () in
  let expr, type_, effect_ =
    infer_expr
      inference
      expr
      ~env:Context.empty
      ~level:0
      ~effect_env:Effect_signature.Context.empty
    |> Or_error.ok_exn
  in
  print_s [%message (expr : Expl.Expr.t)];
  [%expect
    {|
    (expr
     (Let (User id) (Lambda (((Variable (User a))) (Value (Variable (User a)))))
      (Value
       (Lambda
        (((Variable (User x)) (Variable (User f)) (Variable (User y)))
         (Operator
          (Application (Value (Variable (User id)))
           ((Value (Variable (User x)))))
          (Int Plus)
          (Application
           (Application (Value (Variable (User id)))
            ((Value (Variable (User f)))))
           ((Value (Variable (User y)))))))))))
    |}];
  print_s [%message (type_ : Type.Mono.t) (effect_ : Effect.t)];
  [%expect
    {|
    ((type_
      (Arrow ((Metavariable tm1) (Metavariable tm2) (Metavariable tm3))
       (Unknown (Metavariable em3)) (Primitive Int)))
     (effect_ (Labels ())))
    |}];
  print_s [%sexp (inference.constraints : Constraints.t)];
  [%expect
    {|
    ((type_constraints
      ((tm1 ((lower_bounds ()) (upper_bounds ((Metavariable tm4)))))
       (tm2 ((lower_bounds ()) (upper_bounds ((Metavariable tm6)))))
       (tm4 ((lower_bounds ()) (upper_bounds ((Metavariable tm5)))))
       (tm5 ((lower_bounds ()) (upper_bounds ((Primitive Int)))))
       (tm6 ((lower_bounds ()) (upper_bounds ((Metavariable tm7)))))
       (tm7
        ((lower_bounds ())
         (upper_bounds
          ((Arrow ((Metavariable tm3)) (Unknown (Metavariable em2))
            (Metavariable tm8))))))
       (tm8 ((lower_bounds ()) (upper_bounds ((Primitive Int)))))))
     (effect_constraints
      ((em0
        ((lower_bounds ((Labels ())))
         (upper_bounds ((Unknown (Metavariable em3))))))
       (em1
        ((lower_bounds ((Labels ())))
         (upper_bounds ((Unknown (Metavariable em2))))))
       (em2
        ((lower_bounds ((Labels ())))
         (upper_bounds ((Unknown (Metavariable em3))))))
       (em3 ((lower_bounds ((Labels ()))) (upper_bounds ())))))
     (already_seen_constraints
      ((Type_at_most
        (type_lo (Arrow ((Metavariable tm4)) (Labels ()) (Metavariable tm4)))
        (type_hi
         (Arrow ((Metavariable tm1)) (Unknown (Metavariable em0))
          (Metavariable tm5))))
       (Type_at_most
        (type_lo (Arrow ((Metavariable tm6)) (Labels ()) (Metavariable tm6)))
        (type_hi
         (Arrow ((Metavariable tm2)) (Unknown (Metavariable em1))
          (Metavariable tm7))))
       (Type_at_most (type_lo (Metavariable tm1)) (type_hi (Metavariable tm4)))
       (Type_at_most (type_lo (Metavariable tm2)) (type_hi (Metavariable tm6)))
       (Type_at_most (type_lo (Metavariable tm4)) (type_hi (Metavariable tm5)))
       (Type_at_most (type_lo (Metavariable tm5)) (type_hi (Primitive Int)))
       (Type_at_most (type_lo (Metavariable tm6)) (type_hi (Metavariable tm7)))
       (Type_at_most (type_lo (Metavariable tm7))
        (type_hi
         (Arrow ((Metavariable tm3)) (Unknown (Metavariable em2))
          (Metavariable tm8))))
       (Type_at_most (type_lo (Metavariable tm8)) (type_hi (Primitive Int)))
       (Effect_at_most (effect_lo (Unknown (Metavariable em0)))
        (effect_hi (Unknown (Metavariable em3))))
       (Effect_at_most (effect_lo (Unknown (Metavariable em1)))
        (effect_hi (Unknown (Metavariable em2))))
       (Effect_at_most (effect_lo (Unknown (Metavariable em2)))
        (effect_hi (Unknown (Metavariable em3))))
       (Effect_at_most (effect_lo (Labels ()))
        (effect_hi (Unknown (Metavariable em0))))
       (Effect_at_most (effect_lo (Labels ()))
        (effect_hi (Unknown (Metavariable em1))))
       (Effect_at_most (effect_lo (Labels ()))
        (effect_hi (Unknown (Metavariable em2))))
       (Effect_at_most (effect_lo (Labels ()))
        (effect_hi (Unknown (Metavariable em3))))))
     (metavariables
      ((type_metavariable_source ((next 9) (prefix tm)))
       (effect_metavariable_source ((next 4) (prefix em)))
       (type_metavariable_levels
        ((tm0 1) (tm1 0) (tm2 0) (tm3 0) (tm4 0) (tm5 0) (tm6 0) (tm7 0) (tm8 0)))
       (effect_metavariable_levels ((em0 0) (em1 0) (em2 0) (em3 0))))))
    |}];
  let type_variable_source = Type.Variable.Name_source.fresh () ~prefix:"t" in
  let effect_variable_source =
    Effect.Variable.Name_source.fresh () ~prefix:"e"
  in
  let expansion =
    Expansion.create
      ~constraints:inference.constraints
      ~type_variable_source
      ~effect_variable_source
  in
  let polar_type = Expansion.expand_type expansion type_ in
  let polar_effect = Expansion.expand_effect expansion effect_ in
  print_s
    [%message (polar_type : Polar_type.t) (polar_effect : Polar_type.Effect.t)];
  [%expect
    {|
    ((polar_type
      (Arrow
       ((Intersection ((Intersection ((Intersection ((Primitive Int)))))))
        (Intersection
         ((Intersection
           ((Intersection
             ((Arrow ((Variable t6)) (Intersection ((Variable e1)))
               (Intersection ((Primitive Int))))))))))
        (Variable t6))
       (Union ((Labels ()))) (Primitive Int)))
     (polar_effect (Labels ())))
    |}]
;;
