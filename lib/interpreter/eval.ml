open Core
open Import
open Koka_zero_evidence_translation.Evidence_passing_syntax

let eval_literal : Literal.t -> Value.primitive = function
  | Literal.Int i -> Value.Int i
  | Literal.Bool b -> Value.Bool b
  | Literal.Unit -> Value.Unit
;;

let eval_op_expr
    :  left:Value.t -> Operator.t -> right:Value.t
    -> Value.primitive Interpreter.t
  =
 fun ~left op ~right ->
  let open Interpreter.Let_syntax in
  match op with
  | Operator.Int op ->
    let%bind left = Typecast.int_of_value left in
    let%map right = Typecast.int_of_value right in
    (match op with
    | Operator.Int.Plus -> left + right |> Value.Int
    | Operator.Int.Minus -> left - right |> Value.Int
    | Operator.Int.Times -> left * right |> Value.Int
    | Operator.Int.Divide -> left / right |> Value.Int
    | Operator.Int.Modulo ->
      left % right |> Value.Int
      (* TODO: don't inherit OCaml's behaviour for modulo of a negative *)
    | Operator.Int.Equals -> left = right |> Value.Bool
    | Operator.Int.Not_equal -> left <> right |> Value.Bool
    | Operator.Int.Less_than -> left < right |> Value.Bool
    | Operator.Int.Less_equal -> left <= right |> Value.Bool
    | Operator.Int.Greater_equal -> left >= right |> Value.Bool
    | Operator.Int.Greater_than -> left > right |> Value.Bool)
  | Operator.Bool op ->
    let%bind left = Typecast.bool_of_value left in
    let%map right = Typecast.bool_of_value right in
    (match op with
    | Operator.Bool.And -> (left && right) |> Value.Bool
    | Operator.Bool.Or -> (left || right) |> Value.Bool)
;;

let eval_unary_op_expr
    : Operator.Unary.t -> Value.t -> Value.primitive Interpreter.t
  =
 fun op v ->
  let open Interpreter.Let_syntax in
  match op with
  | Operator.Unary.Bool Operator.Bool.Unary.Not ->
    let%map v = Typecast.bool_of_value v in
    (not v) |> Value.Bool
;;

let rec lookup_evidence
    : Value.evidence_vector -> Effect_label.t -> Value.evidence option
  =
 fun vector label ->
  match vector with
  | Value.Evv_nil -> None
  | Value.Evv_cons { label = label'; evidence; tail } ->
    if Effect_label.(label = label')
    then Some evidence
    else lookup_evidence tail label
;;

let rec eval_expr : Expr.t -> env:Value.context -> Value.t Interpreter.t =
 fun expr ~env ->
  let open Interpreter.Let_syntax in
  match expr with
  | Expr.Variable name ->
    (match Variable.Map.find env name with
    | Some v -> return v
    | None ->
      let message =
        sprintf "unbound variable `%s`" (Variable.to_string_user name)
      in
      Interpreter.impossible_error message)
  | Expr.Lambda lambda ->
    let%map closure = eval_lambda lambda ~env in
    Value.Closure closure
  | Expr.Fix_lambda fix_lambda ->
    let%map closure = eval_fix_lambda fix_lambda ~env in
    Value.Closure closure
  | Expr.Application (e_f, e_args) ->
    let%bind v_f = eval_expr e_f ~env in
    let%bind closure = Typecast.closure_of_value v_f in
    let%bind v_args = List.map e_args ~f:(eval_expr ~env) |> Interpreter.all in
    let f, f_env = closure in
    let lambda, f_env =
      match f with
      | Value.Lambda lambda -> lambda, f_env
      | Value.Fix_lambda (f_name, lambda) ->
        lambda, Map.set f_env ~key:f_name ~data:(Value.Closure closure)
    in
    let xs, e_body = lambda in
    let%bind xs_to_args = Typecast.zip_arguments ~params:xs ~args:v_args in
    let f_body_env =
      List.fold xs_to_args ~init:f_env ~f:(fun f_body_env (x, v) ->
          Map.set f_body_env ~key:x ~data:v)
    in
    eval_expr e_body ~env:f_body_env
  | Expr.Literal lit -> eval_literal lit |> Value.Primitive |> return
  | Expr.If_then_else (e_cond, e_yes, e_no) ->
    let%bind v_cond = eval_expr e_cond ~env in
    let%bind cond = Typecast.bool_of_value v_cond in
    let e_body = if cond then e_yes else e_no in
    eval_expr e_body ~env
  | Expr.Operator (e_left, op, e_right) ->
    (* no short circuiting - but that has already been prevented by bind
       sequencing in [translation] *)
    let%bind v_left = eval_expr e_left ~env in
    let%bind v_right = eval_expr e_right ~env in
    let%map v = eval_op_expr ~left:v_left op ~right:v_right in
    Value.Primitive v
  | Expr.Unary_operator (op, e) ->
    let%bind v = eval_expr e ~env in
    let%map v_result = eval_unary_op_expr op v in
    Value.Primitive v_result
  | Expr.Construct_pure e ->
    let%map v = eval_expr e ~env in
    Value.Pure v |> Value.Ctl
  | Expr.Construct_yield { marker; op_clause; resumption } ->
    let%bind marker = eval_expr marker ~env in
    let%bind op_clause = eval_expr op_clause ~env in
    let%bind resumption = eval_expr resumption ~env in
    let%map marker = Typecast.marker_of_value marker in
    Value.Yield { marker; op_clause; resumption } |> Value.Ctl
  | Expr.Match_ctl { subject; pure_branch; yield_branch } ->
    let%bind v_subject = eval_expr subject ~env in
    let%bind branch, args =
      match%map Typecast.ctl_of_value v_subject with
      | Value.Pure v -> pure_branch, [ v ]
      | Value.Yield { marker; op_clause; resumption } ->
        yield_branch, [ Value.Marker marker; op_clause; resumption ]
    in
    let params, e_body = branch in
    let%bind bindings = Typecast.zip_arguments ~params ~args in
    let env' =
      List.fold bindings ~init:env ~f:(fun env (x, v) ->
          Map.set env ~key:x ~data:v)
    in
    eval_expr e_body ~env:env'
  | Expr.Fresh_marker ->
    let%map m = Interpreter.fresh_marker in
    Value.Marker m
  | Expr.Markers_equal (e1, e2) ->
    let%bind v1 = eval_expr e1 ~env in
    let%bind v2 = eval_expr e2 ~env in
    let%bind m1 = Typecast.marker_of_value v1 in
    let%map m2 = Typecast.marker_of_value v2 in
    (* note this isn't polymorphic equals, but [Int.( = )] as markers are
       transparently [int]s *)
    m1 = m2 |> Value.Bool |> Value.Primitive
  | Expr.Construct_handler { handled_effect; operation_clauses; return_clause }
    ->
    let%bind () =
      match return_clause with
      | None -> return ()
      | Some _return_clause ->
        Interpreter.unsupported_feature_error "return clause in handler"
    in
    let%map operation_clauses =
      Map.map operation_clauses ~f:(fun op_clause ->
          let%bind v_op_clause = eval_expr op_clause ~env in
          Typecast.closure_of_value v_op_clause)
      |> Interpreter.all_map
    in
    Value.Hnd { Value.handled_effect; operation_clauses }
  | Expr.Effect_label label -> Value.Effect_label label |> return
  | Expr.Nil_evidence_vector -> Value.Evidence_vector Value.Evv_nil |> return
  | Expr.Cons_evidence_vector { label; marker; handler; vector_tail } ->
    let%bind label = eval_expr label ~env in
    let%bind marker = eval_expr marker ~env in
    let%bind handler = eval_expr handler ~env in
    let%bind vector_tail = eval_expr vector_tail ~env in
    let%bind label = Typecast.effect_label_of_value label in
    let%bind marker = Typecast.marker_of_value marker in
    let%bind handler = Typecast.hnd_of_value handler in
    let%map vector_tail = Typecast.evidence_vector_of_value vector_tail in
    let evidence = { Value.marker; handler } in
    Value.Evv_cons { label; evidence; tail = vector_tail }
    |> Value.Evidence_vector
  | Expr.Lookup_evidence { label; vector } ->
    let%bind label = eval_expr label ~env in
    let%bind label = Typecast.effect_label_of_value label in
    let%bind vector = eval_expr vector ~env in
    let%bind vector = Typecast.evidence_vector_of_value vector in
    (match lookup_evidence vector label with
    | None ->
      let message =
        sprintf
          "effect label `%s` not found in vector"
          (Effect_label.to_string label)
      in
      Interpreter.impossible_error message
    | Some evidence -> Value.Evidence evidence |> return)
  | Expr.Get_evidence_marker e ->
    let%bind v = eval_expr e ~env in
    let%map { Value.marker; _ } = Typecast.evidence_of_value v in
    Value.Marker marker
  | Expr.Get_evidence_handler e ->
    let%bind v = eval_expr e ~env in
    let%map { Value.handler; _ } = Typecast.evidence_of_value v in
    Value.Hnd handler
  | Expr.Select_operation (label, op_name, e_handler) ->
    let%bind v_handler = eval_expr e_handler ~env in
    let%bind { Value.handled_effect; operation_clauses } =
      Typecast.hnd_of_value v_handler
    in
    let%bind () =
      if Effect_label.(label = handled_effect)
      then return ()
      else (
        let message =
          sprintf
            "mismatching effect labels: select `%s` from hnd `%s`"
            (Effect_label.to_string label)
            (Effect_label.to_string handled_effect)
        in
        Interpreter.impossible_error message)
    in
    let%map (op_clause : Value.closure) =
      match Map.find operation_clauses op_name with
      | Some op_clause -> return op_clause
      | None ->
        let message =
          sprintf
            "operation `%s` missing from effect"
            (Variable.to_string_user op_name)
        in
        Interpreter.impossible_error message
    in
    op_clause |> Value.Closure
  | Expr.Impure_built_in impure -> eval_impure_built_in impure ~env

and eval_lambda
    : Expr.lambda -> env:Value.context -> Value.closure Interpreter.t
  =
 fun lambda ~env ->
  let open Interpreter.Let_syntax in
  (* TODO: capturing the entire environment like this, and creating a new map
     upon every binding (rather than sharing tails) is hideously inefficient *)
  (Value.Lambda lambda, env) |> return

and eval_fix_lambda
    : Expr.fix_lambda -> env:Value.context -> Value.closure Interpreter.t
  =
 fun fix_lambda ~env ->
  let open Interpreter.Let_syntax in
  (Value.Fix_lambda fix_lambda, env) |> return

and eval_impure_built_in
    : Expr.impure_built_in -> env:Value.context -> Value.t Interpreter.t
  =
 fun impure ~env ->
  let open Interpreter.Let_syntax in
  match impure with
  | Expr.Impure_print_int e ->
    let%bind v = eval_expr e ~env in
    let%map i = Typecast.int_of_value v in
    printf "%d\n" i;
    Value.Primitive Value.Unit
  | Expr.Impure_read_int ->
    let%map i =
      Interpreter.try_io_with ~message:"failed to read int" (fun () ->
          Out_channel.flush Out_channel.stdout;
          let line = In_channel.input_line_exn In_channel.stdin in
          Int.of_string line)
    in
    Value.Primitive (Value.Int i)
;;

(** evaluate a function declaration, adding it to the context *)
let eval_fun_decl
    : Program.Fun_decl.t -> env:Value.context -> Value.context Interpreter.t
  =
 fun f ~env ->
  let open Interpreter.Let_syntax in
  let%map v_f = eval_fix_lambda f ~env in
  let f_name, _ = f in
  Map.set env ~key:f_name ~data:(Value.Closure v_f)
;;

let eval_program : Program.t -> Value.t Interpreter.t =
 fun { Program.effect_declarations = _; fun_declarations } ->
  let open Interpreter.Let_syntax in
  let env = Value.empty_context in
  let%bind env =
    List.fold fun_declarations ~init:(return env) ~f:(fun env decl ->
        let%bind env = env in
        eval_fun_decl decl ~env)
  in
  eval_expr
    (Expr.Application
       ( Expr.Application (Expr.Variable Keyword.entry_point, [])
       , [ Expr.Nil_evidence_vector ] ))
    ~env
;;
