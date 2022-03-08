open Core
open Import

(** source syntax *)
module Expl = Koka_zero_inference.Explicit_syntax

(** target syntax *)
module EPS = Evidence_passing_syntax

(* [`Value e] indicates that expression [e] represents a pure term, which must
   be lifted into the effect monad via [monadic_of_value] *)

(** convert a value (an expression which cannot step, so is total) into a
    monadic value (via `return`), in this case one with zero effects. *)
let monadic_of_value : EPS.Expr.t -> EPS.Expr.t =
 fun v -> EPS.Expr.Application (EPS.Expr.Variable Primitives.Names.pure, [ v ])
;;

let translate_literal (l : Expl.Literal.t) : EPS.Literal.t = l
let translate_operator (op : Expl.Operator.t) : EPS.Operator.t = op

let translate_unary_operator (op : Expl.Operator.Unary.t) : EPS.Operator.Unary.t
  =
  op
;;

let translate_effect_label (label : Effect.Label.t) : EPS.Expr.t =
  EPS.Expr.Effect_label label
;;

(** [make_bind_into e ~f:(fun x -> e')] builds the expression
    [e >>= fun x -> e'] *)
let make_bind_into
    :  EPS.Expr.t -> f:(EPS.Expr.t -> EPS.Expr.t Generation.t)
    -> EPS.Expr.t Generation.t
  =
 fun e ~f ->
  let open Generation.Let_syntax in
  let%map rhs = Generation.make_lambda_expr_1 (fun x -> f x) in
  EPS.Expr.Application (EPS.Expr.Variable Primitives.Names.bind, [ e; rhs ])
;;

(** [make_map_into e ~f:(fun x -> e')] builds the expression
    [e >>= fun x -> pure e'] *)
let make_map_into
    :  EPS.Expr.t -> f:(EPS.Expr.t -> [ `Value of EPS.Expr.t ] Generation.t)
    -> EPS.Expr.t Generation.t
  =
 fun e ~f ->
  let open Generation.Let_syntax in
  let%map rhs =
    Generation.make_lambda_expr_1 (fun x ->
        let%map (`Value v_y) = f x in
        monadic_of_value v_y)
  in
  EPS.Expr.Application (EPS.Expr.Variable Primitives.Names.bind, [ e; rhs ])
;;

(** [make_bind_many_into \[e1;...;en\] ~f:(fun xs -> e')] builds the expression
    [e1 >>= fun x1 -> e2 >>= fun x2 -> ... en >>= fun xn -> e'] *)
let make_bind_many_into
    :  EPS.Expr.t list -> f:(EPS.Expr.t list -> EPS.Expr.t Generation.t)
    -> EPS.Expr.t Generation.t
  =
  let rec make_bind_many_into es ~xs_rev ~f =
    match es with
    | [] ->
      let xs = List.rev xs_rev in
      f xs
    | e :: es' ->
      make_bind_into e ~f:(fun x ->
          make_bind_many_into es' ~xs_rev:(x :: xs_rev) ~f)
  in
  fun es ~f -> make_bind_many_into es ~xs_rev:[] ~f
;;

let rec translate_expr : Expl.Expr.t -> EPS.Expr.t Generation.t =
  let open Generation.Let_syntax in
  function
  | Expl.Expr.Value v ->
    let%map (`Value v') = translate_value v in
    monadic_of_value v'
  | Expl.Expr.Application (e_f, e_args) ->
    (* `e_f(e_1, e_2, ..., e_n)` translates to: *)
    (* `e_f >>= (\f. e_1 >>= (\x1. e_2 >>= (\x2. ... e_n >>= (\xn. f(x1, x2,
       ..., xn) ))))` *)
    let%bind m_f = translate_expr e_f in
    let%bind (m_args : EPS.Expr.t list) =
      List.map e_args ~f:(fun e_arg -> translate_expr e_arg) |> Generation.all
    in
    make_bind_into m_f ~f:(fun f ->
        make_bind_many_into m_args ~f:(fun xs ->
            EPS.Expr.Application (f, xs) |> return))
  | Expl.Expr.Unary_operator (op, e) ->
    let op' = translate_unary_operator op in
    let%bind m_e = translate_expr e in
    make_map_into m_e ~f:(fun x ->
        `Value (EPS.Expr.Unary_operator (op', x)) |> return)
  | Expl.Expr.Operator (e_left, op, e_right) ->
    (* TODO: note this has no short-circuiting for boolean operators *)
    let op' = translate_operator op in
    let%bind m_left = translate_expr e_left in
    make_bind_into m_left ~f:(fun x_left ->
        let%bind m_right = translate_expr e_right in
        make_map_into m_right ~f:(fun x_right ->
            `Value (EPS.Expr.Operator (x_left, op', x_right)) |> return))
  | Expl.Expr.If_then_else (e_cond, e_yes, e_no) ->
    let%bind e_cond' = translate_expr e_cond in
    make_bind_into e_cond' ~f:(fun cond ->
        let%bind e_yes' = translate_expr e_yes in
        let%map e_no' = translate_expr e_no in
        EPS.Expr.If_then_else (cond, e_yes', e_no'))
  | Expl.Expr.Let (x, v_subject, e_body) ->
    let%bind (`Value subject') = translate_value v_subject in
    let%map m_body = translate_expr e_body in
    EPS.Expr.Application
      (EPS.Expr.Lambda ([ EPS.Parameter.Variable x ], m_body), [ subject' ])
  | Expl.Expr.Seq (e1, e2) ->
    let%bind e1' = translate_expr e1 in
    make_bind_into e1' ~f:(fun _x1 -> translate_expr e2)
  | Expl.Expr.Impure_built_in impure -> translate_impure_built_in impure

and translate_value : Expl.Expr.value -> [ `Value of EPS.Expr.t ] Generation.t =
  let open Generation.Let_syntax in
  function
  | Expl.Expr.Variable v -> `Value (EPS.Expr.Variable v) |> return
  | Expl.Expr.Literal lit ->
    let lit' = translate_literal lit in
    `Value (EPS.Expr.Literal lit') |> return
  | Expl.Expr.Lambda lambda ->
    let%map lambda' = translate_lambda lambda in
    `Value (EPS.Expr.Lambda lambda')
  | Expl.Expr.Fix_lambda fix_lambda ->
    let%map fix_lambda' = translate_fix_lambda fix_lambda in
    `Value (EPS.Expr.Fix_lambda fix_lambda')
  | Expl.Expr.Handler h ->
    (* TODO: `Hnd perhaps isn't useful? indicates it is not a first class object
       to the user *)
    let%bind (`Hnd h') = translate_handler h in
    let { Expl.Expr.handled_effect = label; _ } = h in
    let (label' : EPS.Expr.t) = translate_effect_label label in
    (* [handler'] is not a value, although it steps to one without any effects,
       therefore it must be wrapped in a lambda. The alternative is to
       essentially inline the call to [Primitives.handler], or to lie and
       pretend it is already value. *)
    let handler' =
      EPS.Expr.Application
        (EPS.Expr.Variable Primitives.Names.handler, [ label'; h' ])
    in
    let%map wrapped_handler =
      Generation.make_lambda_expr_1 (fun action ->
          EPS.Expr.Application (handler', [ action ]) |> return)
    in
    `Value wrapped_handler
  | Expl.Expr.Perform
      { Expl.Expr.operation = op_name; performed_effect = label } ->
    let%bind selector =
      Generation.make_lambda_expr_1 (fun h ->
          (* TODO: think more about how to implement `select` *)
          EPS.Expr.Select_operation (label, op_name, h) |> return)
    in
    let label' : EPS.Expr.t = translate_effect_label label in
    let perform' =
      EPS.Expr.Application
        (EPS.Expr.Variable Primitives.Names.perform, [ label'; selector ])
    in
    let%map wrapped_perform =
      Generation.make_lambda_expr_1 (fun argument ->
          EPS.Expr.Application (perform', [ argument ]) |> return)
    in
    `Value wrapped_perform

and translate_lambda : Expl.Expr.lambda -> EPS.Expr.lambda Generation.t =
 fun (ps, e_body) ->
  let open Generation.Let_syntax in
  let%map m_body = translate_expr e_body in
  ps, m_body

and translate_fix_lambda
    : Expl.Expr.fix_lambda -> EPS.Expr.fix_lambda Generation.t
  =
 fun (f, lambda) ->
  let open Generation.Let_syntax in
  let%map m_lambda = translate_lambda lambda in
  f, m_lambda

and translate_handler : Expl.Expr.handler -> [ `Hnd of EPS.Expr.t ] Generation.t
  =
 fun handler ->
  let open Generation.Let_syntax in
  let { Expl.Expr.handled_effect; operations; return_clause } = handler in
  let%bind operation_clauses =
    Map.map operations ~f:(fun (shape, op_handler) ->
        translate_op_handler shape op_handler)
    |> Generation.all_map
  in
  let%map () =
    match return_clause with
    | None -> return ()
    | Some _ -> Generation.unsupported_feature_error "return clause in handler"
  in
  `Hnd (EPS.Expr.Construct_handler { handled_effect; operation_clauses })

and translate_op_handler
    : Operation_shape.t -> Expl.Expr.op_handler -> EPS.Expr.t Generation.t
  =
 fun shape { Expl.Expr.op_argument; op_body } ->
  let open Generation.Let_syntax in
  match shape with
  | Operation_shape.Control ->
    let resume = Expl.Keyword.resume in
    let%map m_body = translate_expr op_body in
    let clause =
      EPS.Expr.Lambda ([ op_argument; EPS.Parameter.Variable resume ], m_body)
    in
    EPS.Expr.Construct_op_normal clause
  | Operation_shape.Fun ->
    let%map m_body = translate_expr op_body in
    let clause = EPS.Expr.Lambda ([ op_argument ], m_body) in
    EPS.Expr.Construct_op_tail clause

and translate_impure_built_in
    : Expl.Expr.impure_built_in -> EPS.Expr.t Generation.t
  =
  let open Generation.Let_syntax in
  function
  | Expl.Expr.Impure_print_int e ->
    let%bind e' = translate_expr e in
    make_bind_into e' ~f:(fun x ->
        EPS.Expr.Application
          ( EPS.Expr.Variable Primitives.Names.pure
          , [ EPS.Expr.Impure_built_in (EPS.Expr.Impure_print_int x) ] )
        |> return)
  | Expl.Expr.Impure_read_int ->
    EPS.Expr.Application
      ( EPS.Expr.Variable Primitives.Names.pure
      , [ EPS.Expr.Impure_built_in EPS.Expr.Impure_read_int ] )
    |> return
;;

let translate_fun_decl : Expl.Decl.Fun.t -> EPS.Program.Fun_decl.t Generation.t =
 fun fix_lambda -> translate_fix_lambda fix_lambda
;;

let translate_effect_decl : Expl.Decl.Effect.t -> EPS.Program.Effect_decl.t =
 fun { Expl.Decl.Effect.name; operations } ->
  let operations = Map.key_set operations in
  { EPS.Program.Effect_decl.name; operations }
;;

let translate_program { Expl.Program.declarations } ~include_prelude =
  let open Generation.Let_syntax in
  let%bind prelude_declarations =
    if include_prelude then Primitives.prelude else return []
  in
  let%map effect_declarations_rev, fun_declarations_rev =
    Generation.list_fold
      declarations
      ~init:([], [])
      ~f:(fun (effect_decls_rev, fun_decls_rev) decl ->
        match decl with
        | Expl.Decl.Fun decl ->
          let%map decl' = translate_fun_decl decl in
          effect_decls_rev, decl' :: fun_decls_rev
        | Expl.Decl.Effect decl ->
          let decl' = translate_effect_decl decl in
          return (decl' :: effect_decls_rev, fun_decls_rev))
  in
  let effect_declarations = List.rev effect_declarations_rev in
  let fun_declarations = List.rev fun_declarations_rev in
  let fun_declarations = prelude_declarations @ fun_declarations in
  { EPS.Program.effect_declarations; fun_declarations }
;;

let translate program = translate_program program ~include_prelude:true

let translate_no_prelude program =
  translate_program program ~include_prelude:false
;;
