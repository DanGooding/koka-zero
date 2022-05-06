open Core
open Import

(** source syntax *)
module Expl = Koka_zero_inference.Explicit_syntax

(** target syntax *)
module EPS = Evidence_passing_syntax

(* [`Value e] indicates that expression [e] represents a pure term, which must
   be lifted into the effect monad via [EPS.Expr.Construct_pure] *)

let translate_literal (l : Expl.Literal.t) : EPS.Literal.t = l

let translate_unary_operator (op : Expl.Operator.Unary.t) : EPS.Operator.Unary.t
  =
  op
;;

let translate_effect_label (label : Effect.Label.t) : EPS.Expr.t =
  EPS.Expr.Effect_label label
;;

(** [make_bind_into e ~evv ~f:(fun x ~evv -> e')] builds the expression
    [(e, evv) >>= fun (x, evv) -> e'] *)
let make_bind_into
    :  EPS.Expr.t -> evv:EPS.Expr.t
    -> f:(EPS.Expr.t -> evv:EPS.Expr.t -> EPS.Expr.t Generation.t)
    -> EPS.Expr.t Generation.t
  =
 fun e ~evv ~f ->
  let open Generation.Let_syntax in
  let%map rhs = Generation.make_lambda_expr_2 (fun x evv -> f x ~evv) in
  EPS.Expr.Application (EPS.Expr.Variable Primitives.Names.bind, [ e; evv; rhs ])
;;

(** [make_map_into e ~f:(fun x -> e')] builds the expression
    [e >>= fun x -> pure e'] *)
let make_map_into
    :  EPS.Expr.t -> evv:EPS.Expr.t
    -> f:(EPS.Expr.t -> [ `Value of EPS.Expr.t ] Generation.t)
    -> EPS.Expr.t Generation.t
  =
 fun e ~evv ~f ->
  let open Generation.Let_syntax in
  let%map rhs =
    Generation.make_lambda_expr_2 (fun x _evv ->
        let%map (`Value v_y) = f x in
        (* construct [Pure v_y] *)
        EPS.Expr.Construct_pure v_y)
  in
  EPS.Expr.Application (EPS.Expr.Variable Primitives.Names.bind, [ e; evv; rhs ])
;;

(** [make_bind_many_into
    \[(fun ~evv -> e1);...;(fun ~evv -> en)\] ~evv
    ~f:(fun \[x1;...;xn\] ~evv -> e')]
    builds the expression
    [(e1, evv) >>= fun (x1, evv) -> (e2, evv) >>= fun (x2, evv) -> ... (en, evv) >>= fun xn -> e'] *)
let make_bind_many_into
    :  (evv:EPS.Expr.t -> EPS.Expr.t Generation.t) list -> evv:EPS.Expr.t
    -> f:(EPS.Expr.t list -> evv:EPS.Expr.t -> EPS.Expr.t Generation.t)
    -> EPS.Expr.t Generation.t
  =
  let open Generation.Let_syntax in
  let rec make_bind_many_into ts ~evv ~xs_rev ~f =
    match ts with
    | [] ->
      let xs = List.rev xs_rev in
      f xs ~evv
    | t :: ts' ->
      let%bind m = t ~evv in
      make_bind_into m ~evv ~f:(fun x ~evv ->
          make_bind_many_into ts' ~evv ~xs_rev:(x :: xs_rev) ~f)
  in
  fun ts ~evv ~f -> make_bind_many_into ts ~evv ~xs_rev:[] ~f
;;

(** Translate a given term, also returning the free variable it expects its
    evidence vector in. Useful for effectful terms wrapped in lambdas *)
let provide_evv
    :  (evv:EPS.Expr.t -> EPS.Expr.t Generation.t)
    -> (Variable.t * EPS.Expr.t) Generation.t
  =
 fun translate ->
  let open Generation.Let_syntax in
  let%bind x_evv = Generation.fresh_name in
  let evv = EPS.Expr.Variable x_evv in
  let%map m = translate ~evv in
  x_evv, m
;;

(** [translate_expr e ~evv] translates effectful term [e] of type [t] and
    effects [eff] into a term of type [Ctl eff t], with evidence vector passed
    as a free variable [evv]. This is an uncurried form of the monad
    [Mon eff t = Evv eff -> Ctl eff t]. *)
let rec translate_expr
    : Expl.Expr.t -> evv:EPS.Expr.t -> EPS.Expr.t Generation.t
  =
  let open Generation.Let_syntax in
  fun e ~evv ->
    match e with
    | Expl.Expr.Value v ->
      let%map (`Value v') = translate_value v in
      EPS.Expr.Construct_pure v'
    | Expl.Expr.Application (e_f, e_args) ->
      (* `e_f(e_1, e_2, ..., e_n)` translates to: *)
      (* `(e_f,evv) >>= (\f evv. (e_1,evv) >>= (\x1 evv. (e_2,evv) >>= (\x2 evv.
         ... (e_n, evv) >>= (\xn evv. f(x1, x2, ..., xn, evv) ))))` *)
      let%bind m_f = translate_expr e_f ~evv in
      make_bind_into m_f ~evv ~f:(fun f ~evv ->
          let (translate_args
                : (evv:EPS.Expr.t -> EPS.Expr.t Generation.t) list)
            =
            List.map e_args ~f:translate_expr
          in
          make_bind_many_into translate_args ~evv ~f:(fun xs ~evv ->
              EPS.Expr.Application (f, xs @ [ evv ]) |> return))
    | Expl.Expr.Unary_operator (op, e) ->
      let op' = translate_unary_operator op in
      let%bind m_e = translate_expr e ~evv in
      make_map_into m_e ~evv ~f:(fun x ->
          `Value (EPS.Expr.Unary_operator (op', x)) |> return)
    | Expl.Expr.Operator (e_left, op, e_right) ->
      translate_operator ~e_left op ~e_right ~evv
    | Expl.Expr.If_then_else (e_cond, e_yes, e_no) ->
      let%bind e_cond' = translate_expr e_cond ~evv in
      make_bind_into e_cond' ~evv ~f:(fun cond ~evv ->
          let%bind e_yes' = translate_expr e_yes ~evv in
          let%map e_no' = translate_expr e_no ~evv in
          EPS.Expr.If_then_else (cond, e_yes', e_no'))
    | Expl.Expr.Let (x, v_subject, e_body) ->
      let%bind (`Value subject') = translate_value v_subject in
      let%map m_body = translate_expr e_body ~evv in
      EPS.Expr.Let (EPS.Parameter.Variable x, subject', m_body)
    | Expl.Expr.Let_mono (x, e_subject, e_body) ->
      (* `(e_subject, evv) >>= (\x evv'. e_body)` *)
      let%bind m_subject = translate_expr e_subject ~evv in
      (* can't just use [make_lambda], since [e_body] expects the name [x] *)
      let%map x_evv_body, m_body = provide_evv (translate_expr e_body) in
      let ps =
        [ EPS.Parameter.Variable x; EPS.Parameter.Variable x_evv_body ]
      in
      let lambda = ps, m_body in
      EPS.Expr.Application
        ( EPS.Expr.Variable Primitives.Names.bind
        , [ m_subject; evv; EPS.Expr.Lambda lambda ] )
    | Expl.Expr.Seq (e1, e2) ->
      let%bind e1' = translate_expr e1 ~evv in
      make_bind_into e1' ~evv ~f:(fun _x1 ~evv -> translate_expr e2 ~evv)
    | Expl.Expr.Impure_built_in impure -> translate_impure_built_in impure ~evv

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
    let%map (`Value h') = translate_handler h in
    let { Expl.Expr.handled_effect = label; _ } = h in
    let label' : EPS.Expr.t = translate_effect_label label in
    let handler' =
      EPS.Expr.Application
        (EPS.Expr.Variable Primitives.Names.handler, [ label'; h' ])
    in
    `Value handler'
  | Expl.Expr.Perform
      { Expl.Expr.operation = op_name; performed_effect = label } ->
    let%map selector =
      Generation.make_lambda_expr_1 (fun h ->
          (* TODO: can we just carry indicies around, rather than select *)
          EPS.Expr.Select_operation (label, op_name, h) |> return)
    in
    let label' : EPS.Expr.t = translate_effect_label label in
    let perform' =
      EPS.Expr.Application
        (EPS.Expr.Variable Primitives.Names.perform, [ label'; selector ])
    in
    `Value perform'

and translate_lambda : Expl.Expr.lambda -> EPS.Expr.lambda Generation.t =
 fun (ps, e_body) ->
  let open Generation.Let_syntax in
  let%map x_evv, m_body = provide_evv (translate_expr e_body) in
  let ps = ps @ [ EPS.Parameter.Variable x_evv ] in
  ps, m_body

and translate_fix_lambda
    : Expl.Expr.fix_lambda -> EPS.Expr.fix_lambda Generation.t
  =
 fun (f, lambda) ->
  let open Generation.Let_syntax in
  let%map m_lambda = translate_lambda lambda in
  f, m_lambda

(** tranlate an operator expression, taking operands as untranslated expressions
    to allow short-circuiting for boolean operators *)
and translate_operator
    :  e_left:Expl.Expr.t -> Expl.Operator.t -> e_right:Expl.Expr.t
    -> evv:EPS.Expr.t -> EPS.Expr.t Generation.t
  =
 fun ~e_left op ~e_right ~evv ->
  let open Generation.Let_syntax in
  match op with
  | Expl.Operator.Int iop ->
    let op = EPS.Operator.Int iop in
    let%bind m_left = translate_expr e_left ~evv in
    make_bind_into m_left ~evv ~f:(fun x_left ~evv ->
        let%bind m_right = translate_expr e_right ~evv in
        make_map_into m_right ~evv ~f:(fun x_right ->
            `Value (EPS.Expr.Operator (x_left, op, x_right)) |> return))
  | Expl.Operator.Bool bop ->
    let%bind m_left = translate_expr e_left ~evv in
    make_bind_into m_left ~evv ~f:(fun x_left ~evv ->
        let%map m_right = translate_expr e_right ~evv in
        let lift_bool b =
          EPS.Expr.Construct_pure (EPS.Expr.Literal (EPS.Literal.Bool b))
        in
        match bop with
        | Expl.Operator.Bool.Or ->
          (* a || b === a ? Pure True : b *)
          EPS.Expr.If_then_else (x_left, lift_bool true, m_right)
        | Expl.Operator.Bool.And ->
          (* a && b === a ? b : Pure False *)
          EPS.Expr.If_then_else (x_left, m_right, lift_bool false))

and translate_handler
    : Expl.Expr.handler -> [ `Value of EPS.Expr.t ] Generation.t
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
  `Value (EPS.Expr.Construct_handler { handled_effect; operation_clauses })

and translate_op_handler
    : Operation_shape.t -> Expl.Expr.op_handler -> EPS.Expr.t Generation.t
  =
 fun shape { Expl.Expr.op_argument; op_body } ->
  let open Generation.Let_syntax in
  match shape with
  | Operation_shape.Control ->
    let resume = Expl.Keyword.resume in
    let%map x_evv, m_body = provide_evv (translate_expr op_body) in
    let clause =
      EPS.Expr.Lambda
        ( [ op_argument
          ; EPS.Parameter.Variable resume
          ; EPS.Parameter.Variable x_evv
          ]
        , m_body )
    in
    EPS.Expr.Construct_op_normal clause
  | Operation_shape.Fun ->
    let%map x_evv, m_body = provide_evv (translate_expr op_body) in
    let clause =
      EPS.Expr.Lambda ([ op_argument; EPS.Parameter.Variable x_evv ], m_body)
    in
    EPS.Expr.Construct_op_tail clause

and translate_impure_built_in
    : Expl.Expr.impure_built_in -> evv:EPS.Expr.t -> EPS.Expr.t Generation.t
  =
  let open Generation.Let_syntax in
  fun built_in ~evv ->
    match built_in with
    | Expl.Expr.Impure_print_int e ->
      let%bind e' = translate_expr e ~evv in
      make_map_into e' ~evv ~f:(fun x ->
          (* `Value as in "won't Yield", does actually perform "real" effect! *)
          `Value (EPS.Expr.Impure_built_in (EPS.Expr.Impure_print_int x))
          |> return)
    | Expl.Expr.Impure_read_int ->
      EPS.Expr.Construct_pure
        (EPS.Expr.Impure_built_in EPS.Expr.Impure_read_int)
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
  (* [entry_point(nil_evidence_vector)] *)
  let entry_expr =
    EPS.Expr.Application
      ( EPS.Expr.Variable EPS.Keyword.entry_point
      , [ EPS.Expr.Nil_evidence_vector ] )
  in
  { EPS.Program.effect_declarations; fun_declarations; entry_expr }
;;

let translate program = translate_program program ~include_prelude:true

let translate_no_prelude program =
  translate_program program ~include_prelude:false
;;
