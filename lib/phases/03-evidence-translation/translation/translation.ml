open Core
open Import

(** source syntax *)
module Expl = Explicit_syntax

(** target syntax *)
module EPS = Evidence_passing_syntax

(* [`Pure e] indicates that expression [e] represents a pure term, which must
   be lifted into the effect monad via [EPS.Expr.Construct_pure] *)

let translate_effect_label (label : Effect.Label.t) : EPS.Expr.t =
  EPS.Expr.Effect_label label
;;

(** Translate a given term, also returning the free variable it expects its
    evidence vector in. Useful for effectful terms wrapped in lambdas *)
let provide_evv
  :  (evv:EPS.Expr.t -> Maybe_effectful.t Generation.t)
  -> (Variable.t * Maybe_effectful.t) Generation.t
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
  :  Polar_type.Effect.t Expl.Expr.t
  -> evv:EPS.Expr.t
  -> Maybe_effectful.t Generation.t
  =
  let open Generation.Let_syntax in
  fun e ~evv ->
    match e with
    | Expl.Expr.Value v ->
      let%map (`Pure v') = translate_value v in
      Maybe_effectful.Pure v'
    | Expl.Expr.Application (e_f, e_args, call_effect) ->
      (* `e_f(e_1, e_2, ..., e_n)` translates to: *)
      (* `(e_f,evv) >>= (\f evv. (e_1,evv) >>= (\x1 evv. (e_2,evv) >>= (\x2 evv.
         ... (e_n, evv) >>= (\xn evv. f(x1, x2, ..., xn, evv) ))))` *)
      let%bind m_f = translate_expr e_f ~evv in
      Maybe_effectful.make_bind_or_let m_f ~evv ~f:(fun f ~evv ->
        let (translate_args
              : (evv:EPS.Expr.t -> Maybe_effectful.t Generation.t) list)
          =
          List.map e_args ~f:translate_expr
        in
        Maybe_effectful.make_bind_or_let_many
          translate_args
          ~evv
          ~f:(fun xs ~evv ->
            let args = xs @ [ evv ] in
            match Polar_type.Effect.is_total call_effect with
            | false ->
              EPS.Expr.Application
                (f, List.map args ~f:(fun arg -> arg, EPS.Type.Pure), Ctl)
              |> Maybe_effectful.Effectful
              |> return
            | true ->
              (* optimisation: know this specific instantiation of [f] won't actually perform any effect *)
              let subject =
                EPS.Expr.Application
                  (f, List.map args ~f:(fun arg -> arg, EPS.Type.Pure), Ctl)
              in
              let%map x = Generation.fresh_name in
              let pure_branch = x, EPS.Expr.Variable x in
              EPS.Expr.Match_ctl_pure { subject; pure_branch }
              |> Maybe_effectful.Pure))
    | Expl.Expr.Construction (constructor, e_args) ->
      List.map e_args ~f:translate_expr
      |> Maybe_effectful.make_bind_or_let_many ~evv ~f:(fun xs ~evv:_ ->
        EPS.Expr.Construction (constructor, xs)
        |> Maybe_effectful.Pure
        |> return)
    | Expl.Expr.Unary_operator (op, e) ->
      let%bind m_e = translate_expr e ~evv in
      Maybe_effectful.make_map_or_let m_e ~evv ~f:(fun x ->
        `Pure (EPS.Expr.Unary_operator (op, x)) |> return)
    | Expl.Expr.Operator (e_left, op, e_right) ->
      translate_operator ~e_left op ~e_right ~evv
    | Expl.Expr.If_then_else (e_cond, e_yes, e_no) ->
      let%bind e_cond' = translate_expr e_cond ~evv in
      Maybe_effectful.make_bind_or_let e_cond' ~evv ~f:(fun cond ~evv ->
        let%bind e_yes' = translate_expr e_yes ~evv in
        let%map e_no' = translate_expr e_no ~evv in
        Maybe_effectful.combine e_yes' e_no' ~f:(fun e_yes' e_no' ->
          EPS.Expr.If_then_else (cond, e_yes', e_no')))
    | Expl.Expr.Let (x, v_subject, e_body) ->
      let%bind (`Pure subject') = translate_value v_subject in
      let%map m_body = translate_expr e_body ~evv in
      Maybe_effectful.map m_body ~f:(fun m_body ->
        EPS.Expr.Let (Parameter.Variable x, Pure, subject', m_body))
    | Expl.Expr.Let_mono (x, e_subject, e_body) ->
      (* `(e_subject, evv) >>= (\x evv'. e_body)` *)
      let%bind m_subject = translate_expr e_subject ~evv in
      Maybe_effectful.make_bind_or_let m_subject ~evv ~f:(fun y ~evv ->
        let%map m_body = translate_expr e_body ~evv in
        Maybe_effectful.map m_body ~f:(fun m_body ->
          EPS.Expr.Let (Variable x, Pure, y, m_body)))
    | Expl.Expr.Seq (e1, e2) ->
      let%bind e1' = translate_expr e1 ~evv in
      Maybe_effectful.make_bind_or_let e1' ~evv ~f:(fun _x1 ~evv ->
        translate_expr e2 ~evv)
    | Expl.Expr.Impure_built_in impure -> translate_impure_built_in impure ~evv

and translate_value
  : Polar_type.Effect.t Expl.Expr.value -> [ `Pure of EPS.Expr.t ] Generation.t
  =
  let open Generation.Let_syntax in
  function
  | Expl.Expr.Variable v -> `Pure (EPS.Expr.Variable v) |> return
  | Expl.Expr.Literal lit -> `Pure (EPS.Expr.Literal lit) |> return
  | Expl.Expr.Lambda lambda ->
    let%map lambda' = translate_lambda lambda in
    `Pure (EPS.Expr.Lambda lambda')
  | Expl.Expr.Fix_lambda fix_lambda ->
    let%map fix_lambda' = translate_fix_lambda fix_lambda in
    `Pure (EPS.Expr.Fix_lambda fix_lambda')
  | Expl.Expr.Handler h ->
    let%map (`Pure h') = translate_handler h in
    let { Expl.Expr.handled_effect = label; _ } = h in
    let label' : EPS.Expr.t = translate_effect_label label in
    let handler' =
      EPS.Expr.Application
        ( EPS.Expr.Variable Primitive_names.handler
        , [ label', Pure; h', Pure ]
        , Pure )
    in
    `Pure handler'
  | Expl.Expr.Perform
      { Expl.Expr.operation = op_name; performed_effect = label } ->
    let%map selector =
      Generation.make_lambda_expr_1 Pure (fun h ->
        EPS.Expr.Select_operation (label, op_name, h) |> return)
    in
    let label' : EPS.Expr.t = translate_effect_label label in
    let perform' =
      EPS.Expr.Application
        ( EPS.Expr.Variable Primitive_names.perform
        , [ label', Pure; selector, Pure ]
        , Pure )
    in
    `Pure perform'

and translate_lambda
  : Polar_type.Effect.t Expl.Expr.lambda -> EPS.Expr.lambda Generation.t
  =
  fun (ps, e_body) ->
  let open Generation.Let_syntax in
  let%map x_evv, m_body = provide_evv (translate_expr e_body) in
  let ps = ps @ [ Parameter.Variable x_evv ] in
  Maybe_effectful.make_lambda ps m_body

and translate_fix_lambda
  : Polar_type.Effect.t Expl.Expr.fix_lambda -> EPS.Expr.fix_lambda Generation.t
  =
  fun (f, lambda) ->
  let open Generation.Let_syntax in
  let%map m_lambda = translate_lambda lambda in
  f, m_lambda

(** tranlate an operator expression, taking operands as untranslated expressions
    to allow short-circuiting for boolean operators *)
and translate_operator
  :  e_left:Polar_type.Effect.t Expl.Expr.t
  -> Operator.t
  -> e_right:Polar_type.Effect.t Expl.Expr.t
  -> evv:EPS.Expr.t
  -> Maybe_effectful.t Generation.t
  =
  fun ~e_left op ~e_right ~evv ->
  let open Generation.Let_syntax in
  match op with
  | Operator.Int iop ->
    let op = Operator.Int iop in
    let%bind m_left = translate_expr e_left ~evv in
    Maybe_effectful.make_bind_or_let m_left ~evv ~f:(fun x_left ~evv ->
      let%bind m_right = translate_expr e_right ~evv in
      Maybe_effectful.make_map_or_let m_right ~evv ~f:(fun x_right ->
        `Pure (EPS.Expr.Operator (x_left, op, x_right)) |> return))
  | Operator.Bool bop ->
    let%bind m_left = translate_expr e_left ~evv in
    Maybe_effectful.make_bind_or_let m_left ~evv ~f:(fun x_left ~evv ->
      let%map m_right = translate_expr e_right ~evv in
      let wrap_bool b = Maybe_effectful.Pure (EPS.Expr.Literal (Bool b)) in
      match bop with
      | Operator.Bool.Or ->
        (* a || b === a ? Pure True : b *)
        Maybe_effectful.combine
          m_right
          (wrap_bool true)
          ~f:(fun m_right m_true ->
            EPS.Expr.If_then_else (x_left, m_true, m_right))
      | Operator.Bool.And ->
        (* a && b === a ? b : Pure False *)
        Maybe_effectful.combine
          m_right
          (wrap_bool false)
          ~f:(fun m_right m_false ->
            EPS.Expr.If_then_else (x_left, m_right, m_false)))

and translate_handler
  :  Polar_type.Effect.t Expl.Expr.handler
  -> [ `Pure of EPS.Expr.t ] Generation.t
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
  `Pure (EPS.Expr.Construct_handler { handled_effect; operation_clauses })

and translate_op_handler
  :  Operation_shape.t
  -> Polar_type.Effect.t Expl.Expr.op_handler
  -> EPS.Expr.t Generation.t
  =
  fun shape { Expl.Expr.op_argument; op_body } ->
  let open Generation.Let_syntax in
  match shape with
  | Operation_shape.Control ->
    let resume = Keyword.resume in
    let%map x_evv, m_body = provide_evv (translate_expr op_body) in
    let clause =
      Maybe_effectful.make_lambda_expr
        [ op_argument; Parameter.Variable resume; Parameter.Variable x_evv ]
        m_body
    in
    EPS.Expr.Construct_op_normal clause
  | Operation_shape.Fun ->
    let%map x_evv, m_body = provide_evv (translate_expr op_body) in
    let clause =
      Maybe_effectful.make_lambda_expr
        [ op_argument; Parameter.Variable x_evv ]
        m_body
    in
    EPS.Expr.Construct_op_tail clause

and translate_impure_built_in
  :  Polar_type.Effect.t Expl.Expr.impure_built_in
  -> evv:EPS.Expr.t
  -> Maybe_effectful.t Generation.t
  =
  let open Generation.Let_syntax in
  fun built_in ~evv ->
    match built_in with
    | Expl.Expr.Impure_println ->
      Maybe_effectful.Pure (EPS.Expr.Impure_built_in EPS.Expr.Impure_println)
      |> return
    | Expl.Expr.Impure_print_int { value = e; newline } ->
      let%bind e' = translate_expr e ~evv in
      Maybe_effectful.make_map_or_let e' ~evv ~f:(fun x ->
        (* `Pure as in "won't Yield", does actually perform "real" effect! *)
        `Pure
          (EPS.Expr.Impure_built_in
             (EPS.Expr.Impure_print_int { value = x; newline }))
        |> return)
    | Expl.Expr.Impure_read_int ->
      Maybe_effectful.Pure (EPS.Expr.Impure_built_in EPS.Expr.Impure_read_int)
      |> return
;;

let translate_fun_decl
  : Polar_type.Effect.t Expl.Decl.Fun.t -> EPS.Program.Fun_decl.t Generation.t
  =
  fun fix_lambda -> translate_fix_lambda fix_lambda
;;

let translate_effect_decl : Effect_decl.t -> EPS.Program.Effect_decl.t =
  fun { Effect_decl.name; operations } ->
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
      ( EPS.Expr.Variable Keyword.entry_point
      , [ EPS.Expr.Nil_evidence_vector, Pure ]
      , Ctl )
  in
  { EPS.Program.effect_declarations; fun_declarations; entry_expr }
;;

let translate program = translate_program program ~include_prelude:true

let translate_no_prelude program =
  translate_program program ~include_prelude:false
;;

let translate_expr expr ~evv =
  let open Generation.Let_syntax in
  let%map m = translate_expr expr ~evv in
  Maybe_effectful.to_effectful m
;;
