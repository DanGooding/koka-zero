open Core

(** source syntax *)
module Min = Koka_zero_inference.Minimal_syntax

(** target syntax *)
module EPS = Evidence_passing_syntax

(* [`Value e] indicates that expression [e] represents a pure term, which must
   be lifted into the effect monad via [monadic_of_value] *)

(** convert a value (an expression which cannot step, so is total) into a
    monadic value (via `return`), in this case one with zero effects. *)
let monadic_of_value : EPS.Expr.t -> EPS.Expr.t =
 fun v -> EPS.Expr.Application (EPS.Expr.Variable Primitives.Names.pure, [ v ])
;;

let translate_variable (v : Min.Variable.t) : [ `Value of EPS.Variable.t ] =
  `Value v
;;

let translate_literal (l : Min.Literal.t) : [ `Value of EPS.Literal.t ] =
  `Value l
;;

let translate_operator (op : Min.Operator.t) : EPS.Operator.t = op

let translate_unary_operator (op : Min.Operator.Unary.t) : EPS.Operator.Unary.t =
  op
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
  let open Generation.Let_syntax in
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

let rec translate_expr : Min.Expr.t -> EPS.Expr.t Generation.t =
  let open Generation.Let_syntax in
  function
  | Min.Expr.Variable v ->
    (* TODO: translate from a form where we already know what the operations
       are *)
    assert (not (is_operation v));
    let (`Value v') = translate_variable v in
    monadic_of_value (EPS.Expr.Variable v') |> return
  | Min.Expr.Literal lit ->
    let (`Value lit') = translate_literal lit in
    monadic_of_value (EPS.Expr.Literal lit') |> return
  | Min.Expr.Lambda lambda ->
    let%map (`Value m_lambda) = translate_lambda lambda in
    monadic_of_value (EPS.Expr.Lambda m_lambda)
  | Min.Expr.Fix_lambda fix_lambda ->
    let%map (`Value m_fix_lambda) = translate_fix_lambda fix_lambda in
    monadic_of_value (EPS.Expr.Fix_lambda m_fix_lambda)
  | Min.Expr.Application (e_f, e_args) ->
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
  | Min.Expr.Unary_operator (op, e) ->
    let%bind m_e = translate_expr e in
    make_map_into m_e ~f:(fun x ->
        `Value (EPS.Expr.Unary_operator (op, x)) |> return)
  | Min.Expr.Operator (e_left, op, e_right) ->
    (* TODO: note this has no short-circuiting for boolean operators *)
    let%bind m_left = translate_expr e_left in
    make_bind_into m_left ~f:(fun x_left ->
        let%bind m_right = translate_expr e_right in
        make_map_into m_right ~f:(fun x_right ->
            `Value (EPS.Expr.Operator (x_left, op, x_right)) |> return))
  | Min.Expr.If_then_else (e_cond, e_yes, e_no) ->
    let%bind e_cond' = translate_expr e_cond in
    make_bind_into e_cond' ~f:(fun cond ->
        Generation.make_lambda_expr_1 (fun vector ->
            let%bind e_yes' = translate_expr e_yes in
            let%map e_no' = translate_expr e_no in
            EPS.Expr.If_then_else
              ( cond
              , EPS.Expr.Application (e_yes', [ vector ])
              , EPS.Expr.Application (e_no', [ vector ]) )))
  | Min.Expr.Handler h ->
    (* TODO: `Hnd perhaps isn't useful? indicates it is not a first class object
       to the user *)
    let%map (`Hnd h') = translate_handler h in
    let (label : Effect.Label.t) =
      ()
      (* TODO: determined in inference stage *)
    in
    let (label' : EPS.Expr.t) = translate_effect_label label in
    EPS.Expr.Application (EPS.Expr.Variable Primitives.handler, [ label'; h' ])
  | Min.Expr.Perform (op_name, label) ->
    let (`Value op_name') = translate_variable op_name in
    let%map selector =
      Generation.make_lambda_1 (fun h ->
          (* TODO: think more about how to implement `select` *)
          EPS.Expr.Select_operation (label, op_name', h) |> return)
    in
    let label' : EPS.Expr.t = translate_effect_label label in
    EPS.Expr.Application
      (EPS.Expr.Variable Primitives.Names.perform, [ label'; selector ])
  | _ -> failwith "not implemented"

and translate_lambda
    : Min.Expr.lambda -> [ `Value of EPS.Expr.lambda ] Generation.t
  =
 fun (xs, e_body) ->
  let open Generation.Let_syntax in
  let%map m_body = translate_expr e_body in
  let xs' =
    List.map xs ~f:(fun v ->
        let (`Value v') = translate_variable v in
        v')
  in
  `Value (xs', m_body)

and translate_fix_lambda
    : Min.Expr.fix_lambda -> [ `Value of EPS.Expr.fix_lambda ] Generation.t
  =
 fun (f, lambda) ->
  let open Generation.Let_syntax in
  let%map (`Value m_lambda) = translate_lambda lambda in
  let (`Value f') = translate_variable f in
  `Value (f', m_lambda)

and translate_handler : Min.Expr.handler -> [ `Hnd of EPS.Expr.t ] Generation.t =
 fun handler ->
  let { Min.Expr.operations; return_clause } = handler in
  failwith "not implemented"
;;

let translate { Min.Program.declarations; has_main } =
  failwith "not implemented"
;;
