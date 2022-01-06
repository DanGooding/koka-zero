open Core
module Min = Koka_zero_inference.Minimal_syntax
module Mon = Monadic_syntax

(* TODO: Generation functions don't add the `Value|`Redex markers... *)
(* TODO: name `Redex is inaccurate, switch to `Monadic *)

(* TODO: remember: `Value|`Redex describe Minimal_syntax in an ad-hoc way.
   `Pure|`Monadic|`NA describe Monadic_syntax in a more complex (type system
   based) way *)

(** convert a value (an expression which cannot step, so is total) into a
    monadic value (via `return`), in this case one with zero effects. *)
let redex_of_value : Mon.Expr.t -> [ `Redex of Mon.Expr.t ] =
 fun v ->
  `Redex (Mon.Expr.Application (Mon.Expr.Variable Primitives.Names.pure, [ v ]))
;;

(** raise [`Value]s into the monad (making them [`Redex]es) *)
let redex_of_expr
    :  [ `Value of Mon.Expr.t | `Redex of Mon.Expr.t ]
    -> [ `Redex of Mon.Expr.t ]
  = function
  | `Redex e -> `Redex e
  | `Value v -> redex_of_value v
;;

let translate_variable (v : Min.Variable.t) : Mon.Variable.t = v
let translate_literal (l : Min.Literal.t) : Mon.Literal.t = l
let translate_operator (op : Min.Operator.t) : Mon.Operator.t = op

let translate_unary_operator (op : Min.Operator.Unary.t) : Mon.Operator.Unary.t =
  op
;;

(** [make_bind_into e ~f:(fun x -> e')] builds the expression `e >>= \x -> e'` *)
let make_bind_into
    :  [ `Redex of Mon.Expr.t ]
    -> f:(Mon.Expr.t -> [ `Redex of Mon.Expr.t ] Generation.t)
    -> [ `Redex of Mon.Expr.t ] Generation.t
  =
 fun (`Redex e) ~f ->
  let open Generation.Let_syntax in
  let%map rhs =
    Generation.make_lambda_expr_1 (fun x ->
        let%map (`Redex y) = f x in
        y)
  in
  `Redex
    (Mon.Expr.Application (Mon.Expr.Variable Primitives.Names.bind, [ e; rhs ]))
;;

let make_map_into
    :  [ `Redex of Mon.Expr.t ]
    -> f:(Mon.Expr.t -> [ `Value of Mon.Expr.t ] Generation.t)
    -> [ `Redex of Mon.Expr.t ] Generation.t
  =
 fun (`Redex e) ~f ->
  let open Generation.Let_syntax in
  let%map rhs =
    Generation.make_lambda_expr_1 (fun x ->
        let%map (`Value y) = f x in
        let (`Redex m_y) = redex_of_value y in
        m_y)
  in
  `Redex
    (Mon.Expr.Application (Mon.Expr.Variable Primitives.Names.bind, [ e; rhs ]))
;;

(** [make_bind_many_into \[e1;...;en\] ~f:(fun xs -> e')] builds the expression
    `e1 >>= \x1 -> e2 >>= \x2 -> ... en >>= \xn -> e'` *)
let make_bind_many_into
    :  [ `Redex of Mon.Expr.t ] list
    -> f:(Mon.Expr.t list -> [ `Redex of Mon.Expr.t ] Generation.t)
    -> [ `Redex of Mon.Expr.t ] Generation.t
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

let rec translate_expr : Min.Expr.t -> [ `Redex of Mon.Expr.t ] Generation.t =
  let open Generation.Let_syntax in
  function
  | Min.Expr.Variable v ->
    (* TODO: translate from a form where we already know what the operations
       are *)
    assert (not (is_operation v));
    let v' = translate_variable v in
    redex_of_value (Mon.Variable v') |> return
  | Min.Expr.Lambda lambda ->
    let%map (`Value m_lambda) = translate_lambda lambda in
    redex_of_value (Mon.Expr.Lambda m_lambda)
  | Min.Expr.Fix_lambda fix_lambda ->
    let%map (`Value m_fix_lambda) = translate_fix_lambda fix_lambda in
    redex_of_value (Mon.Expr.Fix_lambda m_fix_lambda)
  | Min.Expr.Application (e_f, e_args) ->
    (* `e_f(e_1, e_2, ..., e_n)` translates to: *)
    (* `e_f >>= (\f. e_1 >>= (\x1. e_2 >>= (\x2. ... e_n >>= (\xn. f(x1, x2,
       ..., xn) ))))` *)
    let%bind m_f = translate_expr e_f in
    let%bind (m_args : [ `Redex of Mon.Expr.t ] list) =
      List.map e_args ~f:(fun e_arg -> translate_expr e_arg) |> Generation.all
    in
    make_bind_into m_f ~f:(fun f ->
        make_bind_many_into m_args ~f:(fun xs ->
            `Redex (Mon.Expr.Application (f, xs)) |> return))
  | Min.Expr.Unary_operator (op, e) ->
    let%bind (`Redex m_e) = translate_expr e in
    let%map v_apply_op
        (* TODO: again, the Generation functions are not helping here! should
           they be polymorphic? *)
      =
      Generation.make_lambda_1 (fun x ->
          let (`Redex e_app) =
            redex_of_value (Mon.Expr.Unary_operator (op, x))
          in
          return e_app)
    in
    `Redex
      (Mon.Expr.Application
         (Mon.Expr.Variable Primitives.Names.bind, [ m_e; m_apply_op ]))
  | Min.Expr.Operator (e_left, op, e_right) ->
    let%bind m_left = translate_expr e_left in
    make_bind_into m_left ~f:(fun x_left ->
        let%bind m_right = translate_expr e_right in
        make_bind_into m_right ~f:(fun x_right ->
            Mon.Expr.Operator (x_left, op, x_right)
            |> `Value
            |> inject_value
            |> return))
  | _ -> failwith "not implemented"

and translate_lambda
    : Min.Expr.lambda -> [ `Value of Mon.Expr.lambda ] Generation.t
  =
 fun (xs, e_body) ->
  let open Generation.Let_syntax in
  let%map (`Redex m_body) = translate_expr e_body in
  let xs' = List.map xs ~f:translate_variable in
  `Value (xs', m_body)

and translate_fix_lambda
    : Min.Expr.fix_lambda -> [ `Value of Mon.Expr.fix_lambda ] Generation.t
  =
 fun (f, lambda) ->
  let open Generation.Let_syntax in
  let%map (`Value m_lambda) = translate_lambda lambda in
  let f' = translate_variable f in
  `Value (f', m_lambda)

and translate_handler : Min.Expr.handler -> Mon.Expr.t =
 fun handler ->
  let { Min.Expr.operations; return_clause } = handler in
  failwith "not implemented"
;;

let translate { Min.Program.declarations; has_main } =
  failwith "not implemented"
;;
