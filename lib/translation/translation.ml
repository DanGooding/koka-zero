open Core
open Koka_zero_inference

(* TODO: this is terrible naming *)
module Min = Minimal_syntax
module Mon = Monadic_syntax



let translate_variable v = v

let translate_literal l = l

(* TODO: probably need to work on annotated versions and track effects *)

(*  |-val *)

(* TODO: this is difficult to distinguish, but a value is an
   expression which cannot step (so has no effect - i.e. inference
   freely assumes any effect) *)

(** raise values into the monad, but not anything already in it *)
let inject_value : Mon.Expr.t Value_or_reducible.t -> Mon.Expr.t =
  function
  | Reducible e -> e
  | Value v -> Mon.Expr.Application(pure, v)

let rec translate_expr : Min.Expr.t -> Mon.Expr.t Value_or_reducible.t = function
  | Min.Lambda (v, e_body) ->
    let m_body = translate_expr in
    let v' = translate_variable v in
    Value ( Mon.Lambda (v', m_body) )

  | Min.Application (e_f, e_arg) ->
    (* `e_f' >>= (\f. e_arg' >>= f)` *)
    let m_f = translate_expr e_f |> inject_value in
    let m_arg = translate_expr e_arg |> inject_value in
    (* TODO: this doesn't need to be globally unique it just mustn't clash with
       operation names... - maybe a separate namespace for generated names? *)
    let param_f = Mon.fresh_variable () in
    Reducible (
      Mon.Bind (m_f, Mon.Lambda (param_f, Mon.Bind (m_arg, Mon.Variable param_f)))
    )


   (* TODO: also need 'fail if translation is not a value' *)


  | _ -> failwith "not implemented";

(* TODO: use polymorphic variants? *)

and translate_value : Min.Expr.t -> Mon.Expr.t = function
  (* TODO: if this is an operation - add a `perform` *)
  | Min.Variable v ->
    (match lookup_operation v with
    | None -> Mon.Variable (translate_variable v)
    | Some label -> Mon.Perform)

  (* TODO: perhaps `handler` is the better primitive? *)
  (* TODO: `handle h e` is not a value, `handler h` _is_ *)
  (* | Min.Handle (handler, e_subject) -> failwith "not implemented" *)
  | Min.Handler (handler) -> failwith "not implemented"

  | Min.Literal l -> Mon.Literal (translate_literal l)

  | 

  (* `fix f. e` is a value (a function) *)
  | Min.Fix (v, e_body) -> failwith "not implemented"


(* |- *)
and translate_reducible : Min.Expr.t -> Mon.Expr.t = function
  | Min.Application (e_f, e_arg) ->
    (* `e_f' >>= (\f. e_arg' >>= f)` *)
    let m_f = translate_expr e_f in
    let m_arg = translate_expr e_arg in
    (* TODO: this doesn't need to be globally unique it just mustn't clash with
       operation names... - maybe a separate namespace for generated names? *)
    let param_f = Mon.fresh_variable () in
    Mon.Bind (m_f, Mon.Lambda (param_f, Mon.Bind (m_arg, Mon.Variable param_f)))

  | Min.Lambda (v, e_body) ->
    let m_body = translate_expr in
    let v' = translate_variable v in
    Mon.Lambda (v', m_body)

  (* TODO: Let, Fix are not given translations *)
  | Min.Let (v, e_subject, e_body) -> failwith "not implemented"

and translate_handler : Min.Expr.handler -> Mon.Expr.t =
 fun handler ->
  let { Min.Expr.operations; return_clause } = handler in
  failwith "not implemented"
;;

let translate { Minimal_syntax.Program.effect_declarations; body } =
  failwith "not implemented"
;;
