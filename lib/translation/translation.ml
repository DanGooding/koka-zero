open Core
open Koka_zero_inference

(* TODO: this is terrible naming *)
module From = Minimal_syntax
module To = Monadic_syntax



let translate_variable v = From.Variable.to_string v |> To.Variable.of_string

let translate_literal l = l

(* TODO: probably need to work on annotated versions and track effects *)

(*  |-val *)

(* TODO: this is difficult to distinguish, but a value is an
   expression which cannot step (so has no effect - i.e. inference
   freely assumes any effect) *)

(** raise values into the monad, but not anything already in it *)
let inject_value : To.Expr.t Value_or_reducible.t -> To.Expr.t =
  function
  | Reducible e -> e
  | Value v -> To.Expr.Application(pure, v)

let rec translate_expr : From.Expr.t -> To.Expr.t Value_or_reducible.t = function
  | From.Lambda (v, e_body) ->
    let m_body = translate_expr in
    let v' = translate_variable v in
    Value ( To.Lambda (v', m_body) )

  | From.Application (e_f, e_arg) ->
    (* `e_f' >>= (\f. e_arg' >>= f)` *)
    let m_f = translate_expr e_f |> inject_value in
    let m_arg = translate_expr e_arg |> inject_value in
    (* TODO: this doesn't need to be globally unique it just mustn't clash with
       operation names... - maybe a separate namespace for generated names? *)
    let param_f = To.fresh_variable () in
    Reducible (
      To.Bind (m_f, To.Lambda (param_f, To.Bind (m_arg, To.Variable param_f)))
    )


   (* TODO: also need 'fail if translation is not a value' *)


  | _ -> failwith "not implemented";

(* TODO: use polymorphic variants? *)

and translate_value : From.Expr.t -> To.Expr.t = function
  (* TODO: if this is an operation - add a `perform` *)
  | From.Variable v ->
    (match lookup_operation v with
    | None -> To.Variable (translate_variable v)
    | Some label -> To.Perform)

  (* TODO: perhaps `handler` is the better primitive? *)
  (* TODO: `handle h e` is not a value, `handler h` _is_ *)
  (* | From.Handle (handler, e_subject) -> failwith "not implemented" *)
  | From.Handler (handler) -> failwith "not implemented"

  | From.Literal l -> To.Literal (translate_literal l)

  | 

  (* `fix f. e` is a value (a function) *)
  | From.Fix (v, e_body) -> failwith "not implemented"


(* |- *)
and translate_reducible : From.Expr.t -> To.Expr.t = function
  | From.Application (e_f, e_arg) ->
    (* `e_f' >>= (\f. e_arg' >>= f)` *)
    let m_f = translate_expr e_f in
    let m_arg = translate_expr e_arg in
    (* TODO: this doesn't need to be globally unique it just mustn't clash with
       operation names... - maybe a separate namespace for generated names? *)
    let param_f = To.fresh_variable () in
    To.Bind (m_f, To.Lambda (param_f, To.Bind (m_arg, To.Variable param_f)))

  | From.Lambda (v, e_body) ->
    let m_body = translate_expr in
    let v' = translate_variable v in
    To.Lambda (v', m_body)

  (* TODO: Let, Fix are not given translations *)
  | From.Let (v, e_subject, e_body) -> failwith "not implemented"

and translate_handler : From.Expr.handler -> To.Expr.t =
 fun handler ->
  let { From.Expr.operations; return_clause } = handler in
  failwith "not implemented"
;;

let translate { Minimal_syntax.Program.effect_declarations; body } =
  failwith "not implemented"
;;
