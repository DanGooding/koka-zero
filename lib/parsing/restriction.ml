open Core
open Koka_zero_util
open Koka_zero_inference

(** assert that the argument is [None], otherwise returns an unsupported syntax
    error with the given [description] *)
let restrict_to_none ~description = function
  | None -> Result.Ok ()
  | Some _ -> Static_error.unsupported_syntax description |> Result.Error
;;

(** assert that the argument is [ \[\] ], otherwise returns an unsupported
    syntax error with the given [message] *)
let restrict_to_empty ~description = function
  | [] -> Result.Ok ()
  | _ :: _ -> Static_error.unsupported_syntax description |> Result.Error
;;

(* TODO: make sure actual syntax errors aren't reported as unsupported syntax *)

let var_id_to_minimal_syntax (_x : Syntax.Var_id.t) : Minimal_syntax.Variable.t =
  (* TODO: do I need to send this to a variant to allow safe generation of fresh
     dummy names *)
  failwith "not implemented"
;;

let identifier_to_minimal_syntax (x : Syntax.Identifier.t)
    : Minimal_syntax.Variable.t
  =
  match x with
  | Syntax.Identifier.Var x -> var_id_to_minimal_syntax x
;;

let pattern_to_minimal_syntax
    : Syntax.pattern -> Minimal_syntax.Variable.t Or_static_error.t
  = function
  | Syntax.Pattern_id x -> identifier_to_minimal_syntax x |> Result.Ok
  | Syntax.Pattern_wildcard -> failwith "not implemented"
;;

let pattern_parameter_to_minimal_syntax { Syntax.pattern; type_ }
    : Minimal_syntax.Variable.t Or_static_error.t
  =
  let open Result.Let_syntax in
  let%bind () =
    restrict_to_none type_ ~description:"type annotation on parameter"
  in
  pattern_to_minimal_syntax pattern
;;

let binary_operator_to_minimal_syntax (op : Syntax.binary_operator)
    : Minimal_syntax.Operator.t Or_static_error.t
  =
  match op with
  | Syntax.Plus -> Result.Ok Minimal_syntax.Operator.(Int Int.Plus)
  | Syntax.Minus -> Result.Ok Minimal_syntax.Operator.(Int Int.Minus)
  | Syntax.Times -> Result.Ok Minimal_syntax.Operator.(Int Int.Times)
  | Syntax.Divide -> Result.Ok Minimal_syntax.Operator.(Int Int.Divide)
  | Syntax.Modulo -> Result.Ok Minimal_syntax.Operator.(Int Int.Modulo)
  | Syntax.And -> Result.Ok Minimal_syntax.Operator.(Bool Bool.And)
  | Syntax.Or -> Result.Ok Minimal_syntax.Operator.(Bool Bool.Or)
  (* to avoid worrying about overloading, only supporting integer equality tests
     for now *)
  | Syntax.Equals -> Result.Ok Minimal_syntax.Operator.(Int Int.Equals)
  | Syntax.Not_equal -> Result.Ok Minimal_syntax.Operator.(Int Int.Not_equal)
  | Syntax.Less_than -> Result.Ok Minimal_syntax.Operator.(Int Int.Less_than)
  | Syntax.Less_equal -> Result.Ok Minimal_syntax.Operator.(Int Int.Less_equal)
  | Syntax.Greater_than ->
    Result.Ok Minimal_syntax.Operator.(Int Int.Greater_than)
  | Syntax.Greater_equal ->
    Result.Ok Minimal_syntax.Operator.(Int Int.Greater_equal)
;;

let unary_operator_to_minimal_syntax (op : Syntax.unary_operator)
    : Minimal_syntax.Operator.Unary.t Or_static_error.t
  =
  match op with
  | Syntax.Exclamation ->
    Result.Ok Minimal_syntax.Operator.(Unary.Bool Bool.Unary.Not)
;;

let rec expr_to_minimal_syntax (e : Syntax.expr)
    : Minimal_syntax.Expr.t Or_static_error.t
  =
  let open Result.Let_syntax in
  match e with
  | Syntax.Return _ -> Static_error.unsupported_syntax "return" |> Result.Error
  | Syntax.If_then_else (e_cond, block_yes, block_no) ->
    let%bind e_cond' = expr_to_minimal_syntax e_cond in
    let%bind block_yes' = block_to_minimal_syntax block_yes in
    let%map block_no' = block_to_minimal_syntax block_no in
    Minimal_syntax.Expr.If_then_else (e_cond', block_yes', block_no')
  | Syntax.If_then (e_cond, block_yes) ->
    (* TODO: this is desugaring mixed in *)
    let block_no = Syntax.singleton_block (Syntax.Literal Syntax.Unit) in
    Syntax.If_then_else (e_cond, block_yes, block_no) |> expr_to_minimal_syntax
  | Syntax.Handler handler ->
    let%map handler' = effect_handler_to_minimal_syntax handler in
    Minimal_syntax.Expr.Handler handler'
  | Syntax.Handle { subject; handler } ->
    (* TODO: more desugaring here... *)
    let%bind handler' = expr_to_minimal_syntax (Syntax.Handler handler) in
    let%map subject' = expr_to_minimal_syntax subject in
    Minimal_syntax.Expr.Application (handler', [ subject' ])
  | Syntax.Fn f -> fn_to_minimal_syntax f
  | Syntax.Binary_op (e_left, op, e_right) ->
    let%bind e_left' = expr_to_minimal_syntax e_left in
    let%bind e_right' = expr_to_minimal_syntax e_right in
    let%map op' = binary_operator_to_minimal_syntax op in
    Minimal_syntax.Expr.Operator (e_left', op', e_right')
  | Syntax.Unary_op (op, e) ->
    let%bind e' = expr_to_minimal_syntax e in
    let%map op' = unary_operator_to_minimal_syntax op in
    Minimal_syntax.Expr.Unary_operator (op', e')
  | _ -> failwith "not implemented"

and block_to_minimal_syntax { Syntax.statements = _; last = _ }
    : Minimal_syntax.Expr.t Or_static_error.t
  =
  failwith "not implemented"

and fn_to_minimal_syntax
    { Syntax.type_parameters; parameters; result_type; body }
    : Minimal_syntax.Expr.t Or_static_error.t
  =
  let open Result.Let_syntax in
  let%bind () =
    restrict_to_empty type_parameters ~description:"type parameter annotations"
  in
  let%bind () =
    restrict_to_none result_type ~description:"return type annotation"
  in
  let%bind (parameters : Minimal_syntax.Variable.t list) =
    List.map ~f:pattern_parameter_to_minimal_syntax parameters |> Result.all
  in
  let%map body' = block_to_minimal_syntax body in
  Minimal_syntax.Expr.Lambda (parameters, body')

and effect_handler_to_minimal_syntax (Syntax.Effect_handler _op_handlers)
    : Minimal_syntax.Expr.handler Or_static_error.t
  =
  (* TODO: 'forget' that fun/val are special cases, implement all using
     resume *)
  failwith "not implemented"
;;

let program_to_minimal_syntax (_program : Syntax.program)
    : Minimal_syntax.Program.t Or_static_error.t
  =
  failwith "not implemented"
;;
