open Core
open Koka_zero_util
open Koka_zero_inference
module Min = Minimal_syntax

(** assert that the argument is [None], otherwise returns an 'unsupported
    syntax' error with the given [description] *)
let restrict_to_none ~description = function
  | None -> Result.Ok ()
  | Some _ -> Static_error.unsupported_syntax description |> Result.Error
;;

(** assert that the argument is [ \[\] ], otherwise returns an 'unsupported
    syntax' error with the given [message] *)
let restrict_to_empty ~description = function
  | [] -> Result.Ok ()
  | _ :: _ -> Static_error.unsupported_syntax description |> Result.Error
;;

let restrict_to_singleton ~description = function
  | [ x ] -> Result.Ok x
  | [] | _ :: _ -> Static_error.unsupported_syntax description |> Result.Error
;;

(* TODO: make sure actual syntax errors aren't reported as unsupported syntax *)

let type_to_minimal_syntax (_t : Syntax.type_) : Type.Mono.t Or_static_error.t =
  failwith "not implemented"
;;

let var_id_to_minimal_syntax (_x : Syntax.Var_id.t) : Min.Variable.t =
  (* TODO: do I need to send this to a variant to allow safe generation of fresh
     dummy names *)
  failwith "not implemented"
;;

let var_id_to_minimal_syntax_effect_label (_x : Syntax.Var_id.t)
    : Effect.Label.t
  =
  failwith "not implemented"
;;

let identifier_to_minimal_syntax (x : Syntax.Identifier.t) : Min.Variable.t =
  match x with
  | Syntax.Identifier.Var x -> var_id_to_minimal_syntax x
;;

let literal_to_minimal_syntax (lit : Syntax.literal) : Min.Literal.t =
  match lit with
  | Syntax.Unit -> Min.Literal.Unit
  | Syntax.Int i -> Min.Literal.Int i
  | Syntax.Bool b -> Min.Literal.Bool b
;;

let parameter_id_to_minimal_syntax
    : Syntax.parameter_id -> Min.Variable.t Or_static_error.t
  = function
  | Syntax.Parameter_id x -> identifier_to_minimal_syntax x |> Result.Ok
  | Syntax.Parameter_wildcard -> failwith "not implemented"
;;

let parameter_to_minimal_syntax
    : Syntax.parameter -> (Min.Variable.t * Type.Mono.t) Or_static_error.t
  =
 fun { Syntax.id; type_ } ->
  let open Result.Let_syntax in
  let%bind x = parameter_id_to_minimal_syntax id in
  let%map t = type_to_minimal_syntax type_ in
  x, t
;;

let pattern_to_minimal_syntax
    : Syntax.pattern -> Min.Variable.t Or_static_error.t
  = function
  | Syntax.Pattern_id x -> identifier_to_minimal_syntax x |> Result.Ok
  | Syntax.Pattern_wildcard -> failwith "not implemented"
;;

let pattern_parameter_to_minimal_syntax { Syntax.pattern; type_ }
    : (Min.Variable.t * Type.Mono.t option) Or_static_error.t
  =
  let open Result.Let_syntax in
  let%bind x = pattern_to_minimal_syntax pattern in
  let%map type_' =
    Option.map type_ ~f:type_to_minimal_syntax
    |> Static_error.interchange_option
  in
  x, type_'
;;

let operation_parameter_to_minimal_syntax
    :  Syntax.operation_parameter
    -> (Min.Variable.t * Type.Mono.t option) Or_static_error.t
  =
 fun { Syntax.id; type_ } ->
  let open Result.Let_syntax in
  let%bind id' = parameter_id_to_minimal_syntax id in
  let%map type_' =
    Option.map type_ ~f:type_to_minimal_syntax
    |> Static_error.interchange_option
  in
  id', type_'
;;

let binary_operator_to_minimal_syntax (op : Syntax.binary_operator)
    : Min.Operator.t Or_static_error.t
  =
  match op with
  | Syntax.Plus -> Result.Ok Min.Operator.(Int Int.Plus)
  | Syntax.Minus -> Result.Ok Min.Operator.(Int Int.Minus)
  | Syntax.Times -> Result.Ok Min.Operator.(Int Int.Times)
  | Syntax.Divide -> Result.Ok Min.Operator.(Int Int.Divide)
  | Syntax.Modulo -> Result.Ok Min.Operator.(Int Int.Modulo)
  | Syntax.And -> Result.Ok Min.Operator.(Bool Bool.And)
  | Syntax.Or -> Result.Ok Min.Operator.(Bool Bool.Or)
  (* to avoid worrying about overloading, only supporting integer equality tests
     for now *)
  | Syntax.Equals -> Result.Ok Min.Operator.(Int Int.Equals)
  | Syntax.Not_equal -> Result.Ok Min.Operator.(Int Int.Not_equal)
  | Syntax.Less_than -> Result.Ok Min.Operator.(Int Int.Less_than)
  | Syntax.Less_equal -> Result.Ok Min.Operator.(Int Int.Less_equal)
  | Syntax.Greater_than -> Result.Ok Min.Operator.(Int Int.Greater_than)
  | Syntax.Greater_equal -> Result.Ok Min.Operator.(Int Int.Greater_equal)
;;

let unary_operator_to_minimal_syntax (op : Syntax.unary_operator)
    : Min.Operator.Unary.t Or_static_error.t
  =
  match op with
  | Syntax.Exclamation -> Result.Ok Min.Operator.(Unary.Bool Bool.Unary.Not)
;;

let rec expr_to_minimal_syntax (e : Syntax.expr) : Min.Expr.t Or_static_error.t =
  let open Result.Let_syntax in
  match e with
  | Syntax.Return _ -> Static_error.unsupported_syntax "return" |> Result.Error
  | Syntax.If_then_else (e_cond, block_yes, block_no) ->
    let%bind e_cond' = expr_to_minimal_syntax e_cond in
    let%bind block_yes' = block_to_minimal_syntax block_yes in
    let%map block_no' = block_to_minimal_syntax block_no in
    Min.Expr.If_then_else (e_cond', block_yes', block_no')
  | Syntax.If_then (e_cond, block_yes) ->
    (* TODO: this is desugaring mixed in *)
    let block_no = Syntax.singleton_block (Syntax.Literal Syntax.Unit) in
    Syntax.If_then_else (e_cond, block_yes, block_no) |> expr_to_minimal_syntax
  | Syntax.Handler handler ->
    let%map handler' = effect_handler_to_minimal_syntax handler in
    Min.Expr.Handler handler'
  | Syntax.Handle { subject; handler } ->
    (* TODO: more desugaring here... *)
    let%bind handler' = expr_to_minimal_syntax (Syntax.Handler handler) in
    let%map subject' = expr_to_minimal_syntax subject in
    Min.Expr.Application (handler', [ subject' ])
  | Syntax.Fn f -> fn_to_minimal_syntax f
  | Syntax.Application (e_f, e_args) ->
    let%bind e_f' = expr_to_minimal_syntax e_f in
    let%map e_args' = List.map e_args ~f:expr_to_minimal_syntax |> Result.all in
    Min.Expr.Application (e_f', e_args')
  | Syntax.Identifier x ->
    let x' = identifier_to_minimal_syntax x in
    Min.Expr.Variable x' |> Result.Ok
  | Syntax.Literal lit ->
    let lit' = literal_to_minimal_syntax lit in
    Min.Expr.Literal lit' |> Result.Ok
  | Syntax.Binary_op (e_left, op, e_right) ->
    let%bind e_left' = expr_to_minimal_syntax e_left in
    let%bind e_right' = expr_to_minimal_syntax e_right in
    let%map op' = binary_operator_to_minimal_syntax op in
    Min.Expr.Operator (e_left', op', e_right')
  | Syntax.Unary_op (op, e) ->
    let%bind e' = expr_to_minimal_syntax e in
    let%map op' = unary_operator_to_minimal_syntax op in
    Min.Expr.Unary_operator (op', e')
  | Syntax.Annotated (_e, _scheme) ->
    Static_error.unsupported_syntax "type annotation on expression"
    |> Result.Error

and block_to_minimal_syntax { Syntax.statements = _; last = _ }
    : Min.Expr.t Or_static_error.t
  =
  failwith "not implemented"

and fn_to_minimal_syntax
    { Syntax.type_parameters; parameters; result_type; body }
    : Min.Expr.t Or_static_error.t
  =
  let open Result.Let_syntax in
  let%bind () =
    restrict_to_empty type_parameters ~description:"type parameter annotations"
  in
  let%bind () =
    restrict_to_none result_type ~description:"return type annotation"
  in
  let%bind parameters =
    List.map ~f:pattern_parameter_to_minimal_syntax parameters |> Result.all
  in
  let names, types = List.unzip parameters in
  let%bind () =
    restrict_to_empty
      (List.filter_opt types)
      ~description:"type annotations on parameters"
  in
  let%map body' = block_to_minimal_syntax body in
  Min.Expr.Lambda (names, body')

and effect_handler_to_minimal_syntax (Syntax.Effect_handler op_handlers)
    : Min.Expr.handler Or_static_error.t
  =
  failwith "not implemented"

and operation_handler_to_minimal_syntax
    :  Syntax.operation_handler
    -> ([ `Op of Min.Variable.t | `Return ] * Min.Expr.op_handler)
       Or_static_error.t
  =
  let open Result.Let_syntax in
  function
  | Syntax.Op_control { id; parameters; body } ->
    let id' = var_id_to_minimal_syntax id in
    let%bind parameters' =
      List.map parameters ~f:operation_parameter_to_minimal_syntax |> Result.all
    in
    let names, types = List.unzip parameters' in
    let%bind op_argument =
      restrict_to_singleton
        names
        ~description:"operation can only have exactly one argument"
    in
    let%bind () =
      restrict_to_empty
        (List.filter_opt types)
        ~description:"type annotations on operation handler parameters"
    in
    let%map op_body = block_to_minimal_syntax body in
    `Op id', { Min.Expr.op_argument; op_body }
  (* TODO: this also loses the type safety (shouldn't be able to have a
     `control` handler for a `fun` effect) *)
  | Syntax.Op_fun { id; parameters; body } ->
    let { Syntax.statements; last } = body in
    let last =
      Syntax.Application (Syntax.Identifier Syntax.resume_keyword, [ last ])
    in
    let body = { Syntax.statements; last } in
    let as_control = Syntax.Op_control { id; parameters; body } in
    operation_handler_to_minimal_syntax as_control
  | Syntax.Op_except { id = _; parameters = _; body = _ } ->
    (* TODO: doesn't get anying without proper handling of `resume` and use of
       the knowledge for better implementations *)
    Static_error.unsupported_syntax "`execption` effect" |> Result.Error
  | Syntax.Op_val { id = _; type_ = _; value = _ } ->
    (* need to first evaluate `value`, then capture it in the handler (fairly
       simple, but requires creating a fresh variable name) *)
    Static_error.unsupported_syntax "`val` effect" |> Result.Error
  | Syntax.Op_return { parameter; body } ->
    let%bind op_argument, type_ =
      operation_parameter_to_minimal_syntax parameter
    in
    let%bind () =
      restrict_to_none
        type_
        ~description:"type annotation on operation handler parameter"
    in
    let%map op_body = block_to_minimal_syntax body in
    `Return, { Min.Expr.op_argument; op_body }
;;

let operation_declaration_to_minimal_syntax
    { Syntax.id; type_parameters; shape }
    : (Min.Variable.t * Min.Effect_decl.Operation.t) Or_static_error.t
  =
  let open Result.Let_syntax in
  match shape with
  | Syntax.Shape_fun (_, _) | Syntax.Shape_except (_, _) | Syntax.Shape_val _ ->
    (* TODO: keep hander shapes, and check they match the effect, and the body
       (use of `resume`) *)
    Static_error.unsupported_syntax "non `control` effect" |> Result.Error
  | Syntax.Shape_control (parameters, t_answer) ->
    let id' = var_id_to_minimal_syntax id in
    let%bind () =
      restrict_to_empty
        type_parameters
        ~description:"type parameters for operation handler"
    in
    let%bind parameters' =
      List.map parameters ~f:parameter_to_minimal_syntax |> Result.all
    in
    let _names, t_parameters = List.unzip parameters' in
    let%bind t_parameter =
      restrict_to_singleton
        t_parameters
        ~description:"operation can ony have exactly one argument"
    in
    let%map t_answer' = type_to_minimal_syntax t_answer in
    ( id'
    , { Min.Effect_decl.Operation.argument = t_parameter; answer = t_answer' } )
;;

let effect_declaration_to_minimal_syntax
    { Syntax.id; type_parameters; kind; operations }
    : Min.Effect_decl.t Or_static_error.t
  =
  let open Result.Let_syntax in
  let name = var_id_to_minimal_syntax_effect_label id in
  let%bind () =
    restrict_to_empty type_parameters ~description:"type parameters for effect"
  in
  (* TODO: effect would never have outer kind annotation? *)
  let%bind () =
    restrict_to_none kind ~description:"kind annotation for effect"
  in
  let%bind operations' =
    List.map operations ~f:operation_declaration_to_minimal_syntax |> Result.all
  in
  let%map operations' =
    match Min.Variable.Map.of_alist operations' with
    | `Duplicate_key op_name ->
      let message =
        sprintf
          "duplicate operation name %s in effect %s"
          (Min.Variable.to_string op_name)
          (Effect.Label.to_string name)
      in
      Static_error.syntax_error message |> Result.Error
    | `Ok operations' -> Result.Ok operations'
  in
  { Min.Effect_decl.name; operations = operations' }
;;

let program_to_minimal_syntax (_program : Syntax.program)
    : Min.Program.t Or_static_error.t
  =
  failwith "not implemented"
;;
