open Core
open Koka_zero_util
open Koka_zero_inference
module Min = Minimal_syntax
module Syntax = Koka_zero_ir_full_syntax.Syntax

(** assert that the argument is [None], otherwise returns an 'unsupported
    syntax' error with the given [description] *)
let restrict_to_none ~description = function
  | None -> Result.Ok ()
  | Some _ -> Static_error.unsupported_feature description |> Result.Error
;;

(** assert that the argument is [ [] ], otherwise returns an 'unsupported
    syntax' error with the given [message] *)
let restrict_to_empty ~description = function
  | [] -> Result.Ok ()
  | _ :: _ -> Static_error.unsupported_feature description |> Result.Error
;;

let restrict_to_singleton ~description = function
  | [ x ] -> Result.Ok x
  | [] | _ :: _ -> Static_error.unsupported_feature description |> Result.Error
;;

let restrict_to_all_none ~description xs =
  restrict_to_empty (List.filter_opt xs) ~description
;;

(** convert a [Var_id.t] to the [Minimal_syntax] equivalent *)
let simplify_var_id (x : Syntax.Var_id.t) : Variable.t =
  Variable.of_user (Syntax.Var_id.to_string x)
;;

(** convert a [Var_id.t] representing an effect label to the [Minimal_syntax]
    equivalent *)
let simplify_var_id_to_effect_label (x : Syntax.Var_id.t) : Effect.Label.t =
  Effect.Label.of_string (Syntax.Var_id.to_string x)
;;

(** convert an [Identifier.t] to the [Minimal_syntax] equivalent *)
let simplify_identifier (x : Syntax.Identifier.t) : Variable.t =
  match x with
  | Syntax.Identifier.Var x -> simplify_var_id x
;;

(** convert a [Syntax.type_], to a [Type.Mono.t] failing if it is actually an
    [Effect.t] or [Effect.Label.t], or if it doesn't represent a valid
    [Type.Mono.t] *)
let rec simplify_type_as_type : Syntax.type_ -> Type.Mono.t Or_static_error.t =
  fun t ->
  let open Result.Let_syntax in
  match t with
  | Syntax.Arrow (Syntax.Parameters_or_tuple ps, result) ->
    let%bind ps = List.map ps ~f:simplify_parameter_type |> Result.all in
    let%map effect_, t_result = simplify_type_result result in
    let parameter_names, t_args = List.unzip ps in
    ignore (parameter_names : Variable.t option list);
    Type.Mono.Arrow (t_args, effect_, t_result)
  | Syntax.Arrow (t_arg, result) ->
    let%bind t_arg = simplify_type_as_type t_arg in
    let%map effect_, t_result = simplify_type_result result in
    Type.Mono.Arrow ([ t_arg ], effect_, t_result)
  | Syntax.Parameters_or_tuple ps ->
    let%bind ps = List.map ps ~f:simplify_parameter_type |> Result.all in
    let parameter_names, ts = List.unzip ps in
    if List.exists parameter_names ~f:Option.is_some
    then
      Static_error.syntax_error "tuple type cannot have parameter labels"
      |> Result.Error
    else (
      match ts with
      | [] -> Type.Mono.Primitive Type.Primitive.Unit |> Result.Ok
      (* brackets for precedence only *)
      | [ t ] -> Result.Ok t
      | _ts ->
        Static_error.unsupported_feature "tuples other than unit"
        |> Result.Error)
  | Syntax.Effect_row r ->
    let message =
      sprintf
        "expected type, found effect row %s"
        (Syntax.sexp_of_effect_row r |> Sexp.to_string_hum)
    in
    Static_error.syntax_error message |> Result.Error
  | Syntax.Scheme s ->
    let%bind _t = simplify_type_scheme s in
    Static_error.unsupported_feature "higher rank types" |> Result.Error
  | Syntax.Type_atom { constructor; arguments } ->
    let%bind () =
      restrict_to_empty arguments ~description:"parameterised types"
    in
    (match constructor with
     (* variables / wildcards require closing over in a Type.Poly *)
     | Syntax.Variable_or_name _ | Syntax.Type_wildcard _ ->
       Static_error.unsupported_feature "wildcards/names/variables in types"
       |> Result.Error
     | Syntax.Type_int -> Type.Mono.Primitive Type.Primitive.Int |> Result.Ok
     | Syntax.Type_bool -> Type.Mono.Primitive Type.Primitive.Bool |> Result.Ok)
  | Syntax.Annotated_type { type_ = _; kind = _ } ->
    Static_error.unsupported_feature "kind annotation on type" |> Result.Error

(** convert a [Syntax.type_], to an [Effect.t] failing if it is actually a
    [Type.t] or [Effect.Label.t], or if it doesn't represent a valid [Effect.t]
*)
and simplify_type_as_effect : Syntax.type_ -> Effect.t Or_static_error.t =
  fun t ->
  let open Result.Let_syntax in
  match t with
  | Syntax.Effect_row (Syntax.Closed labels) ->
    let%map labels =
      List.map labels ~f:simplify_type_as_effect_label |> Result.all
    in
    let labels = Effect.Label.Multiset.of_list labels in
    Effect.Row (Effect.Row.Closed labels)
  | Syntax.Effect_row (Syntax.Open (labels, tail)) ->
    let%bind labels =
      Non_empty_list.map labels ~f:simplify_type_as_effect_label
      |> Static_error.all_non_empty
    in
    let labels = Effect.Label.Multiset.Non_empty.of_non_empty_list labels in
    let%map tail_effect = simplify_type_as_effect tail in
    Effect.cons_row ~labels ~effect_:tail_effect |> Effect.Row
  | Syntax.Type_atom { constructor; arguments } ->
    let%bind () =
      restrict_to_empty arguments ~description:"parameterised types"
    in
    (match constructor with
     (* variables / wildcards require closing over in a Type.Poly *)
     | Syntax.Variable_or_name _ | Syntax.Type_wildcard _ ->
       (* TODO: this will need to be supported *)
       Static_error.unsupported_feature "wildcards/names/variables in effects"
       |> Result.Error
     | Syntax.Type_int | Syntax.Type_bool ->
       let message =
         sprintf
           "expected effect, found type %s"
           (Syntax.sexp_of_type_constructor constructor |> Sexp.to_string_hum)
       in
       Static_error.syntax_error message |> Result.Error)
  | Syntax.Annotated_type { type_ = _; kind = _ } ->
    Static_error.unsupported_feature "kind annotation on type" |> Result.Error
  | Syntax.Arrow (_, _) | Syntax.Scheme _ | Syntax.Parameters_or_tuple _ ->
    let message =
      sprintf
        "expected effect, found type %s"
        (Syntax.sexp_of_type_ t |> Sexp.to_string_hum)
    in
    Static_error.syntax_error message |> Result.Error

and simplify_type_as_effect_label
  : Syntax.type_ -> Effect.Label.t Or_static_error.t
  =
  fun t ->
  let open Result.Let_syntax in
  match t with
  | Syntax.Type_atom { constructor; arguments } ->
    let%bind () =
      restrict_to_empty arguments ~description:"parameterised effects"
    in
    (match constructor with
     | Syntax.Variable_or_name l ->
       (* for now, assume always is a name, real rules are based on binding /
          whether the name is a single letter *)
       simplify_var_id_to_effect_label l |> Result.Ok
     | Syntax.Type_wildcard _ ->
       Static_error.syntax_error
         "polymorphism over effect labels is not permitted"
       |> Result.Error
     | Syntax.Type_int | Syntax.Type_bool ->
       let message =
         sprintf
           "expected effect label, found type %s"
           (Syntax.sexp_of_type_constructor constructor |> Sexp.to_string_hum)
       in
       Static_error.syntax_error message |> Result.Error)
  | Syntax.Annotated_type { type_ = _; kind = _ } ->
    Static_error.unsupported_feature "kind annotation on type" |> Result.Error
  | Syntax.Arrow (_, _)
  | Syntax.Effect_row _ | Syntax.Scheme _ | Syntax.Parameters_or_tuple _ ->
    let message =
      sprintf
        "expected effect label, found type %s"
        (Syntax.sexp_of_type_ t |> Sexp.to_string_hum)
    in
    Static_error.syntax_error message |> Result.Error

and simplify_type_scheme { Syntax.forall_quantified = _; body = _ }
  : Type.Poly.t Or_static_error.t
  =
  Static_error.unsupported_feature "type scheme" |> Result.Error

and simplify_parameter_type { Syntax.parameter_id; type_ }
  : (Variable.t option * Type.Mono.t) Or_static_error.t
  =
  let open Result.Let_syntax in
  let id = Option.map parameter_id ~f:simplify_identifier in
  let%map t = simplify_type_as_type type_ in
  id, t

and simplify_type_result { Syntax.effect_; result }
  : (Effect.t * Type.Mono.t) Or_static_error.t
  =
  let open Result.Let_syntax in
  let%bind effect_ = simplify_type_as_effect effect_ in
  let%map t_result = simplify_type_as_type result in
  effect_, t_result
;;

let simplify_literal (lit : Syntax.literal) : Min.Literal.t =
  match lit with
  | Syntax.Unit -> Min.Literal.Unit
  | Syntax.Int i -> Min.Literal.Int i
  | Syntax.Bool b -> Min.Literal.Bool b
;;

let simplify_parameter_id : Syntax.parameter_id -> Min.Parameter.t = function
  | Syntax.Parameter_id x -> simplify_identifier x |> Min.Parameter.Variable
  | Syntax.Parameter_wildcard -> Min.Parameter.Wildcard
;;

let simplify_parameter
  : Syntax.parameter -> (Min.Parameter.t * Type.Mono.t) Or_static_error.t
  =
  fun { Syntax.id; type_ } ->
  let open Result.Let_syntax in
  let p = simplify_parameter_id id in
  let%map t = simplify_type_as_type type_ in
  p, t
;;

let simplify_pattern : Syntax.pattern -> Min.Parameter.t = function
  | Syntax.Pattern_id x -> simplify_identifier x |> Min.Parameter.Variable
  | Syntax.Pattern_wildcard -> Min.Parameter.Wildcard
;;

let simplify_annotated_pattern { Syntax.pattern; scheme }
  : Min.Parameter.t Or_static_error.t
  =
  let open Result.Let_syntax in
  let p = simplify_pattern pattern in
  let%bind t =
    Option.map scheme ~f:simplify_type_scheme |> Static_error.all_option
  in
  let%map () = restrict_to_none t ~description:"type annotation on pattern" in
  p
;;

let simplify_pattern_parameter { Syntax.pattern; type_ }
  : (Min.Parameter.t * Type.Mono.t option) Or_static_error.t
  =
  let open Result.Let_syntax in
  let p = simplify_pattern pattern in
  let%map type_' =
    Option.map type_ ~f:simplify_type_as_type |> Static_error.all_option
  in
  p, type_'
;;

let simplify_operation_parameter
  :  Syntax.operation_parameter
  -> (Min.Parameter.t * Type.Mono.t option) Or_static_error.t
  =
  fun { Syntax.id; type_ } ->
  let open Result.Let_syntax in
  let p' = simplify_parameter_id id in
  let%map type_' =
    Option.map type_ ~f:simplify_type_as_type |> Static_error.all_option
  in
  p', type_'
;;

let simplify_binary_operator (op : Syntax.binary_operator)
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

let simplify_unary_operator (op : Syntax.unary_operator)
  : Min.Operator.Unary.t Or_static_error.t
  =
  match op with
  | Syntax.Exclamation -> Result.Ok Min.Operator.(Unary.Bool Bool.Unary.Not)
;;

let rec simplify_expr (e : Syntax.expr) : Min.Expr.t Or_static_error.t =
  let open Result.Let_syntax in
  match e with
  | Syntax.Return _ -> Static_error.unsupported_feature "return" |> Result.Error
  | Syntax.If_then_else (e_cond, block_yes, block_no) ->
    let%bind e_cond' = simplify_expr e_cond in
    let%bind block_yes' = simplify_block block_yes in
    let%map block_no' = simplify_block block_no in
    Min.Expr.If_then_else (e_cond', block_yes', block_no')
  | Syntax.If_then (e_cond, block_yes) ->
    let block_no = Syntax.singleton_block (Syntax.Literal Syntax.Unit) in
    Syntax.If_then_else (e_cond, block_yes, block_no) |> simplify_expr
  | Syntax.Handler handler ->
    let%map handler' = simplify_effect_handler handler in
    Min.Expr.Handler handler' |> Min.Expr.Value
  | Syntax.Handle { subject; handler } ->
    let%bind handler' = simplify_expr (Syntax.Handler handler) in
    let%map subject' = simplify_expr subject in
    Min.Expr.Application (handler', [ subject' ])
  | Syntax.Fn f ->
    let%map lambda = simplify_fn f in
    Min.Expr.Lambda lambda |> Min.Expr.Value
  | Syntax.Application (e_f, e_args) ->
    let%bind e_f' = simplify_expr e_f in
    let%map e_args' = List.map e_args ~f:simplify_expr |> Result.all in
    Min.Expr.Application (e_f', e_args')
  | Syntax.Identifier x ->
    let x' = simplify_identifier x in
    Min.Expr.Variable x' |> Min.Expr.Value |> Result.Ok
  | Syntax.Literal lit ->
    let lit' = simplify_literal lit in
    Min.Expr.Literal lit' |> Min.Expr.Value |> Result.Ok
  | Syntax.Binary_op (e_left, op, e_right) ->
    let%bind e_left' = simplify_expr e_left in
    let%bind e_right' = simplify_expr e_right in
    let%map op' = simplify_binary_operator op in
    Min.Expr.Operator (e_left', op', e_right')
  | Syntax.Unary_op (op, e) ->
    let%bind e' = simplify_expr e in
    let%map op' = simplify_unary_operator op in
    Min.Expr.Unary_operator (op', e')
  | Syntax.Annotated_expr (_e, _scheme) ->
    Static_error.unsupported_feature "type annotation on expression"
    |> Result.Error

and simplify_fun_declaration { Syntax.id; fn }
  : Min.Decl.Fun.t Or_static_error.t
  =
  let open Result.Let_syntax in
  let%map lambda = simplify_fn fn in
  let f_name = simplify_identifier id in
  f_name, lambda

and simplify_declaration_preceding
  : Syntax.declaration -> Min.Expr.t -> Min.Expr.t Or_static_error.t
  =
  fun declaration e ->
  let open Result.Let_syntax in
  match declaration with
  | Syntax.Fun f ->
    let%map (f_fix_lambda : Min.Expr.fix_lambda) = simplify_fun_declaration f in
    let f_name, _ = f_fix_lambda in
    Min.Expr.Let (f_name, Min.Expr.Fix_lambda f_fix_lambda, e)
  (* local `val` is monomorphic binding *)
  | Syntax.Val (pattern, block) ->
    let%bind p = simplify_annotated_pattern pattern in
    let%map e_x = simplify_block block in
    (match p with
     | Min.Parameter.Variable x -> Min.Expr.Let_mono (x, e_x, e)
     | Min.Parameter.Wildcard -> Min.Expr.Seq (e_x, e))

(** given the simplification of the tail of a block, and a statement (its head)
    produce the resulting expression *)
and simplify_statement_preceding
  : Syntax.statement -> Min.Expr.t -> Min.Expr.t Or_static_error.t
  =
  fun statement e ->
  let open Result.Let_syntax in
  match statement with
  | Syntax.Declaration d -> simplify_declaration_preceding d e
  | Syntax.Expr e0 ->
    let%map e0 = simplify_expr e0 in
    Min.Expr.Seq (e0, e)

and simplify_block { Syntax.statements; last } : Min.Expr.t Or_static_error.t =
  let open Result.Let_syntax in
  let%bind last = simplify_expr last in
  Static_error.list_fold_right statements ~init:last ~f:(fun statement e ->
    simplify_statement_preceding statement e)

and simplify_fn { Syntax.type_parameters; parameters; result_type; body }
  : Min.Expr.lambda Or_static_error.t
  =
  let open Result.Let_syntax in
  let%bind () =
    restrict_to_empty type_parameters ~description:"type parameter annotations"
  in
  let%bind () =
    restrict_to_none result_type ~description:"return type annotation"
  in
  let%bind parameters =
    List.map ~f:simplify_pattern_parameter parameters |> Result.all
  in
  let names, types = List.unzip parameters in
  let%bind () =
    restrict_to_all_none types ~description:"type annotations on parameters"
  in
  let%map body' = simplify_block body in
  names, body'

and simplify_effect_handler (Syntax.Effect_handler op_handlers)
  : Min.Expr.handler Or_static_error.t
  =
  let open Result.Let_syntax in
  let%bind named_op_handers =
    List.map op_handlers ~f:simplify_operation_handler |> Result.all
  in
  let named_op_handers, return_handlers =
    List.partition_map named_op_handers ~f:(fun (name, op_handler) ->
      match name with
      | `Op (name, shape) -> Either.First (name, (shape, op_handler))
      | `Return -> Either.Second op_handler)
  in
  let%bind operations =
    match Variable.Map.of_alist named_op_handers with
    | `Ok operations -> Result.Ok operations
    | `Duplicate_key name ->
      let message =
        sprintf
          "multiple handler clauses given for operation %s"
          (Variable.to_string_user name)
      in
      Static_error.syntax_error message |> Result.Error
  in
  let%map return_clause =
    match return_handlers with
    | [] -> Result.Ok None
    | [ return_clause ] -> Result.Ok (Some return_clause)
    | _ ->
      Static_error.syntax_error
        "multiple return clauses given in effect handler"
      |> Result.Error
  in
  { Min.Expr.operations; return_clause }

and simplify_operation_handler
  :  Syntax.operation_handler
  -> ([ `Op of Variable.t * Operation_shape.t | `Return ] * Min.Expr.op_handler)
       Or_static_error.t
  =
  let open Result.Let_syntax in
  let simplify_clause parameters body =
    let%bind parameters' =
      List.map parameters ~f:simplify_operation_parameter |> Result.all
    in
    let names, types = List.unzip parameters' in
    let%bind op_argument =
      restrict_to_singleton
        names
        ~description:"operation can only have exactly one argument"
    in
    let%bind () =
      restrict_to_all_none
        types
        ~description:"type annotations on operation handler parameters"
    in
    let%map op_body = simplify_block body in
    op_argument, op_body
  in
  function
  | Syntax.Op_control { id; parameters; body } ->
    let id' = simplify_var_id id in
    let%map op_argument, op_body = simplify_clause parameters body in
    let shape = Operation_shape.Control in
    `Op (id', shape), { Min.Expr.op_argument; op_body }
  | Syntax.Op_fun { id; parameters; body } ->
    let id' = simplify_var_id id in
    let%map op_argument, op_body = simplify_clause parameters body in
    let shape = Operation_shape.Fun in
    `Op (id', shape), { Min.Expr.op_argument; op_body }
  | Syntax.Op_except { id = _; parameters = _; body = _ } ->
    (* TODO: doesn't gain any performance over control by default (special
       implementation wouldn't collection resumption) *)
    Static_error.unsupported_feature "`execption` effect" |> Result.Error
  | Syntax.Op_val { id = _; type_ = _; value = _ } ->
    (* need to first evaluate `value`, then capture it in the handler (fairly
       simple, but requires creating a fresh variable name) *)
    Static_error.unsupported_feature "`val` effect" |> Result.Error
  | Syntax.Op_return { parameter; body } ->
    let%bind op_argument, type_ = simplify_operation_parameter parameter in
    let%bind () =
      restrict_to_none
        type_
        ~description:"type annotation on operation handler parameter"
    in
    let%map op_body = simplify_block body in
    `Return, { Min.Expr.op_argument; op_body }
;;

let simplify_operation_declaration { Syntax.id; type_parameters; shape }
  : (Variable.t * Min.Decl.Effect.Operation.t) Or_static_error.t
  =
  let open Result.Let_syntax in
  let%bind shape, parameters, t_answer =
    match shape with
    | Syntax.Shape_except (_, _) | Syntax.Shape_val _ ->
      Static_error.unsupported_feature "non `control` effect" |> Result.Error
    | Syntax.Shape_control (parameters, t_answer) ->
      (Operation_shape.Control, parameters, t_answer) |> Result.Ok
    | Syntax.Shape_fun (parameters, t_answer) ->
      (Operation_shape.Fun, parameters, t_answer) |> Result.Ok
  in
  let id' = simplify_var_id id in
  let%bind () =
    restrict_to_empty
      type_parameters
      ~description:"type parameters for operation handler"
  in
  let%bind parameters' =
    List.map parameters ~f:simplify_parameter |> Result.all
  in
  let _names, t_parameters = List.unzip parameters' in
  let%bind t_parameter =
    restrict_to_singleton
      t_parameters
      ~description:"operation can ony have exactly one argument"
  in
  let%map t_answer' = simplify_type_as_type t_answer in
  ( id'
  , { Min.Decl.Effect.Operation.shape
    ; argument = t_parameter
    ; answer = t_answer'
    } )
;;

let simplify_effect_declaration { Syntax.id; type_parameters; kind; operations }
  : Min.Decl.Effect.t Or_static_error.t
  =
  let open Result.Let_syntax in
  let name = simplify_var_id_to_effect_label id in
  let%bind () =
    restrict_to_empty type_parameters ~description:"type parameters for effect"
  in
  (* TODO: effect would never have outer kind annotation? *)
  let%bind () =
    restrict_to_none kind ~description:"kind annotation for effect"
  in
  let%bind operations' =
    List.map operations ~f:simplify_operation_declaration |> Result.all
  in
  let%map operations' =
    match Variable.Map.of_alist operations' with
    | `Duplicate_key op_name ->
      let message =
        sprintf
          "duplicate operation name %s in effect %s"
          (Variable.to_string_user op_name)
          (Effect.Label.to_string name)
      in
      Static_error.syntax_error message |> Result.Error
    | `Ok operations' -> Result.Ok operations'
  in
  { Min.Decl.Effect.name; operations = operations' }
;;

let simplify_pure_declaration
  : Syntax.pure_declaration -> Min.Decl.t Or_static_error.t
  = function
  | Syntax.Top_val _ ->
    Static_error.unsupported_feature "toplevel val binding" |> Result.Error
  | Syntax.Top_fun declaration ->
    let%map.Result declaration = simplify_fun_declaration declaration in
    Min.Decl.Fun declaration
;;

let simplify_type_declaration
  : Syntax.type_declaration -> Min.Decl.t Or_static_error.t
  = function
  | Syntax.Effect_declaration declaration ->
    let%map.Result declaration = simplify_effect_declaration declaration in
    Min.Decl.Effect declaration
;;

let simplify_toplevel_declaration
  : Syntax.toplevel_declaration -> Min.Decl.t Or_static_error.t
  = function
  | Syntax.Pure_declaration declaration -> simplify_pure_declaration declaration
  | Syntax.Type_declaration declaration -> simplify_type_declaration declaration
;;

let simplify_program (Syntax.Program declarations)
  : Min.Program.t Or_static_error.t
  =
  let open Result.Let_syntax in
  let%map declarations =
    List.map declarations ~f:simplify_toplevel_declaration |> Result.all
  in
  { Min.Program.declarations }
;;
