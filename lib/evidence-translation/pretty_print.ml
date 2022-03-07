open Core
open Import
open Evidence_passing_syntax
module Parameter = Koka_zero_inference.Minimal_syntax.Parameter
module Literal = Koka_zero_inference.Minimal_syntax.Literal
module Operator = Koka_zero_inference.Minimal_syntax.Operator

let print_variable : Variable.t -> string = Variable.to_string_user

let print_parameter : Parameter.t -> string = function
  | Parameter.Wildcard -> "_"
  | Parameter.Variable v -> print_variable v
;;

let print_literal : Literal.t -> string = function
  | Literal.Int i -> sprintf "%d" i
  | Literal.Bool b -> sprintf "%b" b
  | Literal.Unit -> "()"
;;

let print_operator : Operator.t -> string =
  let print_int_op = function
    | Operator.Int.Plus -> "+"
    | Operator.Int.Minus -> "-"
    | Operator.Int.Times -> "*"
    | Operator.Int.Divide -> "/"
    | Operator.Int.Modulo -> "%"
    | Operator.Int.Less_than -> "<"
    | Operator.Int.Less_equal -> "<="
    | Operator.Int.Greater_than -> ">"
    | Operator.Int.Greater_equal -> ">="
    | Operator.Int.Equals -> "=="
    | Operator.Int.Not_equal -> "!="
  in
  let print_bool_op = function
    | Operator.Bool.And -> "&&"
    | Operator.Bool.Or -> "||"
  in
  function
  | Operator.Int iop -> print_int_op iop
  | Operator.Bool bop -> print_bool_op bop
;;

let print_unary_operator : Operator.Unary.t -> string = function
  | Operator.Unary.Bool Operator.Bool.Unary.Not -> "not"
;;

let rec print_expr : Expr.t -> string = function[@warning "-4"]
  (* TODO: how to efficiently build string? use [Buffer]? *)
  | Expr.Variable v -> print_variable v
  | Expr.Lambda lambda -> print_lambda lambda
  | Expr.Fix_lambda (f, lambda) ->
    (* TODO: actual declarations won't be expressions *)
    let f_s = print_variable f in
    let lambda_s = print_lambda lambda in
    sprintf "(let rec %s = %s in %s)" f_s lambda_s f_s
  | Expr.Application (e_f, e_args) ->
    let f_s = print_expr e_f in
    let args_s = List.map e_args ~f:print_expr |> String.concat ~sep:", " in
    sprintf "%s (%s)" f_s args_s
  | Expr.Literal lit -> print_literal lit
  | Expr.If_then_else (e_cond, e_yes, e_no) ->
    let cond_s = print_expr e_cond in
    let yes_s = print_expr e_yes in
    let no_s = print_expr e_no in
    sprintf "(if %s then %s else %s)" cond_s yes_s no_s
  | Expr.Operator (e_left, op, e_right) ->
    let left_s = print_expr e_left in
    let op_s = print_operator op in
    let right_s = print_expr e_right in
    sprintf "(%s %s %s)" left_s op_s right_s
  | Expr.Unary_operator (op, e) ->
    let op_s = print_unary_operator op in
    let e_s = print_expr e in
    sprintf "(%s %s)" op_s e_s
  | Expr.Construct_pure e ->
    let e_s = print_expr e in
    sprintf "Pure %s" e_s
  | Expr.Construct_yield { marker; op_clause; resumption } ->
    let marker_s = print_expr marker in
    let op_clause_s = print_expr op_clause in
    let resumption_s = print_expr resumption in
    sprintf
      "Yield { marker = %s; op_clause = %s; resumption = %s }"
      marker_s
      op_clause_s
      resumption_s
  | Expr.Match_ctl { subject; pure_branch; yield_branch } ->
    let subject_s = print_expr subject in
    let pure_branch_s = print_lambda pure_branch in
    let yield_branch_s = print_lambda yield_branch in
    sprintf
      "(match %s with\n\
       | Pure x -> x |> %s\n\
       | Yield { marker; op_clause; resumption } ->\n\
       (marker; op_clause; resumption) |> %s)"
      subject_s
      pure_branch_s
      yield_branch_s
  | _ ->
    (* TODO: remainder won't appear in user code *)
    (* TODO: special case return & pure *)
    "failwith \"not implemented\""

and print_lambda : Expr.lambda -> string =
 fun (xs, e) ->
  (* non curried arguments passed as tuples *)
  let xs_s = List.map xs ~f:print_parameter |> String.concat ~sep:", " in
  let e_s = print_expr e in
  sprintf "(fun (%s) -> %s)" xs_s e_s
;;

let types_prelude =
  (* TODO: separate types for each effect's hnd? *)
  {|
type ('a, 'b, 'c, 'd) ctl =
  | Pure of 'a
  | Yield of { marker : 'b; op_clause: 'c; resumption: 'd }
;;

type ('a, 'b) op =
  | Normal of 'a
  | Tail of 'b
;;
|}
;;

(* TODO: not aiming for code to run, only to typecheck! *)

let print_fun_decl : Program.Fun_decl.t -> string =
 fun (f, (xs, e)) ->
  let f_s = print_variable f in
  let xs_s = List.map xs ~f:print_parameter |> String.concat ~sep:", " in
  let e_s = print_expr e in
  sprintf "let rec %s (%s) = %s" f_s xs_s e_s
;;

let print_program : Program.t -> string =
 fun { Program.effect_declarations = _; fun_declarations } ->
  (* OCaml prelude: define Ctl/Pure, Op, Hnd, Evidence, Vector, marker global,
     impure_builtins *)
  (* Koka prelude too? - no choice? *)
  (* TODO: what is the purpose of effect_declarations? *)
  let fun_decls_s =
    List.map fun_declarations ~f:print_fun_decl |> String.concat ~sep:"\n;;\n\n"
  in
  types_prelude ^ fun_decls_s
;;
