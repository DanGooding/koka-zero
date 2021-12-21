open Core
open Koka_zero_inference
module M = Minimal_syntax
module E = M.Expr

let%expect_test "occurs check rejects omega combinator" =
  let expr =
    E.Lambda
      ( [ M.Variable.of_string "x" ]
      , E.Application
          ( E.Variable (M.Variable.of_string "x")
          , [ E.Variable (M.Variable.of_string "x") ] ) )
  in
  Util.print_expr_inference_result expr;
  [%expect
    {|
    (Error
     ((kind Type_error)
      (message
        "cannot unify\
       \n(Metavariable a0)\
       \nwith\
       \n(Arrow ((Metavariable a0)) (Metavariable e0) (Metavariable a2))\
       \n")
      (location ()))) |}]
;;

let%expect_test "if statement's branches must have the same type" =
  (* if true then 1 else () *)
  let expr =
    E.If_then_else
      ( E.Literal (M.Literal.Bool true)
      , E.Literal (M.Literal.Int 1)
      , E.Literal M.Literal.Unit )
  in
  Util.print_expr_inference_result expr;
  [%expect
    {|
    (Error
     ((kind Type_error) (message  "cannot unify\
                                 \nInt\
                                 \nwith\
                                 \nUnit\
                                 \n")
      (location ()))) |}]
;;

let decl_state =
  let name = Effect.Label.of_string "state" in
  let op_get =
    let argument = Type.Mono.Primitive Type.Primitive.Unit in
    let answer = Type.Mono.Primitive Type.Primitive.Int in
    { M.Effect_decl.Operation.argument; answer }
  in
  let op_set =
    let argument = Type.Mono.Primitive Type.Primitive.Int in
    let answer = Type.Mono.Primitive Type.Primitive.Unit in
    { M.Effect_decl.Operation.argument; answer }
  in
  let operations =
    M.Variable.Map.of_alist_exn
      [ M.Variable.of_string "get", op_get; M.Variable.of_string "set", op_set ]
  in
  { M.Effect_decl.name; operations }
;;

let%expect_test "handler must include all operations" =
  let effect_declarations = [ decl_state ] in
  let state_handler_set_only =
    (* handler { set(x) { () } } *)
    let set_clause =
      let op_argument = M.Variable.of_string "x" in
      let op_body = E.Literal M.Literal.Unit in
      { E.op_argument; op_body }
    in
    let operations =
      M.Variable.Map.singleton (M.Variable.of_string "set") set_clause
    in
    { E.operations; return_clause = None }
  in
  let body = E.Handle (state_handler_set_only, E.Literal M.Literal.Unit) in
  let program = { M.Program.effect_declarations; body } in
  Util.print_inference_result program;
  [%expect
    {|
    (Error
     ((kind Type_error) (message "handler does not match any effect: (set)")
      (location ()))) |}]
;;
