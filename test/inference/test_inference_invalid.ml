open Koka_zero_inference
module M = Minimal_syntax
module E = M.Expr

let%expect_test "occurs check rejects omega combinator" =
  let expr =
    E.Lambda
      ( [ M.Variable.of_user "x" ]
      , E.Application
          ( E.Variable (M.Variable.of_user "x")
          , [ E.Variable (M.Variable.of_user "x") ] ) )
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

let%expect_test "handler must include all operations" =
  let declarations = [ M.Decl.Effect Util.Expr.decl_state ] in
  let state_handler_set_only =
    (* handler { set(x) { () } } *)
    let set_clause =
      let op_argument = M.Variable.of_user "x" in
      let op_body = E.Literal M.Literal.Unit in
      { E.op_argument; op_body }
    in
    let operations =
      M.Variable.Map.singleton (M.Variable.of_user "set") set_clause
    in
    { E.operations; return_clause = None }
  in
  let body =
    Util.Expr.make_handle_expr state_handler_set_only (E.Literal M.Literal.Unit)
  in
  Util.print_expr_inference_result ~declarations body;
  [%expect
    {|
    (Error
     ((kind Type_error)
      (message "handler does not match any effect: ((User set))") (location ()))) |}]
;;
