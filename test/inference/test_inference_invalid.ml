open Koka_zero_inference
module M = Minimal_syntax
module E = M.Expr
module UE = Util.Expr

let%expect_test "occurs check rejects omega combinator" =
  let expr =
    E.Value
      (E.Lambda
         ([ Variable.of_user "x" ], E.Application (UE.var "x", [ UE.var "x" ])))
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
       \n(Arrow ((Metavariable a0)) (Metavariable e0) (Metavariable a1))\
       \n")
      (location ()))) |}]
;;

let%expect_test "if statement's branches must have the same type" =
  (* if true then 1 else () *)
  let expr = E.If_then_else (UE.lit_bool true, UE.lit_int 1, UE.lit_unit) in
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

let%expect_test "fix lambdas name cannot collide with own parameters" =
  let expr =
    E.Value
      (E.Fix_lambda
         ( Variable.of_user "f"
         , ([ Variable.of_user "g"; Variable.of_user "f" ], UE.lit_unit) ))
  in
  Util.print_expr_inference_result expr;
  [%expect
    {|
    (Error
     ((kind Type_error)
      (message "recursive function's name is shadowed by own parameter f")
      (location ())))|}]
;;

let%expect_test "cannot shadow functions at toplevel" =
  let declarations =
    [ M.Decl.Fun (Variable.of_user "foo", ([], UE.lit_unit))
    ; M.Decl.Fun (Variable.of_user "foo", ([], UE.lit_unit))
    ]
  in
  let program = { M.Program.declarations } in
  Util.print_check_program_without_main_result program;
  [%expect
    {|
    (Error
     ((kind Type_error) (message "cannot shadow 'foo' at toplevel")
      (location ()))) |}]
;;

let%expect_test "handler must include all operations" =
  let declarations = [ M.Decl.Effect Util.Expr.decl_state ] in
  let state_handler_set_only =
    (* handler { set(x) { () } } *)
    let set_clause =
      let op_argument = Variable.of_user "x" in
      let op_body = UE.lit_unit in
      { E.op_argument; op_body }
    in
    let operations =
      Variable.Map.singleton (Variable.of_user "set") set_clause
    in
    { E.operations; return_clause = None }
  in
  let body = Util.Expr.make_handle_expr state_handler_set_only UE.lit_unit in
  Util.print_expr_inference_result ~declarations body;
  [%expect
    {|
    (Error
     ((kind Type_error)
      (message "handler does not match any effect: ((User set))") (location ()))) |}]
;;
