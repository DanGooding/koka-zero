open Core
open Koka_zero_inference
module M = Minimal_syntax
module E = M.Expr
module UE = Util.Expr
module UP = Util.Parameter

let%expect_test "identity gets polymorphic type" =
  (* TODO: once introduce declarations, add a better test than this (expressions
     never have polymorphic type themselves) *)
  let expr =
    (* let id = \x. x in id id () *)
    (* tests id can be applied to functions, and units *)
    let id = UE.var "id" in
    E.Let
      ( Variable.of_user "id"
      , E.Lambda ([ UP.var "z" ], UE.var "z")
      , E.Application (E.Application (id, [ id ]), [ UE.lit_unit ]) )
  in
  Util.print_expr_inference_result expr;
  [%expect
    {|
    (Ok
     ((Primitive Unit) (Metavariable e7)
      (Let (User id) (Lambda (((Variable (User z))) (Value (Variable (User z)))))
       (Application
        (Application (Value (Variable (User id))) ((Value (Variable (User id)))))
        ((Value (Literal Unit))))))) |}]
;;

(* TODO: tests to add: static scoping, not generalising free varaibles, not
   generalising lambdas *)

let%expect_test "fix combinator allows recursion" =
  let expr =
    (* fix f. \x. f x *)
    E.Value
      (E.Fix_lambda
         ( Variable.of_user "f"
         , ([ UP.var "x" ], E.Application (UE.var "f", [ UE.var "x" ])) ))
  in
  Util.print_expr_inference_result expr;
  [%expect
    {|
    (Ok
     ((Arrow ((Metavariable a2)) (Metavariable e3) (Metavariable a3))
      (Metavariable e4)
      (Value
       (Fix_lambda
        ((User f)
         (((Variable (User x)))
          (Application (Value (Variable (User f))) ((Value (Variable (User x))))))))))) |}]
;;

let%expect_test "wildcard parameters affect type" =
  let expr =
    (* fn (_, y, _) y *)
    E.Value
      (E.Lambda
         ( [ M.Parameter.Wildcard
           ; M.Parameter.Variable (Variable.of_user "y")
           ; M.Parameter.Wildcard
           ]
         , UE.var "x" ))
  in
  Util.print_expr_inference_result expr;
  [%expect
    {| (Error ((kind Type_error) (message "unbound variable: x") (location ()))) |}]
;;

let%expect_test "literal unit" =
  let expr = UE.lit_unit in
  Util.print_expr_inference_result expr;
  [%expect
    {| (Ok ((Primitive Unit) (Metavariable e0) (Value (Literal Unit)))) |}]
;;

let%expect_test "literal bool" =
  let expr = UE.lit_bool true in
  Util.print_expr_inference_result expr;
  [%expect
    {| (Ok ((Primitive Bool) (Metavariable e0) (Value (Literal (Bool true))))) |}]
;;

let%expect_test "literal int" =
  let expr = UE.lit_int 1 in
  Util.print_expr_inference_result expr;
  [%expect
    {| (Ok ((Primitive Int) (Metavariable e0) (Value (Literal (Int 1))))) |}]
;;

let%expect_test "int operators" =
  let i n = UE.lit_int n in
  let oi o = M.Operator.Int o in
  let expr =
    E.Operator
      ( i 1
      , oi M.Operator.Int.Plus
      , E.Operator
          ( i 2
          , oi M.Operator.Int.Minus
          , E.Operator
              ( i 3
              , oi M.Operator.Int.Times
              , E.Operator
                  ( i 4
                  , oi M.Operator.Int.Divide
                  , E.Operator (i 5, oi M.Operator.Int.Modulo, i 7) ) ) ) )
  in
  Util.print_expr_inference_result expr;
  [%expect
    {|
    (Ok
     ((Primitive Int) (Metavariable e5)
      (Operator (Value (Literal (Int 1))) (Int Plus)
       (Operator (Value (Literal (Int 2))) (Int Minus)
        (Operator (Value (Literal (Int 3))) (Int Times)
         (Operator (Value (Literal (Int 4))) (Int Divide)
          (Operator (Value (Literal (Int 5))) (Int Modulo)
           (Value (Literal (Int 7)))))))))) |}]
;;

let%expect_test "comparsion operators" =
  let i n = UE.lit_int n in
  let oi o = M.Operator.Int o in
  let ob o = M.Operator.Bool o in
  let expr =
    E.Operator
      ( E.Operator
          ( E.Operator (i 3, oi M.Operator.Int.Less_than, i 5)
          , ob M.Operator.Bool.Or
          , E.Operator (i 3, oi M.Operator.Int.Greater_than, i 5) )
      , ob M.Operator.Bool.Or
      , E.Operator (i 3, oi M.Operator.Int.Equals, i 5) )
  in
  Util.print_expr_inference_result expr;
  [%expect
    {|
    (Ok
     ((Primitive Bool) (Metavariable e5)
      (Operator
       (Operator
        (Operator (Value (Literal (Int 3))) (Int Less_than)
         (Value (Literal (Int 5))))
        (Bool Or)
        (Operator (Value (Literal (Int 3))) (Int Greater_than)
         (Value (Literal (Int 5)))))
       (Bool Or)
       (Operator (Value (Literal (Int 3))) (Int Equals)
        (Value (Literal (Int 5))))))) |}]
;;

let%expect_test "multi argument functions" =
  (* ( \b x y. if b then x else y )(true, 1, 0) *)
  let expr =
    E.Application
      ( E.Value
          (E.Lambda
             ( [ UP.var "b"; UP.var "x"; UP.var "y" ]
             , E.If_then_else (UE.var "b", UE.var "x", UE.var "y") ))
      , [ UE.lit_bool true; UE.lit_int 1; UE.lit_int 0 ] )
  in
  Util.print_expr_inference_result expr;
  [%expect
    {|
    (Ok
     ((Primitive Int) (Metavariable e7)
      (Application
       (Value
        (Lambda
         (((Variable (User b)) (Variable (User x)) (Variable (User y)))
          (If_then_else (Value (Variable (User b))) (Value (Variable (User x)))
           (Value (Variable (User y)))))))
       ((Value (Literal (Bool true))) (Value (Literal (Int 1)))
        (Value (Literal (Int 0))))))) |}]
;;

let%expect_test "declared functions are generalised" =
  let identity = Variable.of_user "id", ([ UP.var "x" ], UE.var "x") in
  let const_true =
    ( Variable.of_user "const_true"
    , ([], E.Application (UE.var "id", [ UE.lit_bool true ])) )
  in
  let const_three =
    ( Variable.of_user "const_three"
    , ([], E.Application (UE.var "id", [ UE.lit_int 3 ])) )
  in
  let declarations =
    [ M.Decl.Fun identity; M.Decl.Fun const_true; M.Decl.Fun const_three ]
  in
  let program = { M.Program.declarations } in
  Util.print_check_program_without_main_result program;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun ((User id) (((Variable (User x))) (Value (Variable (User x))))))
        (Fun
         ((User const_true)
          (()
           (Application (Value (Variable (User id)))
            ((Value (Literal (Bool true))))))))
        (Fun
         ((User const_three)
          (()
           (Application (Value (Variable (User id))) ((Value (Literal (Int 3)))))))))))) |}]
;;

let%expect_test "sequence doesn't require first to be unit" =
  let e = E.Seq (UE.lit_int 0, UE.lit_bool true) in
  Util.print_expr_inference_result e;
  [%expect
    {|
    (Ok
     ((Primitive Bool) (Metavariable e1)
      (Seq (Value (Literal (Int 0))) (Value (Literal (Bool true)))))) |}]
;;

let%expect_test "local function can shadow toplevel" =
  let declarations =
    [ M.Decl.Fun (Variable.of_user "foo", ([], UE.lit_unit))
    ; M.Decl.Fun
        ( Variable.of_user "bar"
        , ( []
          , E.Let
              (Variable.of_user "foo", E.Lambda ([], UE.lit_unit), UE.lit_unit)
          ) )
    ]
  in
  let program = { M.Program.declarations } in
  Util.print_check_program_without_main_result program;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun ((User foo) (() (Value (Literal Unit)))))
        (Fun
         ((User bar)
          (()
           (Let (User foo) (Lambda (() (Value (Literal Unit))))
            (Value (Literal Unit)))))))))) |}]
;;

let%expect_test "handled effects reflected in subject's effect" =
  let declarations =
    [ M.Decl.Effect Util.Expr.decl_read; M.Decl.Effect Util.Expr.decl_exn ]
  in
  let body =
    (* \f. handler h_exn (\_. handler h_read (\_. handler h_read f)) *)
    (* (() -> <exn,read,read|e> a) -> e a *)
    E.Value
      (E.Lambda
         ( [ UP.var "f" ]
         , Util.Expr.make_handle_expr
             (Util.Expr.exn_handler UE.lit_unit)
             (Util.Expr.make_handle_expr
                (Util.Expr.read_handler 1)
                (Util.Expr.make_handle_expr
                   (Util.Expr.read_handler 1)
                   (E.Application (UE.var "f", [])))) ))
  in
  Util.print_expr_inference_result ~declarations body;
  [%expect
    {|
    (Ok
     ((Arrow
       ((Arrow () (Row (Open (Non_empty ((exn 1) (read 2))) (Metavariable e19)))
         (Primitive Unit)))
       (Metavariable e19) (Primitive Unit))
      (Metavariable e20)
      (Value
       (Lambda
        (((Variable (User f)))
         (Application
          (Value
           (Handler
            ((handled_effect exn)
             (operations
              (((User throw)
                (Control
                 ((op_argument Wildcard) (op_body (Value (Literal Unit))))))))
             (return_clause ()))))
          ((Value
            (Lambda
             (()
              (Application
               (Value
                (Handler
                 ((handled_effect read)
                  (operations
                   (((User ask)
                     (Fun
                      ((op_argument Wildcard)
                       (op_body (Value (Literal (Int 1)))))))))
                  (return_clause ()))))
               ((Value
                 (Lambda
                  (()
                   (Application
                    (Value
                     (Handler
                      ((handled_effect read)
                       (operations
                        (((User ask)
                          (Fun
                           ((op_argument Wildcard)
                            (op_body (Value (Literal (Int 1)))))))))
                       (return_clause ()))))
                    ((Value
                      (Lambda (() (Application (Value (Variable (User f))) ()))))))))))))))))))))) |}]
;;

let%expect_test "return clause is typed correctly" =
  let declarations = [ M.Decl.Effect Util.Expr.decl_query ] in
  let query_handler =
    (* {[
         handler { fun test(x){ x == 3 }; return(b) { if b then () else () } }
       ]} *)
    let test_clause =
      let op_argument = Variable.of_user "x" in
      let op_body =
        E.Operator
          ( E.Value (E.Variable op_argument)
          , M.Operator.Int M.Operator.Int.Equals
          , UE.lit_int 3 )
      in
      let op_argument = M.Parameter.Variable op_argument in
      { E.op_argument; op_body }
    in
    let return_clause =
      let op_argument = Variable.of_user "b" in
      let op_body =
        E.If_then_else
          (E.Value (E.Variable op_argument), UE.lit_unit, UE.lit_unit)
      in
      let op_argument = M.Parameter.Variable op_argument in
      Some { E.op_argument; op_body }
    in
    let operations =
      Variable.Map.singleton
        (Variable.of_user "test")
        (Operation_shape.Fun, test_clause)
    in
    { E.operations; return_clause }
  in
  let body =
    (* handle h_query (test(5)) *)
    Util.Expr.make_handle_expr
      query_handler
      (E.Application (UE.var "test", [ UE.lit_int 5 ]))
  in
  Util.print_expr_inference_result ~declarations body;
  [%expect
    {|
    (Ok
     ((Primitive Unit) (Metavariable e13)
      (Application
       (Value
        (Handler
         ((handled_effect query)
          (operations
           (((User test)
             (Fun
              ((op_argument (Variable (User x)))
               (op_body
                (Operator (Value (Variable (User x))) (Int Equals)
                 (Value (Literal (Int 3))))))))))
          (return_clause
           (((op_argument (Variable (User b)))
             (op_body
              (If_then_else (Value (Variable (User b))) (Value (Literal Unit))
               (Value (Literal Unit))))))))))
       ((Value
         (Lambda
          (()
           (Application
            (Value (Perform ((operation (User test)) (performed_effect query))))
            ((Value (Literal (Int 5)))))))))))) |}]
;;

let%expect_test "handlers can delegate to outer handlers" =
  let declarations = [ M.Decl.Effect Util.Expr.decl_read ] in
  let outer_handler = Util.Expr.read_handler 1 in
  (* { fun ask(unit) { ask(()) + 1 } } *)
  let inner_handler =
    let op_name = Variable.of_user "ask" in
    let op_argument = M.Parameter.Wildcard in
    let op_body =
      E.Operator
        ( E.Application (UE.var "ask", [ UE.lit_unit ])
        , M.Operator.Int M.Operator.Int.Plus
        , UE.lit_int 1 )
    in
    Util.Expr.singleton_handler
      ~op_name
      ~op_argument
      ~op_body
      ~shape:Operation_shape.Fun
  in
  (* handle outer (handle inner (ask(()) )) *)
  let body =
    Util.Expr.make_handle_expr
      outer_handler
      (Util.Expr.make_handle_expr
         inner_handler
         (E.Application (UE.var "ask", [ UE.lit_unit ])))
  in
  Util.print_expr_inference_result ~declarations body;
  [%expect
    {|
    (Ok
     ((Primitive Int) (Metavariable e19)
      (Application
       (Value
        (Handler
         ((handled_effect read)
          (operations
           (((User ask)
             (Fun ((op_argument Wildcard) (op_body (Value (Literal (Int 1)))))))))
          (return_clause ()))))
       ((Value
         (Lambda
          (()
           (Application
            (Value
             (Handler
              ((handled_effect read)
               (operations
                (((User ask)
                  (Fun
                   ((op_argument Wildcard)
                    (op_body
                     (Operator
                      (Application
                       (Value
                        (Perform
                         ((operation (User ask)) (performed_effect read))))
                       ((Value (Literal Unit))))
                      (Int Plus) (Value (Literal (Int 1))))))))))
               (return_clause ()))))
            ((Value
              (Lambda
               (()
                (Application
                 (Value
                  (Perform ((operation (User ask)) (performed_effect read))))
                 ((Value (Literal Unit)))))))))))))))) |}]
;;

let%expect_test "`fun` handler can implemnent `control` operation" =
  let declarations = [ M.Decl.Effect Util.Expr.decl_choose ] in
  let choose_handler =
    Util.Expr.singleton_handler
      ~op_name:(Variable.of_user "choose")
      ~op_argument:M.Parameter.Wildcard
      ~op_body:(E.Value (E.Literal (M.Literal.Bool true)))
      ~shape:Operation_shape.Fun
  in
  let body = E.Value (E.Handler choose_handler) in
  Util.print_expr_inference_result ~declarations body;
  [%expect
    {|
    (Ok
     ((Arrow
       ((Arrow () (Row (Open (Non_empty ((choose 1))) (Metavariable e0)))
         (Metavariable a1)))
       (Metavariable e0) (Metavariable a1))
      (Metavariable e3)
      (Value
       (Handler
        ((handled_effect choose)
         (operations
          (((User choose)
            (Fun
             ((op_argument Wildcard) (op_body (Value (Literal (Bool true)))))))))
         (return_clause ())))))) |}]
;;
