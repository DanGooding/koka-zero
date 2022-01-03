open Core
open Koka_zero_inference
module M = Minimal_syntax
module E = M.Expr

let%expect_test "identity gets polymorphic type" =
  (* TODO: once introduce declarations, add a better test than this (expressions
     never have polymorphic type themselves) *)
  let expr =
    (* let id = \x. x in id id () *)
    (* tests id can be applied to functions, and units *)
    let id = E.Variable (M.Variable.of_string "id") in
    E.Let
      ( M.Variable.of_string "id"
      , E.Lambda
          ([ M.Variable.of_string "z" ], E.Variable (M.Variable.of_string "z"))
      , E.Application (E.Application (id, [ id ]), [ E.Literal M.Literal.Unit ])
      )
  in
  Util.print_expr_inference_result expr;
  [%expect {| (Ok ((Primitive Unit) (Metavariable e16))) |}]
;;

(* TODO: tests to add: static scoping, not generalising free varaibles, not
   generalising lambdas *)

let%expect_test "fix combinator allows recursion" =
  let expr =
    (* fix f. \x. f x *)
    E.Fix_lambda
      ( M.Variable.of_string "f"
      , ( [ M.Variable.of_string "x" ]
        , E.Application
            ( E.Variable (M.Variable.of_string "f")
            , [ E.Variable (M.Variable.of_string "x") ] ) ) )
  in
  Util.print_expr_inference_result expr;
  [%expect
    {|
    (Ok
     ((Arrow ((Metavariable a4)) (Metavariable e6) (Metavariable a6))
      (Metavariable e8))) |}]
;;

let%expect_test "literal unit" =
  let expr = E.Literal M.Literal.Unit in
  Util.print_expr_inference_result expr;
  [%expect {| (Ok ((Primitive Unit) (Metavariable e0))) |}]
;;

let%expect_test "literal bool" =
  let expr = E.Literal (M.Literal.Bool true) in
  Util.print_expr_inference_result expr;
  [%expect {| (Ok ((Primitive Bool) (Metavariable e0))) |}]
;;

let%expect_test "literal int" =
  let expr = E.Literal (M.Literal.Int 1) in
  Util.print_expr_inference_result expr;
  [%expect {| (Ok ((Primitive Int) (Metavariable e0))) |}]
;;

let%expect_test "int operators" =
  let i n = E.Literal (M.Literal.Int n) in
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
  [%expect {| (Ok ((Primitive Int) (Metavariable e10))) |}]
;;

let%expect_test "comparsion operators" =
  let i n = E.Literal (M.Literal.Int n) in
  let oi o = M.Operator.Int o in
  let ob o = M.Operator.Bool o in
  let expr =
    E.Operator
      ( E.Operator
          ( E.Operator (i 3, oi M.Operator.Int.Less_than, i 5)
          , ob M.Operator.Bool.Or
          , E.Operator (i 3, oi M.Operator.Int.Less_than, i 5) )
      , ob M.Operator.Bool.Or
      , E.Operator (i 3, oi M.Operator.Int.Equals, i 5) )
  in
  Util.print_expr_inference_result expr;
  [%expect {| (Ok ((Primitive Bool) (Metavariable e10))) |}]
;;

let%expect_test "multi argument functions" =
  (* ( \b x y. if b then x else y )(true, 1, 0) *)
  let expr =
    E.Application
      ( E.Lambda
          ( [ M.Variable.of_string "b"
            ; M.Variable.of_string "x"
            ; M.Variable.of_string "y"
            ]
          , E.If_then_else
              ( E.Variable (M.Variable.of_string "b")
              , E.Variable (M.Variable.of_string "x")
              , E.Variable (M.Variable.of_string "y") ) )
      , [ E.Literal (M.Literal.Bool true)
        ; E.Literal (M.Literal.Int 1)
        ; E.Literal (M.Literal.Int 0)
        ] )
  in
  Util.print_expr_inference_result expr;
  [%expect {| (Ok ((Primitive Int) (Metavariable e14))) |}]
;;

let%expect_test "declared functions are generalised" =
  let identity =
    ( M.Variable.of_string "id"
    , ([ M.Variable.of_string "x" ], E.Variable (M.Variable.of_string "x")) )
  in
  let const_true =
    ( M.Variable.of_string "const_true"
    , ( []
      , E.Application
          ( E.Variable (M.Variable.of_string "id")
          , [ E.Literal (M.Literal.Bool true) ] ) ) )
  in
  let const_three =
    ( M.Variable.of_string "const_three"
    , ( []
      , E.Application
          ( E.Variable (M.Variable.of_string "id")
          , [ E.Literal (M.Literal.Int 3) ] ) ) )
  in
  let declarations =
    [ M.Decl.Fun identity; M.Decl.Fun const_true; M.Decl.Fun const_three ]
  in
  let program = { M.Program.declarations; has_main = false } in
  Util.print_check_program_result program;
  [%expect {| (Ok ()) |}]
;;

let%expect_test "sequence doesn't require first to be unit" =
  let e =
    E.Seq (E.Literal (M.Literal.Int 0), E.Literal (M.Literal.Bool true))
  in
  Util.print_expr_inference_result e;
  [%expect {| (Ok ((Primitive Bool) (Metavariable e2))) |}]
;;

let%expect_test "handled effects reflected in subject's effect" =
  let declarations =
    [ M.Decl.Effect Util.Expr.decl_read; M.Decl.Effect Util.Expr.decl_exn ]
  in
  let body =
    (* \f. handler h_exn (\_. handler h_read (\_. handler h_read f)) *)
    (* (() -> <exn,read,read|e> a) -> e a *)
    E.Lambda
      ( [ M.Variable.of_string "f" ]
      , Util.Expr.make_handle_expr
          (Util.Expr.exn_handler (E.Literal M.Literal.Unit))
          (Util.Expr.make_handle_expr
             (Util.Expr.read_handler 1)
             (Util.Expr.make_handle_expr
                (Util.Expr.read_handler 1)
                (E.Application (E.Variable (M.Variable.of_string "f"), [])))) )
  in
  Util.print_expr_inference_result ~declarations body;
  [%expect
    {|
    (Ok
     ((Arrow
       ((Arrow () (Row ((labels ((exn 1) (read 2))) (tail ((Metavariable e52)))))
         (Primitive Unit)))
       (Metavariable e52) (Primitive Unit))
      (Metavariable e54))) |}]
;;

let%expect_test "return clause is typed correctly" =
  let declarations = [ M.Decl.Effect Util.Expr.decl_query ] in
  let query_handler =
    (* {[ handler { test(x){ resume( x == 3 ) }; return(b) { if b then () else
       () } } ]} *)
    let test_clause =
      let op_argument = M.Variable.of_string "x" in
      let op_body =
        E.Application
          ( E.Variable M.Keyword.resume
          , [ E.Operator
                ( E.Variable op_argument
                , M.Operator.Int M.Operator.Int.Equals
                , E.Literal (M.Literal.Int 3) )
            ] )
      in
      { E.op_argument; op_body }
    in
    let return_clause =
      let op_argument = M.Variable.of_string "b" in
      let op_body =
        E.If_then_else
          ( E.Variable op_argument
          , E.Literal M.Literal.Unit
          , E.Literal M.Literal.Unit )
      in
      Some { E.op_argument; op_body }
    in
    let operations =
      M.Variable.Map.singleton (M.Variable.of_string "test") test_clause
    in
    { E.operations; return_clause }
  in
  let body =
    (* handle h_query (test(5)) *)
    Util.Expr.make_handle_expr
      query_handler
      (E.Application
         ( E.Variable (M.Variable.of_string "test")
         , [ E.Literal (M.Literal.Int 5) ] ))
  in
  Util.print_expr_inference_result ~declarations body;
  [%expect {| (Ok ((Primitive Unit) (Metavariable e32))) |}]
;;

let%expect_test "handlers can delegate to outer handlers" =
  let declarations = [ M.Decl.Effect Util.Expr.decl_read ] in
  let outer_handler = Util.Expr.read_handler 1 in
  (* { ask(unit) { ask(()) + 1 } } *)
  let inner_handler =
    let op_name = M.Variable.of_string "ask" in
    let op_argument = M.Variable.of_string "unit" in
    let op_body =
      E.Operator
        ( E.Application
            ( E.Variable (M.Variable.of_string "ask")
            , [ E.Literal M.Literal.Unit ] )
        , M.Operator.Int M.Operator.Int.Plus
        , E.Literal (M.Literal.Int 1) )
    in
    Util.Expr.singleton_handler ~op_name ~op_argument ~op_body
  in
  (* handle outer (handle inner (ask(()) )) *)
  let body =
    Util.Expr.make_handle_expr
      outer_handler
      (Util.Expr.make_handle_expr
         inner_handler
         (E.Application
            ( E.Variable (M.Variable.of_string "ask")
            , [ E.Literal M.Literal.Unit ] )))
  in
  Util.print_expr_inference_result ~declarations body;
  [%expect {| (Ok ((Primitive Int) (Metavariable e46))) |}]
;;
