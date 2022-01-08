open Core
open Koka_zero_inference
module M = Minimal_syntax
module E = M.Expr
module UE = Util.Expr

let%expect_test "identity gets polymorphic type" =
  (* TODO: once introduce declarations, add a better test than this (expressions
     never have polymorphic type themselves) *)
  let expr =
    (* let id = \x. x in id id () *)
    (* tests id can be applied to functions, and units *)
    let id = UE.var "id" in
    E.Let
      ( M.Variable.of_user "id"
      , E.Lambda ([ M.Variable.of_user "z" ], UE.var "z")
      , E.Application (E.Application (id, [ id ]), [ UE.lit_unit ]) )
  in
  Util.print_expr_inference_result expr;
  [%expect {| (Ok ((Primitive Unit) (Metavariable e7))) |}]
;;

(* TODO: tests to add: static scoping, not generalising free varaibles, not
   generalising lambdas *)

let%expect_test "fix combinator allows recursion" =
  let expr =
    (* fix f. \x. f x *)
    E.Value
      (E.Fix_lambda
         ( M.Variable.of_user "f"
         , ( [ M.Variable.of_user "x" ]
           , E.Application (UE.var "f", [ UE.var "x" ]) ) ))
  in
  Util.print_expr_inference_result expr;
  [%expect
    {|
    (Ok
     ((Arrow ((Metavariable a2)) (Metavariable e3) (Metavariable a3))
      (Metavariable e4))) |}]
;;

let%expect_test "literal unit" =
  let expr = UE.lit_unit in
  Util.print_expr_inference_result expr;
  [%expect {| (Ok ((Primitive Unit) (Metavariable e0))) |}]
;;

let%expect_test "literal bool" =
  let expr = UE.lit_bool true in
  Util.print_expr_inference_result expr;
  [%expect {| (Ok ((Primitive Bool) (Metavariable e0))) |}]
;;

let%expect_test "literal int" =
  let expr = UE.lit_int 1 in
  Util.print_expr_inference_result expr;
  [%expect {| (Ok ((Primitive Int) (Metavariable e0))) |}]
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
  [%expect {| (Ok ((Primitive Int) (Metavariable e5))) |}]
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
          , E.Operator (i 3, oi M.Operator.Int.Less_than, i 5) )
      , ob M.Operator.Bool.Or
      , E.Operator (i 3, oi M.Operator.Int.Equals, i 5) )
  in
  Util.print_expr_inference_result expr;
  [%expect {| (Ok ((Primitive Bool) (Metavariable e5))) |}]
;;

let%expect_test "multi argument functions" =
  (* ( \b x y. if b then x else y )(true, 1, 0) *)
  let expr =
    E.Application
      ( E.Value
          (E.Lambda
             ( [ M.Variable.of_user "b"
               ; M.Variable.of_user "x"
               ; M.Variable.of_user "y"
               ]
             , E.If_then_else (UE.var "b", UE.var "x", UE.var "y") ))
      , [ UE.lit_bool true; UE.lit_int 1; UE.lit_int 0 ] )
  in
  Util.print_expr_inference_result expr;
  [%expect {| (Ok ((Primitive Int) (Metavariable e7))) |}]
;;

let%expect_test "declared functions are generalised" =
  let identity =
    M.Variable.of_user "id", ([ M.Variable.of_user "x" ], UE.var "x")
  in
  let const_true =
    ( M.Variable.of_user "const_true"
    , ([], E.Application (UE.var "id", [ UE.lit_bool true ])) )
  in
  let const_three =
    ( M.Variable.of_user "const_three"
    , ([], E.Application (UE.var "id", [ UE.lit_int 3 ])) )
  in
  let declarations =
    [ M.Decl.Fun identity; M.Decl.Fun const_true; M.Decl.Fun const_three ]
  in
  let program = { M.Program.declarations; has_main = false } in
  Util.print_check_program_result program;
  [%expect {| (Ok ()) |}]
;;

let%expect_test "sequence doesn't require first to be unit" =
  let e = E.Seq (UE.lit_int 0, UE.lit_bool true) in
  Util.print_expr_inference_result e;
  [%expect {| (Ok ((Primitive Bool) (Metavariable e1))) |}]
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
         ( [ M.Variable.of_user "f" ]
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
       ((Arrow () (Row ((labels ((exn 1) (read 2))) (tail ((Metavariable e26)))))
         (Primitive Unit)))
       (Metavariable e26) (Primitive Unit))
      (Metavariable e27))) |}]
;;

let%expect_test "return clause is typed correctly" =
  let declarations = [ M.Decl.Effect Util.Expr.decl_query ] in
  let query_handler =
    (* {[ handler { test(x){ resume( x == 3 ) }; return(b) { if b then () else
       () } } ]} *)
    let test_clause =
      let op_argument = M.Variable.of_user "x" in
      let op_body =
        E.Application
          ( E.Value (E.Variable M.Keyword.resume)
          , [ E.Operator
                ( E.Value (E.Variable op_argument)
                , M.Operator.Int M.Operator.Int.Equals
                , UE.lit_int 3 )
            ] )
      in
      { E.op_argument; op_body }
    in
    let return_clause =
      let op_argument = M.Variable.of_user "b" in
      let op_body =
        E.If_then_else
          (E.Value (E.Variable op_argument), UE.lit_unit, UE.lit_unit)
      in
      Some { E.op_argument; op_body }
    in
    let operations =
      M.Variable.Map.singleton (M.Variable.of_user "test") test_clause
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
  [%expect {| (Ok ((Primitive Unit) (Metavariable e16))) |}]
;;

let%expect_test "handlers can delegate to outer handlers" =
  let declarations = [ M.Decl.Effect Util.Expr.decl_read ] in
  let outer_handler = Util.Expr.read_handler 1 in
  (* { ask(unit) { ask(()) + 1 } } *)
  let inner_handler =
    let op_name = M.Variable.of_user "ask" in
    let op_argument = M.Variable.of_user "unit" in
    let op_body =
      E.Operator
        ( E.Application (UE.var "ask", [ UE.lit_unit ])
        , M.Operator.Int M.Operator.Int.Plus
        , UE.lit_int 1 )
    in
    Util.Expr.singleton_handler ~op_name ~op_argument ~op_body
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
  [%expect {| (Ok ((Primitive Int) (Metavariable e23))) |}]
;;
