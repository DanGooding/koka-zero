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
          (M.Variable.of_string "z", E.Variable (M.Variable.of_string "z"))
      , E.Application (E.Application (id, id), E.Literal M.Literal.Unit) )
  in
  Util.print_expr_inference_result expr;
  [%expect {| (Ok ((Primitive Unit) (Metavariable @m14))) |}]
;;

(* TODO: tests to add: static scoping, not generalising free varaibles, not
   gemeralising lambdas fix working properly *)

let%expect_test "fix combinator allows recursion" =
  let expr =
    (* fix f. \x. f x *)
    E.Fix
      ( M.Variable.of_string "f"
      , E.Lambda
          ( M.Variable.of_string "x"
          , E.Application
              ( E.Variable (M.Variable.of_string "f")
              , E.Variable (M.Variable.of_string "x") ) ) )
  in
  Util.print_expr_inference_result expr;
  [%expect
    {|
    (Ok
     ((Arrow (Metavariable $m2) (Metavariable @m2) (Metavariable $m4))
      (Metavariable @m4))) |}]
;;

let%expect_test "literal unit" =
  let expr = E.Literal M.Literal.Unit in
  Util.print_expr_inference_result expr;
  [%expect {| (Ok ((Primitive Unit) (Metavariable @m0))) |}]
;;

let%expect_test "literal bool" =
  let expr = E.Literal (M.Literal.Bool true) in
  Util.print_expr_inference_result expr;
  [%expect {| (Ok ((Primitive Bool) (Metavariable @m0))) |}]
;;

let%expect_test "literal int" =
  let expr = E.Literal (M.Literal.Int 1) in
  Util.print_expr_inference_result expr;
  [%expect {| (Ok ((Primitive Int) (Metavariable @m0))) |}]
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
  [%expect {| (Ok ((Primitive Int) (Metavariable @m10))) |}]
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
  [%expect {| (Ok ((Primitive Bool) (Metavariable @m10))) |}]
;;
