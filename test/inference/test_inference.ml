open Core
open Koka_zero_inference
module M = Minimal_syntax
module E = M.Expr

let%expect_test "occurs check rejects omega combinator" =
  let expr =
    E.Lambda
      ( M.Variable.of_string "x"
      , E.Application
          ( E.Variable (M.Variable.of_string "x")
          , E.Variable (M.Variable.of_string "x") ) )
  in
  Util.print_inference_result expr;
  [%expect {| |}]
;;

let%expect_test "identity gets polymorphic type" =
  let expr =
    E.Let
      ( M.Variable.of_string "id"
      , E.Lambda
          (M.Variable.of_string "z", E.Variable (M.Variable.of_string "z"))
      , E.Variable (M.Variable.of_string "id") )
  in
  Util.print_inference_result expr;
  [%expect {| |}]
;;
