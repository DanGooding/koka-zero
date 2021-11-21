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
  [%expect
    {|
    (Error
     ((kind Type_error)
      (message
        "cannot unify\
       \n(Metavariable $m0)\
       \nwith\
       \n(Arrow (Metavariable $m0) (Metavariable $m2))\
       \n")
      (location ()))) |}]
;;
