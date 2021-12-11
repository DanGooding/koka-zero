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
  Util.print_expr_inference_result expr;
  [%expect
    {|
    (Error
     ((kind Type_error)
      (message
        "cannot unify\
       \n(Metavariable $m0)\
       \nwith\
       \n(Arrow (Metavariable $m0) (Metavariable @m0) (Metavariable $m2))\
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
