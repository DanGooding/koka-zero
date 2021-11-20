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
  Util.print_inference_result expr;
  [%expect {| (Ok (Primitive Unit)) |}]
;;

(* TODO: tests to add: static scoping, not generalising free varaibles, not
   gemeralising lambdas fix working properly *)
