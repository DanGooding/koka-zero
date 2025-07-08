open! Core
open! Import
module UE = Util.Expr
module UP = Util.Parameter

let%expect_test
    "recursive types allow the omega combinator - but we can't use it"
  =
  let omega =
    E.Value
      (E.Lambda ([ UP.var "x" ], E.Application (UE.var "x", [ UE.var "x" ])))
  in
  Util.print_expr_inference_result omega;
  (* this has type [Rec(t0. t0 -> _ t1) -> _ t1] *)
  [%expect
    {|
    (Ok
     ((Arrow
       ((Recursive t0
         (Intersection ((Arrow ((Variable t0)) (Variable e0) (Variable t1))))))
       (Union ((Labels ()))) (Variable t1))
      (Labels ())
      (Value
       (Lambda
        (((Variable (User x)))
         (Application (Value (Variable (User x))) ((Value (Variable (User x))))))))))
    |}];
  (* here we try to type [omega (fun x -> x) ()]
     which should expand to [(fun x -> x) (fun x -> x) ()] which is just unit.
     However we get a type error - because it doesn't take [x] as polymorphic
     (which would require higher kinded polymorphism).
  *)
  let expr =
    E.Application
      ( E.Application
          (omega, [ E.Value (E.Lambda ([ UP.var "y" ], UE.var "y")) ])
      , [ E.Value (E.Literal Unit) ] )
  in
  Util.print_expr_inference_result expr;
  [%expect
    {|
    (Error
     (("error when expanding constraint" (type_lo (Metavariable tm3))
       (type_hi
        (Arrow ((Primitive Unit)) (Unknown (Metavariable em2))
         (Metavariable tm4))))
      ("type error: cannot relate" (type_lo (Primitive Unit))
       (type_hi
        (Arrow ((Primitive Unit)) (Unknown (Metavariable em2))
         (Metavariable tm4))))))
    |}]
;;

let%expect_test "if statement's branch types are unioned" =
  (* if true then 1 else () *)
  let expr = E.If_then_else (UE.lit_bool true, UE.lit_int 1, UE.lit_unit) in
  Util.print_expr_inference_result expr;
  (* this isn't a type error, however code cannot do anything with the value of
     type Union [Int; Bool] - since it doesn't know which case applies. *)
  [%expect
    {|
    (Ok
     ((Union ((Primitive Unit) (Primitive Int))) (Union ((Labels ())))
      (If_then_else (Value (Literal (Bool true))) (Value (Literal (Int 1)))
       (Value (Literal Unit)))))
    |}]
;;

let%expect_test "fix lambdas name cannot collide with own parameters" =
  let expr =
    E.Value
      (E.Fix_lambda
         (Variable.of_user "f", ([ UP.var "g"; UP.var "f" ], UE.lit_unit)))
  in
  Util.print_expr_inference_result expr;
  [%expect
    {|
    (Ok
     ((Arrow ((Variable t0) (Variable t1)) (Labels ()) (Primitive Unit))
      (Labels ())
      (Value
       (Fix_lambda
        ((User f)
         (((Variable (User g)) (Variable (User f))) (Value (Literal Unit))))))))
    |}]
;;

let%expect_test "cannot shadow functions at toplevel" =
  let declarations =
    [ M.Decl.Fun (Variable.of_user "foo", ([], UE.lit_unit))
    ; M.Decl.Fun (Variable.of_user "foo", ([], UE.lit_unit))
    ]
  in
  let program = { M.Program.declarations } in
  Util.print_check_program_without_main_result program;
  [%expect {| (Error "cannot shadow 'foo' at toplevel") |}]
;;

let%expect_test "handler must include all operations" =
  let declarations = [ M.Decl.Effect Util.Expr.decl_state ] in
  let state_handler_set_only =
    (* handler { fun set(x) { () } } *)
    let set_clause =
      let op_argument = UP.var "x" in
      let op_body = UE.lit_unit in
      { E.op_argument; op_body }
    in
    let operations =
      Variable.Map.singleton
        (Variable.of_user "set")
        (Operation_shape.Fun, set_clause)
    in
    { E.operations; return_clause = None }
  in
  let body = Util.Expr.make_handle_expr state_handler_set_only UE.lit_unit in
  Util.print_expr_inference_result ~declarations body;
  [%expect {| (Error "handler does not match any effect: ((User set))") |}]
;;

let%expect_test "`control` handler is not allowed to implemnent `fun` operation"
  =
  let declarations = [ M.Decl.Effect Util.Expr.decl_read ] in
  let read_handler =
    Util.Expr.singleton_handler
      ~op_name:(Variable.of_user "ask")
      ~op_argument:Parameter.Wildcard
      ~op_body:
        (E.Application
           ( E.Value (E.Variable Keyword.resume)
           , [ E.Value (E.Literal (Literal.Int 3)) ] ))
      ~shape:Operation_shape.Control
  in
  let body = E.Value (E.Handler read_handler) in
  Util.print_expr_inference_result ~declarations body;
  [%expect
    {|
    (Error
     "cannot handle operation `ask` declared as `fun` with `control` clause")
    |}]
;;

let%expect_test "`fun` clause cannot use `resume`" =
  let declarations = [ M.Decl.Effect Util.Expr.decl_read ] in
  let read_handler =
    Util.Expr.singleton_handler
      ~op_name:(Variable.of_user "ask")
      ~op_argument:Parameter.Wildcard
      ~op_body:
        (E.Application
           ( E.Value (E.Variable Keyword.resume)
           , [ E.Value (E.Literal (Literal.Int 3)) ] ))
      ~shape:Operation_shape.Fun
  in
  let body = E.Value (E.Handler read_handler) in
  Util.print_expr_inference_result ~declarations body;
  [%expect {| (Error "unbound variable: resume") |}]
;;
