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
  [%expect {| (Ok ((Primitive Unit) (Metavariable e14))) |}]
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
     ((Arrow (Metavariable a4) (Metavariable e4) (Metavariable a6))
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

let decl_read =
  let name = Effect.Label.of_string "read" in
  let op_ask =
    let argument = Type.Mono.Primitive Type.Primitive.Unit in
    let answer = Type.Mono.Primitive Type.Primitive.Int in
    { M.Effect_decl.Operation.argument; answer }
  in
  let operations =
    M.Variable.Map.singleton (M.Variable.of_string "ask") op_ask
  in
  { M.Effect_decl.name; operations }
;;

let decl_exn =
  let name = Effect.Label.of_string "exn" in
  let op_ask =
    let argument = Type.Mono.Primitive Type.Primitive.Unit in
    (* TODO: this should be forall a. a, once polymorphic effects are added *)
    let answer = Type.Mono.Primitive Type.Primitive.Unit in
    { M.Effect_decl.Operation.argument; answer }
  in
  let operations =
    M.Variable.Map.singleton (M.Variable.of_string "throw") op_ask
  in
  { M.Effect_decl.name; operations }
;;

let decl_query =
  let name = Effect.Label.of_string "query" in
  let op_ask =
    let argument = Type.Mono.Primitive Type.Primitive.Int in
    (* TODO: this should be forall a. a, once polymorphic effects are added *)
    let answer = Type.Mono.Primitive Type.Primitive.Bool in
    { M.Effect_decl.Operation.argument; answer }
  in
  let operations =
    M.Variable.Map.singleton (M.Variable.of_string "test") op_ask
  in
  { M.Effect_decl.name; operations }
;;

let singleton_handler
    ~(op_name : M.Variable.t)
    ~(op_argument : M.Variable.t)
    ~(op_body : E.t)
  =
  let clause = { E.op_argument; op_body } in
  let operations = M.Variable.Map.singleton op_name clause in
  { E.operations; return_clause = None }
;;

let read_handler (value : int) =
  (* handler { ask(unit) { resume(value) } } *)
  let op_name = M.Variable.of_string "ask" in
  let op_argument = M.Variable.of_string "unit" in
  let op_body =
    E.Application (E.Variable M.Keyword.resume, E.Literal (M.Literal.Int value))
  in
  singleton_handler ~op_name ~op_argument ~op_body
;;

(* handler { throw(unit) { default } } *)
let exn_handler default =
  let throw_clause =
    let op_argument = M.Variable.of_string "unit" in
    let op_body = default in
    { E.op_argument; op_body }
  in
  let operations =
    M.Variable.Map.singleton (M.Variable.of_string "throw") throw_clause
  in
  { E.operations; return_clause = None }
;;

let%expect_test "handled effects reflected in subject's effect" =
  let effect_declarations = [ decl_read; decl_exn ] in
  let body =
    (* \f. handle h_exn (handle h_read (handle h_read f)) *)
    (* (() -> <exn,read,read|e> a) -> e a *)
    E.Lambda
      ( M.Variable.of_string "f"
      , E.Handle
          ( exn_handler (E.Literal M.Literal.Unit)
          , E.Handle
              ( read_handler 1
              , E.Handle
                  ( read_handler 1
                  , E.Application
                      ( E.Variable (M.Variable.of_string "f")
                      , E.Literal M.Literal.Unit ) ) ) ) )
  in
  let program = { M.Program.effect_declarations; body } in
  Util.print_inference_result program;
  [%expect
    {|
    (Ok
     ((Arrow
       (Arrow (Primitive Unit)
        (Row ((labels ((exn 1) (read 2))) (tail ((Metavariable e24)))))
        (Primitive Unit))
       (Metavariable e24) (Primitive Unit))
      (Metavariable e32))) |}]
;;

let%expect_test "return clause is typed correctly" =
  let effect_declarations = [ decl_query ] in
  let query_handler =
    (* {[ handler { test(x){ resume( x == 3 ) }; return(b) { if b then () else
       () } } ]} *)
    let test_clause =
      let op_argument = M.Variable.of_string "x" in
      let op_body =
        E.Application
          ( E.Variable M.Keyword.resume
          , E.Operator
              ( E.Variable op_argument
              , M.Operator.Int M.Operator.Int.Equals
              , E.Literal (M.Literal.Int 3) ) )
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
    E.Handle
      ( query_handler
      , E.Application
          (E.Variable (M.Variable.of_string "test"), E.Literal (M.Literal.Int 5))
      )
  in
  let program = { M.Program.effect_declarations; body } in
  Util.print_inference_result program;
  [%expect {| (Ok ((Primitive Unit) (Metavariable e22))) |}]
;;

let%expect_test "handlers can delegate to outer handlers" =
  let effect_declarations = [ decl_read ] in
  let outer_handler = read_handler 1 in
  (* { ask(unit) { ask(()) + 1 } } *)
  let inner_handler =
    let op_name = M.Variable.of_string "ask" in
    let op_argument = M.Variable.of_string "unit" in
    let op_body =
      E.Operator
        ( E.Application
            (E.Variable (M.Variable.of_string "ask"), E.Literal M.Literal.Unit)
        , M.Operator.Int M.Operator.Int.Plus
        , E.Literal (M.Literal.Int 1) )
    in
    singleton_handler ~op_name ~op_argument ~op_body
  in
  (* handle outer (handle inner (ask(()) )) *)
  let body =
    E.Handle
      ( outer_handler
      , E.Handle
          ( inner_handler
          , E.Application
              (E.Variable (M.Variable.of_string "ask"), E.Literal M.Literal.Unit)
          ) )
  in
  let program = { M.Program.effect_declarations; body } in
  Util.print_inference_result program;
  [%expect {| (Ok ((Primitive Int) (Metavariable e28))) |}]
;;
