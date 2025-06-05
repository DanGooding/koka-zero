open! Core

let%expect_test "toplevel value declaration" =
  let code =
    {|
  val number : int = 1729;
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_val
         ((id (Var number))
          (type_ ((Type_atom (constructor Type_int) (arguments ())))))
         ((statements ()) (last (Literal (Int 1729))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Error
     ((kind Unsupported_feature) (message "toplevel val binding") (location ()))) |}]
;;

let%expect_test "single expression function" =
  let code =
    {|
fun main() {
  0;
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var main))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body ((statements ()) (last (Literal (Int 0))))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok ((declarations ((Fun ((User main) (() (Value (Literal (Int 0)))))))))) |}]
;;

let%expect_test "multi statement function" =
  let code =
    {|
fun main() {
  val x = 1;
  print(x);
  val y = foo(x);
  y * 2;
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var main))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body
             ((statements
               ((Declaration
                 (Val ((pattern (Pattern_id (Var x))) (scheme ()))
                  ((statements ()) (last (Literal (Int 1))))))
                (Expr
                 (Application (Identifier (Var print)) ((Identifier (Var x)))))
                (Declaration
                 (Val ((pattern (Pattern_id (Var y))) (scheme ()))
                  ((statements ())
                   (last
                    (Application (Identifier (Var foo)) ((Identifier (Var x))))))))))
              (last (Binary_op (Identifier (Var y)) Times (Literal (Int 2)))))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun
         ((User main)
          (()
           (Let_mono (User x) (Value (Literal (Int 1)))
            (Seq
             (Application (Value (Variable (User print)))
              ((Value (Variable (User x)))))
             (Let_mono (User y)
              (Application (Value (Variable (User foo)))
               ((Value (Variable (User x)))))
              (Operator (Value (Variable (User y))) (Int Times)
               (Value (Literal (Int 2)))))))))))))) |}]
;;

let%expect_test "dashes in identifiers" =
  let code =
    {|
fun wrapper() {
  val kebab-case = 0;
  val x-y-z = 1;
  val number3-letter = 2;
  ();
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var wrapper))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body
             ((statements
               ((Declaration
                 (Val ((pattern (Pattern_id (Var kebab-case))) (scheme ()))
                  ((statements ()) (last (Literal (Int 0))))))
                (Declaration
                 (Val ((pattern (Pattern_id (Var x-y-z))) (scheme ()))
                  ((statements ()) (last (Literal (Int 1))))))
                (Declaration
                 (Val ((pattern (Pattern_id (Var number3-letter))) (scheme ()))
                  ((statements ()) (last (Literal (Int 2))))))))
              (last (Literal Unit)))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun
         ((User wrapper)
          (()
           (Let_mono (User kebab-case) (Value (Literal (Int 0)))
            (Let_mono (User x-y-z) (Value (Literal (Int 1)))
             (Let_mono (User number3-letter) (Value (Literal (Int 2)))
              (Value (Literal Unit)))))))))))) |}]
;;

let%expect_test "hex literals" =
  let code =
    {|
fun wrapper() {
  0x1234ABCD;
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var wrapper))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body ((statements ()) (last (Literal (Int 305441741))))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun ((User wrapper) (() (Value (Literal (Int 305441741)))))))))) |}]
;;

let%expect_test "prime at end of identifier" =
  let code =
    {|
fun wrapper() {
  val f' = diff(f);
  val f'' = diff(f');
  ();
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var wrapper))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body
             ((statements
               ((Declaration
                 (Val ((pattern (Pattern_id (Var f'))) (scheme ()))
                  ((statements ())
                   (last
                    (Application (Identifier (Var diff)) ((Identifier (Var f))))))))
                (Declaration
                 (Val ((pattern (Pattern_id (Var f''))) (scheme ()))
                  ((statements ())
                   (last
                    (Application (Identifier (Var diff)) ((Identifier (Var f'))))))))))
              (last (Literal Unit)))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun
         ((User wrapper)
          (()
           (Let_mono (User f')
            (Application (Value (Variable (User diff)))
             ((Value (Variable (User f)))))
            (Let_mono (User f'')
             (Application (Value (Variable (User diff)))
              ((Value (Variable (User f')))))
             (Value (Literal Unit))))))))))) |}]
;;

let%expect_test "wildcard parameter" =
  let code =
    {|
fun foo(_a, b, _c, _, e, _f) { () };
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var foo))
          (fn
           ((type_parameters ())
            (parameters
             (((pattern Pattern_wildcard) (type_ ()))
              ((pattern (Pattern_id (Var b))) (type_ ()))
              ((pattern Pattern_wildcard) (type_ ()))
              ((pattern Pattern_wildcard) (type_ ()))
              ((pattern (Pattern_id (Var e))) (type_ ()))
              ((pattern Pattern_wildcard) (type_ ()))))
            (result_type ()) (body ((statements ()) (last (Literal Unit)))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun
         ((User foo)
          ((Wildcard (Variable (User b)) Wildcard Wildcard (Variable (User e))
            Wildcard)
           (Value (Literal Unit))))))))) |}]
;;

let%expect_test "operators" =
  let code =
    {|
fun hypotenuse(a, b) {
  val c-squared = a * a + b * b;
  val c = isqrt(c-squared);
  c;
}

fun main() {
  val all = 12 + 33 * 44 - 36 / 4 + 91 % 7 + 11;
  val inside = 0 <= x && x < 7 || 100 < x && x >= 9000;
  ();
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var hypotenuse))
          (fn
           ((type_parameters ())
            (parameters
             (((pattern (Pattern_id (Var a))) (type_ ()))
              ((pattern (Pattern_id (Var b))) (type_ ()))))
            (result_type ())
            (body
             ((statements
               ((Declaration
                 (Val ((pattern (Pattern_id (Var c-squared))) (scheme ()))
                  ((statements ())
                   (last
                    (Binary_op
                     (Binary_op (Identifier (Var a)) Times (Identifier (Var a)))
                     Plus
                     (Binary_op (Identifier (Var b)) Times (Identifier (Var b))))))))
                (Declaration
                 (Val ((pattern (Pattern_id (Var c))) (scheme ()))
                  ((statements ())
                   (last
                    (Application (Identifier (Var isqrt))
                     ((Identifier (Var c-squared))))))))))
              (last (Identifier (Var c))))))))))
       (Pure_declaration
        (Top_fun
         ((id (Var main))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body
             ((statements
               ((Declaration
                 (Val ((pattern (Pattern_id (Var all))) (scheme ()))
                  ((statements ())
                   (last
                    (Binary_op
                     (Binary_op
                      (Binary_op
                       (Binary_op (Literal (Int 12)) Plus
                        (Binary_op (Literal (Int 33)) Times (Literal (Int 44))))
                       Minus
                       (Binary_op (Literal (Int 36)) Divide (Literal (Int 4))))
                      Plus
                      (Binary_op (Literal (Int 91)) Modulo (Literal (Int 7))))
                     Plus (Literal (Int 11)))))))
                (Declaration
                 (Val ((pattern (Pattern_id (Var inside))) (scheme ()))
                  ((statements ())
                   (last
                    (Binary_op
                     (Binary_op
                      (Binary_op (Literal (Int 0)) Less_equal
                       (Identifier (Var x)))
                      And
                      (Binary_op (Identifier (Var x)) Less_than
                       (Literal (Int 7))))
                     Or
                     (Binary_op
                      (Binary_op (Literal (Int 100)) Less_than
                       (Identifier (Var x)))
                      And
                      (Binary_op (Identifier (Var x)) Greater_equal
                       (Literal (Int 9000)))))))))))
              (last (Literal Unit))))))))))))
|}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun
         ((User hypotenuse)
          (((Variable (User a)) (Variable (User b)))
           (Let_mono (User c-squared)
            (Operator
             (Operator (Value (Variable (User a))) (Int Times)
              (Value (Variable (User a))))
             (Int Plus)
             (Operator (Value (Variable (User b))) (Int Times)
              (Value (Variable (User b)))))
            (Let_mono (User c)
             (Application (Value (Variable (User isqrt)))
              ((Value (Variable (User c-squared)))))
             (Value (Variable (User c))))))))
        (Fun
         ((User main)
          (()
           (Let_mono (User all)
            (Operator
             (Operator
              (Operator
               (Operator (Value (Literal (Int 12))) (Int Plus)
                (Operator (Value (Literal (Int 33))) (Int Times)
                 (Value (Literal (Int 44)))))
               (Int Minus)
               (Operator (Value (Literal (Int 36))) (Int Divide)
                (Value (Literal (Int 4)))))
              (Int Plus)
              (Operator (Value (Literal (Int 91))) (Int Modulo)
               (Value (Literal (Int 7)))))
             (Int Plus) (Value (Literal (Int 11))))
            (Let_mono (User inside)
             (Operator
              (Operator
               (Operator (Value (Literal (Int 0))) (Int Less_equal)
                (Value (Variable (User x))))
               (Bool And)
               (Operator (Value (Variable (User x))) (Int Less_than)
                (Value (Literal (Int 7)))))
              (Bool Or)
              (Operator
               (Operator (Value (Literal (Int 100))) (Int Less_than)
                (Value (Variable (User x))))
               (Bool And)
               (Operator (Value (Variable (User x))) (Int Greater_equal)
                (Value (Literal (Int 9000))))))
             (Value (Literal Unit))))))))))) |}]
;;

let%expect_test "negative integer literals" =
  let code =
    {|
fun wrapper() {
  val minus-fourty = -40;
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Error
     ((kind Syntax_error) (message "parse error")
      (location (((filename ()) (line 4) (char 2)))))) |}];
  Util.print_simplification_result syntax;
  [%expect {| |}]
;;

let%expect_test "boolean literals" =
  let code =
    {|
fun wrapper() {
  val t = True;
  val f = False;
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Error
     ((kind Syntax_error) (message "parse error")
      (location (((filename ()) (line 5) (char 2)))))) |}];
  Util.print_simplification_result syntax;
  [%expect {| |}]
;;

let%expect_test "if statements" =
  let code =
    {|
fun if-example() {
  if x % 2 == 0 then
    123
  elif y % 2 == 0 then
    400 + 50 + 6
  elif False then
    -1
  else
    1 + 1;
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var if-example))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body
             ((statements ())
              (last
               (If_then_else
                (Binary_op
                 (Binary_op (Identifier (Var x)) Modulo (Literal (Int 2))) Equals
                 (Literal (Int 0)))
                ((statements ()) (last (Literal (Int 123))))
                ((statements ())
                 (last
                  (If_then_else
                   (Binary_op
                    (Binary_op (Identifier (Var y)) Modulo (Literal (Int 2)))
                    Equals (Literal (Int 0)))
                   ((statements ())
                    (last
                     (Binary_op
                      (Binary_op (Literal (Int 400)) Plus (Literal (Int 50)))
                      Plus (Literal (Int 6)))))
                   ((statements ())
                    (last
                     (If_then_else (Literal (Bool false))
                      ((statements ()) (last (Literal (Int -1))))
                      ((statements ())
                       (last
                        (Binary_op (Literal (Int 1)) Plus (Literal (Int 1))))))))))))))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun
         ((User if-example)
          (()
           (If_then_else
            (Operator
             (Operator (Value (Variable (User x))) (Int Modulo)
              (Value (Literal (Int 2))))
             (Int Equals) (Value (Literal (Int 0))))
            (Value (Literal (Int 123)))
            (If_then_else
             (Operator
              (Operator (Value (Variable (User y))) (Int Modulo)
               (Value (Literal (Int 2))))
              (Int Equals) (Value (Literal (Int 0))))
             (Operator
              (Operator (Value (Literal (Int 400))) (Int Plus)
               (Value (Literal (Int 50))))
              (Int Plus) (Value (Literal (Int 6))))
             (If_then_else (Value (Literal (Bool false)))
              (Value (Literal (Int -1)))
              (Operator (Value (Literal (Int 1))) (Int Plus)
               (Value (Literal (Int 1)))))))))))))) |}]
;;

let%expect_test "nested if statements" =
  (* TODO: note that indentation is ignored - a dangling `else` always
     associates to the innermost `if`. This may be unintuitive and perhaps
     should be changed. see koka's layout algorithm, which warns when
     indentation doesn't match the parse *)
  let code =
    {|
fun i() {
  if a then
    if b then
      c
  else d;
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var i))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body
             ((statements ())
              (last
               (If_then (Identifier (Var a))
                ((statements ())
                 (last
                  (If_then_else (Identifier (Var b))
                   ((statements ()) (last (Identifier (Var c))))
                   ((statements ()) (last (Identifier (Var d))))))))))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun
         ((User i)
          (()
           (If_then_else (Value (Variable (User a)))
            (If_then_else (Value (Variable (User b))) (Value (Variable (User c)))
             (Value (Variable (User d))))
            (Value (Literal Unit)))))))))) |}]
;;

let%expect_test "if statement body" =
  let code =
    {|
fun i() {
  if condition then {
    val x = 101;
    print(x % 7);
  };
  if b then {
    aaa();
    bbb();
  } else {
    ccc();
    ddd();
  };
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var i))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body
             ((statements
               ((Expr
                 (If_then (Identifier (Var condition))
                  ((statements
                    ((Declaration
                      (Val ((pattern (Pattern_id (Var x))) (scheme ()))
                       ((statements ()) (last (Literal (Int 101))))))))
                   (last
                    (Application (Identifier (Var print))
                     ((Binary_op (Identifier (Var x)) Modulo (Literal (Int 7)))))))))))
              (last
               (If_then_else (Identifier (Var b))
                ((statements ((Expr (Application (Identifier (Var aaa)) ()))))
                 (last (Application (Identifier (Var bbb)) ())))
                ((statements ((Expr (Application (Identifier (Var ccc)) ()))))
                 (last (Application (Identifier (Var ddd)) ()))))))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun
         ((User i)
          (()
           (Seq
            (If_then_else (Value (Variable (User condition)))
             (Let_mono (User x) (Value (Literal (Int 101)))
              (Application (Value (Variable (User print)))
               ((Operator (Value (Variable (User x))) (Int Modulo)
                 (Value (Literal (Int 7)))))))
             (Value (Literal Unit)))
            (If_then_else (Value (Variable (User b)))
             (Seq (Application (Value (Variable (User aaa))) ())
              (Application (Value (Variable (User bbb))) ()))
             (Seq (Application (Value (Variable (User ccc))) ())
              (Application (Value (Variable (User ddd))) ()))))))))))) |}]
;;

let%expect_test "dot application" =
  let code =
    {|
fun dot-application() {
  x.best.fst.pow(3).print;
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var dot-application))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body
             ((statements ())
              (last
               (Application (Identifier (Var print))
                ((Application (Identifier (Var pow))
                  ((Application (Identifier (Var fst))
                    ((Application (Identifier (Var best)) ((Identifier (Var x))))))
                   (Literal (Int 3))))))))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun
         ((User dot-application)
          (()
           (Application (Value (Variable (User print)))
            ((Application (Value (Variable (User pow)))
              ((Application (Value (Variable (User fst)))
                ((Application (Value (Variable (User best)))
                  ((Value (Variable (User x)))))))
               (Value (Literal (Int 3)))))))))))))) |}]
;;

let%expect_test "trailing lambda application" =
  let code =
    {|
fun trailing-lambda() {
  for(1, 10) fn(i) { println(i * i) };
  f(x,y,z) { alpha } fn(b) {beta} {gamma};
  a.g(1).h(2) { zzz };
}
  |}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var trailing-lambda))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body
             ((statements
               ((Expr
                 (Application (Identifier (Var for))
                  ((Literal (Int 1)) (Literal (Int 10))
                   (Fn
                    ((type_parameters ())
                     (parameters (((pattern (Pattern_id (Var i))) (type_ ()))))
                     (result_type ())
                     (body
                      ((statements ())
                       (last
                        (Application (Identifier (Var println))
                         ((Binary_op (Identifier (Var i)) Times
                           (Identifier (Var i)))))))))))))
                (Expr
                 (Application (Identifier (Var f))
                  ((Identifier (Var x)) (Identifier (Var y)) (Identifier (Var z))
                   (Fn
                    ((type_parameters ()) (parameters ()) (result_type ())
                     (body ((statements ()) (last (Identifier (Var alpha)))))))
                   (Fn
                    ((type_parameters ())
                     (parameters (((pattern (Pattern_id (Var b))) (type_ ()))))
                     (result_type ())
                     (body ((statements ()) (last (Identifier (Var beta)))))))
                   (Fn
                    ((type_parameters ()) (parameters ()) (result_type ())
                     (body ((statements ()) (last (Identifier (Var gamma))))))))))))
              (last
               (Application (Identifier (Var h))
                ((Application (Identifier (Var g))
                  ((Identifier (Var a)) (Literal (Int 1))))
                 (Literal (Int 2))
                 (Fn
                  ((type_parameters ()) (parameters ()) (result_type ())
                   (body ((statements ()) (last (Identifier (Var zzz))))))))))))))))))))
    |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun
         ((User trailing-lambda)
          (()
           (Seq
            (Application (Value (Variable (User for)))
             ((Value (Literal (Int 1))) (Value (Literal (Int 10)))
              (Value
               (Lambda
                (((Variable (User i)))
                 (Application (Value (Variable (User println)))
                  ((Operator (Value (Variable (User i))) (Int Times)
                    (Value (Variable (User i)))))))))))
            (Seq
             (Application (Value (Variable (User f)))
              ((Value (Variable (User x))) (Value (Variable (User y)))
               (Value (Variable (User z)))
               (Value (Lambda (() (Value (Variable (User alpha))))))
               (Value
                (Lambda (((Variable (User b))) (Value (Variable (User beta))))))
               (Value (Lambda (() (Value (Variable (User gamma))))))))
             (Application (Value (Variable (User h)))
              ((Application (Value (Variable (User g)))
                ((Value (Variable (User a))) (Value (Literal (Int 1)))))
               (Value (Literal (Int 2)))
               (Value (Lambda (() (Value (Variable (User zzz))))))))))))))))) |}]
;;

let%expect_test "with syntax" =
  let code =
    {|
fun one(aa, bb, cc, dd) {
  val z = 1;
  with aa;
  println(zz);
  with bb();
  with cc(3);
  with x <- dd(5);
  println(x);
}
  |}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var one))
          (fn
           ((type_parameters ())
            (parameters
             (((pattern (Pattern_id (Var aa))) (type_ ()))
              ((pattern (Pattern_id (Var bb))) (type_ ()))
              ((pattern (Pattern_id (Var cc))) (type_ ()))
              ((pattern (Pattern_id (Var dd))) (type_ ()))))
            (result_type ())
            (body
             ((statements
               ((Declaration
                 (Val ((pattern (Pattern_id (Var z))) (scheme ()))
                  ((statements ()) (last (Literal (Int 1))))))))
              (last
               (Application (Identifier (Var aa))
                ((Fn
                  ((type_parameters ()) (parameters ()) (result_type ())
                   (body
                    ((statements
                      ((Expr
                        (Application (Identifier (Var println))
                         ((Identifier (Var zz)))))))
                     (last
                      (Application (Identifier (Var bb))
                       ((Fn
                         ((type_parameters ()) (parameters ()) (result_type ())
                          (body
                           ((statements ())
                            (last
                             (Application (Identifier (Var cc))
                              ((Literal (Int 3))
                               (Fn
                                ((type_parameters ()) (parameters ())
                                 (result_type ())
                                 (body
                                  ((statements ())
                                   (last
                                    (Application (Identifier (Var dd))
                                     ((Literal (Int 5))
                                      (Fn
                                       ((type_parameters ())
                                        (parameters
                                         (((pattern (Pattern_id (Var x)))
                                           (type_ ()))))
                                        (result_type ())
                                        (body
                                         ((statements ())
                                          (last
                                           (Application
                                            (Identifier (Var println))
                                            ((Identifier (Var x)))))))))))))))))))))))))))))))))))))))))))
|}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun
         ((User one)
          (((Variable (User aa)) (Variable (User bb)) (Variable (User cc))
            (Variable (User dd)))
           (Let_mono (User z) (Value (Literal (Int 1)))
            (Application (Value (Variable (User aa)))
             ((Value
               (Lambda
                (()
                 (Seq
                  (Application (Value (Variable (User println)))
                   ((Value (Variable (User zz)))))
                  (Application (Value (Variable (User bb)))
                   ((Value
                     (Lambda
                      (()
                       (Application (Value (Variable (User cc)))
                        ((Value (Literal (Int 3)))
                         (Value
                          (Lambda
                           (()
                            (Application (Value (Variable (User dd)))
                             ((Value (Literal (Int 5)))
                              (Value
                               (Lambda
                                (((Variable (User x)))
                                 (Application (Value (Variable (User println)))
                                  ((Value (Variable (User x)))))))))))))))))))))))))))))))))) |}]
;;

let%expect_test "single line comments" =
  let code =
    {|
// this is a comment 12 + 13 == 25
// and this is another
fun get-speed() {// they can go after declarations too!
  100;
} // and here

fun documented() {
  // comments
  val x = 1
    + 2  // can ...
    + 3; // go on lines within expressions!
  x * x; // after them
  // and even at the end of blocks
}
// multiline comments do not start within them! /*
fun not-commented-out() { True; };
// // /// ////
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var get-speed))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body ((statements ()) (last (Literal (Int 100))))))))))
       (Pure_declaration
        (Top_fun
         ((id (Var documented))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body
             ((statements
               ((Declaration
                 (Val ((pattern (Pattern_id (Var x))) (scheme ()))
                  ((statements ())
                   (last
                    (Binary_op
                     (Binary_op (Literal (Int 1)) Plus (Literal (Int 2))) Plus
                     (Literal (Int 3)))))))))
              (last (Binary_op (Identifier (Var x)) Times (Identifier (Var x)))))))))))
       (Pure_declaration
        (Top_fun
         ((id (Var not-commented-out))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body ((statements ()) (last (Literal (Bool true))))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun ((User get-speed) (() (Value (Literal (Int 100))))))
        (Fun
         ((User documented)
          (()
           (Let_mono (User x)
            (Operator
             (Operator (Value (Literal (Int 1))) (Int Plus)
              (Value (Literal (Int 2))))
             (Int Plus) (Value (Literal (Int 3))))
            (Operator (Value (Variable (User x))) (Int Times)
             (Value (Variable (User x))))))))
        (Fun ((User not-commented-out) (() (Value (Literal (Bool true)))))))))) |}]
;;

let%expect_test "multiline comments" =
  let code =
    {|
fun main() {
  /* these multiline comments
  can of course go over multiple lines!
      */
  val x = 1;
  /* they /* can
      /* be
      /* nested */
      */
      */
  as much as is required */

  val y = x * /* can be within expressions! */ 5;
  ()
}

|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var main))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body
             ((statements
               ((Declaration
                 (Val ((pattern (Pattern_id (Var x))) (scheme ()))
                  ((statements ()) (last (Literal (Int 1))))))
                (Declaration
                 (Val ((pattern (Pattern_id (Var y))) (scheme ()))
                  ((statements ())
                   (last
                    (Binary_op (Identifier (Var x)) Times (Literal (Int 5)))))))))
              (last (Literal Unit)))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun
         ((User main)
          (()
           (Let_mono (User x) (Value (Literal (Int 1)))
            (Let_mono (User y)
             (Operator (Value (Variable (User x))) (Int Times)
              (Value (Literal (Int 5))))
             (Value (Literal Unit))))))))))) |}]
;;

let%expect_test "" =
  let code =
    {|
fun op-trailing-lambda-example() {
  5 * a fn() 3 + 4 * 5;
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var op-trailing-lambda-example))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body
             ((statements ())
              (last
               (Binary_op (Literal (Int 5)) Times
                (Application (Identifier (Var a))
                 ((Fn
                   ((type_parameters ()) (parameters ()) (result_type ())
                    (body
                     ((statements ())
                      (last
                       (Binary_op (Literal (Int 3)) Plus
                        (Binary_op (Literal (Int 4)) Times (Literal (Int 5))))))))))))))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun
         ((User op-trailing-lambda-example)
          (()
           (Operator (Value (Literal (Int 5))) (Int Times)
            (Application (Value (Variable (User a)))
             ((Value
               (Lambda
                (()
                 (Operator (Value (Literal (Int 3))) (Int Plus)
                  (Operator (Value (Literal (Int 4))) (Int Times)
                   (Value (Literal (Int 5)))))))))))))))))) |}]
;;

let%expect_test "trailing and single line lambda" =
  let code =
    {|
fun trailing-lambdas() {
  fn(a) a fn(b) b fn(c) c;
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var trailing-lambdas))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body
             ((statements ())
              (last
               (Fn
                ((type_parameters ())
                 (parameters (((pattern (Pattern_id (Var a))) (type_ ()))))
                 (result_type ())
                 (body
                  ((statements ())
                   (last
                    (Application (Identifier (Var a))
                     ((Fn
                       ((type_parameters ())
                        (parameters
                         (((pattern (Pattern_id (Var b))) (type_ ()))))
                        (result_type ())
                        (body
                         ((statements ())
                          (last
                           (Application (Identifier (Var b))
                            ((Fn
                              ((type_parameters ())
                               (parameters
                                (((pattern (Pattern_id (Var c))) (type_ ()))))
                               (result_type ())
                               (body
                                ((statements ()) (last (Identifier (Var c))))))))))))))))))))))))))))))))
    |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun
         ((User trailing-lambdas)
          (()
           (Value
            (Lambda
             (((Variable (User a)))
              (Application (Value (Variable (User a)))
               ((Value
                 (Lambda
                  (((Variable (User b)))
                   (Application (Value (Variable (User b)))
                    ((Value
                      (Lambda
                       (((Variable (User c))) (Value (Variable (User c))))))))))))))))))))))) |}]
;;

let%expect_test "application after trailing lambda" =
  let code =
    {|
fun app-after-trailing-lambda() {
  xs.foo fn(x) {x} (y, z).bar();
  // desugars to:
  // bar(foo(xs, fn(x) {x})(y, z));
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var app-after-trailing-lambda))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body
             ((statements ())
              (last
               (Application (Identifier (Var bar))
                ((Application
                  (Application (Identifier (Var foo))
                   ((Identifier (Var xs))
                    (Fn
                     ((type_parameters ())
                      (parameters (((pattern (Pattern_id (Var x))) (type_ ()))))
                      (result_type ())
                      (body ((statements ()) (last (Identifier (Var x)))))))))
                  ((Identifier (Var y)) (Identifier (Var z))))))))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun
         ((User app-after-trailing-lambda)
          (()
           (Application (Value (Variable (User bar)))
            ((Application
              (Application (Value (Variable (User foo)))
               ((Value (Variable (User xs)))
                (Value
                 (Lambda (((Variable (User x))) (Value (Variable (User x))))))))
              ((Value (Variable (User y))) (Value (Variable (User z)))))))))))))) |}]
;;

let%expect_test "effect declaration" =
  let code =
    {|
effect my-effect {
  control choose-upto(n : int) : int;
  fun depth(dummy : ()) : int;
  fun get(dummy : ()) : int;
  fun set(x : int) : bool;
  control raise(x : int) : ();
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Type_declaration
        (Effect_declaration
         ((id my-effect) (type_parameters ()) (kind ())
          (operations
           (((id choose-upto) (type_parameters ())
             (shape
              (Shape_control
               (((id (Parameter_id (Var n)))
                 (type_ (Type_atom (constructor Type_int) (arguments ())))))
               (Type_atom (constructor Type_int) (arguments ())))))
            ((id depth) (type_parameters ())
             (shape
              (Shape_fun
               (((id (Parameter_id (Var dummy)))
                 (type_ (Parameters_or_tuple ()))))
               (Type_atom (constructor Type_int) (arguments ())))))
            ((id get) (type_parameters ())
             (shape
              (Shape_fun
               (((id (Parameter_id (Var dummy)))
                 (type_ (Parameters_or_tuple ()))))
               (Type_atom (constructor Type_int) (arguments ())))))
            ((id set) (type_parameters ())
             (shape
              (Shape_fun
               (((id (Parameter_id (Var x)))
                 (type_ (Type_atom (constructor Type_int) (arguments ())))))
               (Type_atom (constructor Type_bool) (arguments ())))))
            ((id raise) (type_parameters ())
             (shape
              (Shape_control
               (((id (Parameter_id (Var x)))
                 (type_ (Type_atom (constructor Type_int) (arguments ())))))
               (Parameters_or_tuple ())))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Effect
         ((name my-effect)
          (operations
           (((User choose-upto)
             ((shape Control) (argument (Primitive Int))
              (answer (Primitive Int))))
            ((User depth)
             ((shape Fun) (argument (Primitive Unit)) (answer (Primitive Int))))
            ((User get)
             ((shape Fun) (argument (Primitive Unit)) (answer (Primitive Int))))
            ((User raise)
             ((shape Control) (argument (Primitive Int))
              (answer (Primitive Unit))))
            ((User set)
             ((shape Fun) (argument (Primitive Int)) (answer (Primitive Bool)))))))))))) |}]
;;

let%expect_test "multi shaped effect declaration" =
  let code =
    {|
effect my-effect {
  control choose-upto(n : int) : int;
  val depth : int;
  fun get(dummy : ()) : int;
  fun set(x : int) : bool;
  except raise(x : int) : ();
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Type_declaration
        (Effect_declaration
         ((id my-effect) (type_parameters ()) (kind ())
          (operations
           (((id choose-upto) (type_parameters ())
             (shape
              (Shape_control
               (((id (Parameter_id (Var n)))
                 (type_ (Type_atom (constructor Type_int) (arguments ())))))
               (Type_atom (constructor Type_int) (arguments ())))))
            ((id depth) (type_parameters ())
             (shape
              (Shape_val (Type_atom (constructor Type_int) (arguments ())))))
            ((id get) (type_parameters ())
             (shape
              (Shape_fun
               (((id (Parameter_id (Var dummy)))
                 (type_ (Parameters_or_tuple ()))))
               (Type_atom (constructor Type_int) (arguments ())))))
            ((id set) (type_parameters ())
             (shape
              (Shape_fun
               (((id (Parameter_id (Var x)))
                 (type_ (Type_atom (constructor Type_int) (arguments ())))))
               (Type_atom (constructor Type_bool) (arguments ())))))
            ((id raise) (type_parameters ())
             (shape
              (Shape_except
               (((id (Parameter_id (Var x)))
                 (type_ (Type_atom (constructor Type_int) (arguments ())))))
               (Parameters_or_tuple ())))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Error
     ((kind Unsupported_feature) (message "non `control` effect") (location ()))) |}]
;;

let%expect_test "parameterised effect declaration" =
  let code =
    {|
effect my-effect<a :: V> {
  control choose(x : a, y : a) : a;
  val depth : int;
  fun get() : a;
  fun set(x : a) : bool;
  except raise(x : a) : b;
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Type_declaration
        (Effect_declaration
         ((id my-effect)
          (type_parameters (((id a) (kind ((Kind_atom Kind_value)))))) (kind ())
          (operations
           (((id choose) (type_parameters ())
             (shape
              (Shape_control
               (((id (Parameter_id (Var x)))
                 (type_
                  (Type_atom (constructor (Variable_or_name a)) (arguments ()))))
                ((id (Parameter_id (Var y)))
                 (type_
                  (Type_atom (constructor (Variable_or_name a)) (arguments ())))))
               (Type_atom (constructor (Variable_or_name a)) (arguments ())))))
            ((id depth) (type_parameters ())
             (shape
              (Shape_val (Type_atom (constructor Type_int) (arguments ())))))
            ((id get) (type_parameters ())
             (shape
              (Shape_fun ()
               (Type_atom (constructor (Variable_or_name a)) (arguments ())))))
            ((id set) (type_parameters ())
             (shape
              (Shape_fun
               (((id (Parameter_id (Var x)))
                 (type_
                  (Type_atom (constructor (Variable_or_name a)) (arguments ())))))
               (Type_atom (constructor Type_bool) (arguments ())))))
            ((id raise) (type_parameters ())
             (shape
              (Shape_except
               (((id (Parameter_id (Var x)))
                 (type_
                  (Type_atom (constructor (Variable_or_name a)) (arguments ())))))
               (Type_atom (constructor (Variable_or_name b)) (arguments ()))))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Error
     ((kind Unsupported_feature) (message "type parameters for effect")
      (location ()))) |}]
;;

let%expect_test "shorthand effect declaration" =
  let code =
    {|
effect control yield(x : int) : bool;
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Type_declaration
        (Effect_declaration
         ((id yield) (type_parameters ()) (kind ())
          (operations
           (((id yield) (type_parameters ())
             (shape
              (Shape_control
               (((id (Parameter_id (Var x)))
                 (type_ (Type_atom (constructor Type_int) (arguments ())))))
               (Type_atom (constructor Type_bool) (arguments ()))))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Effect
         ((name yield)
          (operations
           (((User yield)
             ((shape Control) (argument (Primitive Int))
              (answer (Primitive Bool)))))))))))) |}]
;;

let%expect_test "handler" =
  let code =
    {|
fun fail-to-default(default, action) {
  with handler {
    except fail() default;
    return(x) x;
  };
  action();
}
fun many-operations() {
  val h = handler {
    control choose(x, y) {
      resume(x) || resume(y);
    };
    return(x) is-goal(x);
    val depth = 42;
    fun get() {
      y;
    };
    fun set(x) 0;
    except raise(x) { println(x) };
  };
  h;
}
fun one-operation() {
  with val depth = depth + 1;
  subject();
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var fail-to-default))
          (fn
           ((type_parameters ())
            (parameters
             (((pattern (Pattern_id (Var default))) (type_ ()))
              ((pattern (Pattern_id (Var action))) (type_ ()))))
            (result_type ())
            (body
             ((statements ())
              (last
               (Application
                (Handler
                 (Effect_handler
                  ((Op_except (id fail) (parameters ())
                    (body ((statements ()) (last (Identifier (Var default))))))
                   (Op_return
                    (parameter ((id (Parameter_id (Var x))) (type_ ())))
                    (body ((statements ()) (last (Identifier (Var x)))))))))
                ((Fn
                  ((type_parameters ()) (parameters ()) (result_type ())
                   (body
                    ((statements ())
                     (last (Application (Identifier (Var action)) ()))))))))))))))))
       (Pure_declaration
        (Top_fun
         ((id (Var many-operations))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body
             ((statements
               ((Declaration
                 (Val ((pattern (Pattern_id (Var h))) (scheme ()))
                  ((statements ())
                   (last
                    (Handler
                     (Effect_handler
                      ((Op_control (id choose)
                        (parameters
                         (((id (Parameter_id (Var x))) (type_ ()))
                          ((id (Parameter_id (Var y))) (type_ ()))))
                        (body
                         ((statements ())
                          (last
                           (Binary_op
                            (Application (Identifier (Var resume))
                             ((Identifier (Var x))))
                            Or
                            (Application (Identifier (Var resume))
                             ((Identifier (Var y)))))))))
                       (Op_return
                        (parameter ((id (Parameter_id (Var x))) (type_ ())))
                        (body
                         ((statements ())
                          (last
                           (Application (Identifier (Var is-goal))
                            ((Identifier (Var x))))))))
                       (Op_val (id depth) (type_ ())
                        (value ((statements ()) (last (Literal (Int 42))))))
                       (Op_fun (id get) (parameters ())
                        (body ((statements ()) (last (Identifier (Var y))))))
                       (Op_fun (id set)
                        (parameters (((id (Parameter_id (Var x))) (type_ ()))))
                        (body ((statements ()) (last (Literal (Int 0))))))
                       (Op_except (id raise)
                        (parameters (((id (Parameter_id (Var x))) (type_ ()))))
                        (body
                         ((statements ())
                          (last
                           (Application (Identifier (Var println))
                            ((Identifier (Var x)))))))))))))))))
              (last (Identifier (Var h))))))))))
       (Pure_declaration
        (Top_fun
         ((id (Var one-operation))
          (fn
           ((type_parameters ()) (parameters ()) (result_type ())
            (body
             ((statements ())
              (last
               (Application
                (Handler
                 (Effect_handler
                  ((Op_val (id depth) (type_ ())
                    (value
                     ((statements ())
                      (last
                       (Binary_op (Identifier (Var depth)) Plus
                        (Literal (Int 1))))))))))
                ((Fn
                  ((type_parameters ()) (parameters ()) (result_type ())
                   (body
                    ((statements ())
                     (last (Application (Identifier (Var subject)) ()))))))))))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Error
     ((kind Unsupported_feature) (message "`execption` effect") (location ()))) |}]
;;

let%expect_test "handle" =
  let code =
    {|
fun handle-example(action) {
  handle (action) {
    fun scramble(x) x * x + x;
  }
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var handle-example))
          (fn
           ((type_parameters ())
            (parameters (((pattern (Pattern_id (Var action))) (type_ ()))))
            (result_type ())
            (body
             ((statements ())
              (last
               (Handle (subject (Identifier (Var action)))
                (handler
                 (Effect_handler
                  ((Op_fun (id scramble)
                    (parameters (((id (Parameter_id (Var x))) (type_ ()))))
                    (body
                     ((statements ())
                      (last
                       (Binary_op
                        (Binary_op (Identifier (Var x)) Times
                         (Identifier (Var x)))
                        Plus (Identifier (Var x)))))))))))))))))))))) |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Ok
     ((declarations
       ((Fun
         ((User handle-example)
          (((Variable (User action)))
           (Application
            (Value
             (Handler
              ((operations
                (((User scramble)
                  (Fun
                   ((op_argument (Variable (User x)))
                    (op_body
                     (Operator
                      (Operator (Value (Variable (User x))) (Int Times)
                       (Value (Variable (User x))))
                      (Int Plus) (Value (Variable (User x))))))))))
               (return_clause ()))))
            ((Value (Variable (User action)))))))))))) |}]
;;

let%expect_test "type annotations" =
  let code =
    {|
fun sqrt(x : int) : exn int {
  raise();
}
fun square(x : int) : <> int {
  x * x;
}
  |}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var sqrt))
          (fn
           ((type_parameters ())
            (parameters
             (((pattern (Pattern_id (Var x)))
               (type_ ((Type_atom (constructor Type_int) (arguments ())))))))
            (result_type
             (((effect_
                (Type_atom (constructor (Variable_or_name exn)) (arguments ())))
               (result (Type_atom (constructor Type_int) (arguments ()))))))
            (body
             ((statements ()) (last (Application (Identifier (Var raise)) ())))))))))
       (Pure_declaration
        (Top_fun
         ((id (Var square))
          (fn
           ((type_parameters ())
            (parameters
             (((pattern (Pattern_id (Var x)))
               (type_ ((Type_atom (constructor Type_int) (arguments ())))))))
            (result_type
             (((effect_ (Effect_row (Closed ())))
               (result (Type_atom (constructor Type_int) (arguments ()))))))
            (body
             ((statements ())
              (last (Binary_op (Identifier (Var x)) Times (Identifier (Var x))))))))))))))
    |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Error
     ((kind Unsupported_feature) (message "return type annotation")
      (location ()))) |}]
;;

let%expect_test "effect annotations" =
  let code =
    {|
fun compose(f : a -> e b, g : b -> e c) : (a -> e c) {
  fn(a) g(f(a));
}
fun fail-with-default(x : a, action : () -> <fail|e> a) : e a {
  with handler {
    except fail() x;
  };
  action();
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Top_fun
         ((id (Var compose))
          (fn
           ((type_parameters ())
            (parameters
             (((pattern (Pattern_id (Var f)))
               (type_
                ((Arrow
                  (Type_atom (constructor (Variable_or_name a)) (arguments ()))
                  ((effect_
                    (Type_atom (constructor (Variable_or_name e)) (arguments ())))
                   (result
                    (Type_atom (constructor (Variable_or_name b)) (arguments ()))))))))
              ((pattern (Pattern_id (Var g)))
               (type_
                ((Arrow
                  (Type_atom (constructor (Variable_or_name b)) (arguments ()))
                  ((effect_
                    (Type_atom (constructor (Variable_or_name e)) (arguments ())))
                   (result
                    (Type_atom (constructor (Variable_or_name c)) (arguments ()))))))))))
            (result_type
             (((effect_ (Effect_row (Closed ())))
               (result
                (Parameters_or_tuple
                 (((parameter_id ())
                   (type_
                    (Arrow
                     (Type_atom (constructor (Variable_or_name a))
                      (arguments ()))
                     ((effect_
                       (Type_atom (constructor (Variable_or_name e))
                        (arguments ())))
                      (result
                       (Type_atom (constructor (Variable_or_name c))
                        (arguments ())))))))))))))
            (body
             ((statements ())
              (last
               (Fn
                ((type_parameters ())
                 (parameters (((pattern (Pattern_id (Var a))) (type_ ()))))
                 (result_type ())
                 (body
                  ((statements ())
                   (last
                    (Application (Identifier (Var g))
                     ((Application (Identifier (Var f)) ((Identifier (Var a)))))))))))))))))))
       (Pure_declaration
        (Top_fun
         ((id (Var fail-with-default))
          (fn
           ((type_parameters ())
            (parameters
             (((pattern (Pattern_id (Var x)))
               (type_
                ((Type_atom (constructor (Variable_or_name a)) (arguments ())))))
              ((pattern (Pattern_id (Var action)))
               (type_
                ((Arrow (Parameters_or_tuple ())
                  ((effect_
                    (Effect_row
                     (Open
                      (Cons
                       (Type_atom (constructor (Variable_or_name fail))
                        (arguments ()))
                       ())
                      (Type_atom (constructor (Variable_or_name e))
                       (arguments ())))))
                   (result
                    (Type_atom (constructor (Variable_or_name a)) (arguments ()))))))))))
            (result_type
             (((effect_
                (Type_atom (constructor (Variable_or_name e)) (arguments ())))
               (result
                (Type_atom (constructor (Variable_or_name a)) (arguments ()))))))
            (body
             ((statements ())
              (last
               (Application
                (Handler
                 (Effect_handler
                  ((Op_except (id fail) (parameters ())
                    (body ((statements ()) (last (Identifier (Var x)))))))))
                ((Fn
                  ((type_parameters ()) (parameters ()) (result_type ())
                   (body
                    ((statements ())
                     (last (Application (Identifier (Var action)) ())))))))))))))))))))
    |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Error
     ((kind Unsupported_feature) (message "return type annotation")
      (location ()))) |}]
;;

let%expect_test "apparently unnecessary brackets in arrow types" =
  let code =
    {|
// val f : ((x : int, y : int)) -> (() -> (int)) = f_;
// testing like this since annotations are unsupported everywhere else
effect control f(x : int) : ( ((x : int, y : int)) -> (() -> (int)) );
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Type_declaration
        (Effect_declaration
         ((id f) (type_parameters ()) (kind ())
          (operations
           (((id f) (type_parameters ())
             (shape
              (Shape_control
               (((id (Parameter_id (Var x)))
                 (type_ (Type_atom (constructor Type_int) (arguments ())))))
               (Parameters_or_tuple
                (((parameter_id ())
                  (type_
                   (Arrow
                    (Parameters_or_tuple
                     (((parameter_id ())
                       (type_
                        (Parameters_or_tuple
                         (((parameter_id ((Var x)))
                           (type_
                            (Type_atom (constructor Type_int) (arguments ()))))
                          ((parameter_id ((Var y)))
                           (type_
                            (Type_atom (constructor Type_int) (arguments ()))))))))))
                    ((effect_ (Effect_row (Closed ())))
                     (result
                      (Parameters_or_tuple
                       (((parameter_id ())
                         (type_
                          (Arrow (Parameters_or_tuple ())
                           ((effect_ (Effect_row (Closed ())))
                            (result
                             (Parameters_or_tuple
                              (((parameter_id ())
                                (type_
                                 (Type_atom (constructor Type_int)
                                  (arguments ())))))))))))))))))))))))))))))))
    |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Error
     ((kind Syntax_error) (message "tuple type cannot have parameter labels")
      (location ()))) |}]
;;

let%expect_test "kind annotations" =
  let code =
    {|
effect eff<a :: X, b :: X, c :: E, d :: V> {
  control foo(_ : d) : (() -> <a, b | c>);
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Ok
     (Program
      ((Type_declaration
        (Effect_declaration
         ((id eff)
          (type_parameters
           (((id a) (kind ((Kind_atom Kind_effect_type))))
            ((id b) (kind ((Kind_atom Kind_effect_type))))
            ((id c) (kind ((Kind_atom Kind_effect_row))))
            ((id d) (kind ((Kind_atom Kind_value))))))
          (kind ())
          (operations
           (((id foo) (type_parameters ())
             (shape
              (Shape_control
               (((id Parameter_wildcard)
                 (type_
                  (Type_atom (constructor (Variable_or_name d)) (arguments ())))))
               (Parameters_or_tuple
                (((parameter_id ())
                  (type_
                   (Arrow (Parameters_or_tuple ())
                    ((effect_ (Effect_row (Closed ())))
                     (result
                      (Effect_row
                       (Open
                        (Cons
                         (Type_atom (constructor (Variable_or_name a))
                          (arguments ()))
                         ((Type_atom (constructor (Variable_or_name b))
                           (arguments ()))))
                        (Type_atom (constructor (Variable_or_name c))
                         (arguments ()))))))))))))))))))))))
    |}];
  Util.print_simplification_result syntax;
  [%expect
    {|
    (Error
     ((kind Unsupported_feature) (message "type parameters for effect")
      (location ()))) |}]
;;
