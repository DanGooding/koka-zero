open Core
open Koka_zero

let print_parse_result code =
  let ast = parse_string code in
  [%sexp (ast : (Syntax.program, string) Result.t)]
  |> Sexp.to_string_hum
  |> print_endline
;;

let%expect_test "toplevel value declaration" =
  let code = {|
  val number : int = 1729;
|} in
  print_parse_result code;
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Val
         ((id (Var number))
          (type_ ((Type_atom (constructor Type_int) (arguments ())))))
         ((statements ((Expr (Literal (Int 1729))))))))))) |}]
;;

let%expect_test "single expression function" =
  let code = {|
fun main() {
  0;
}
|} in
  print_parse_result code;
  [%expect {| |}]
;;

let%expect_test "dashes in identifiers" =
  let code =
    {|
val kebab-case = 0;
val x-y-z = 1;
val number3-letter = 2;
      |}
  in
  print_parse_result code;
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Val ((id (Var kebab-case)) (type_ ()))
         ((statements ((Expr (Literal (Int 0))))))))
       (Pure_declaration
        (Val ((id (Var x-y-z)) (type_ ()))
         ((statements ((Expr (Literal (Int 1))))))))
       (Pure_declaration
        (Val ((id (Var number3-letter)) (type_ ()))
         ((statements ((Expr (Literal (Int 2))))))))))) |}]
;;

let _e =
  {e|

let%expect_test
  =
  let code =
  in print_parse_result code;
  [%expect {| |}]
;;

|e}
;;

let%expect_test "hex literals" =
  let code = {|
val abcd = 0x1234;
|} in
  print_parse_result code;
  [%expect {| |}]
;;

let%expect_test "prime at end of identifier" =
  let code = {|
val f' = dif
f(f);
val f'' = diff(f')
  |} in
  print_parse_result code;
  [%expect {| |}]
;;

let%expect_test "operators" =
  let code =
    {|
fun hypotenuse(a : int, b : int) {
  val c-squared = a * a + b * b;
  val c = isqrt(c-squared);
  c;
}
val all = 12 + 33 * 44 - 36 / 4 + 91 % 7 + 11
val inside = 0 <= x && x < 7 || 100 < x && x <= 9000;
|}
  in
  print_parse_result code;
  [%expect {| |}]
;;

let%expect_test "negative integer literals" =
  let code = {|
val minus-fourty = -40;
|} in
  print_parse_result code;
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Val ((id (Var minus-fourty)) (type_ ()))
         ((statements ((Expr (Literal (Int -40))))))))))) |}]
;;

let%expect_test "boolean literals" =
  let code = {|
val t = True;
val f = False;
  |} in
  print_parse_result code;
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Val ((id (Var t)) (type_ ()))
         ((statements ((Expr (Literal (Bool true))))))))
       (Pure_declaration
        (Val ((id (Var f)) (type_ ()))
         ((statements ((Expr (Literal (Bool false))))))))))) |}]
;;

let%expect_test "if statements" =
  let code =
    {|
fun if-example() {
  if x % 2 == 0 then
    123
  elif y % 2 == 0 then
    400 + 50 + 6
  elif False
    -1
  else
    1 + 1;
}
    |}
  in
  print_parse_result code;
  [%expect {| |}]
;;

let correct_cases =
  [ ( "nested if statements"
    , {|
fun i() {
  if a then
    if b then
      c
  else d;
}
|} )
  ; ( "dot application"
    , {|
fun dot-application() {
  x.best.fst.pow(3).print;
;
  |} )
  ; ( "trailing lambda application"
    , {|
fun trailing-lambda() {
  f(x,y,z) { alpha } fn(b) {beta} {gamma};
  a.g(1).h(2) { zzz };
}
  |}
    )
  ; ( "with syntax"
    , {|
fun one(aa, bb, cc, dd) {
  with aa;
  with bb();
  with cc(3);
  with x <- dd(5);
  println(x : string);
}
  |}
      (* TODO: more complex with syntax example *) )
  ; ( "single line comments"
    , {|
// this is a comment 12 + 13 == 25
// and this is another
val speed = 100; // they can go after declarations too!

fun documented() {
  // comments
  val x = 1
    + 2  // can ...
    + 3; // go on lines within expressions!
  x * x; // after them
  // and even at the end of blocks
}
// multiline comments do not start within them! /*
val not-commented-out = True;
// // /// ////
|}
    )
  ; ( "multiline comments"
    , {|
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
|}
    )
  ]
;;

let wrong_cases =
  [ "function without parameters", {|
fun compute-the-answer {
  42;
}
|}
  ; "dashes in wrong place in identifier", {|
val n-3 = n - 3;
val n- = n
|}
    (* TODO: more incorrect examples *)
  ]
;;

let (_ : (string * string) list) = correct_cases @ wrong_cases
