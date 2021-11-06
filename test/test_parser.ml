let%expect_test "toplevel value declaration" =
  let code = {|
  val number : int = 1729;
|} in
  Test_parser_util.print_parse_result code;
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Val
         ((id (Var number))
          (type_ ((Type_atom (constructor Type_int) (arguments ())))))
         ((statements ()) (last (Literal (Int 1729))))))))) |}]
;;

let%expect_test "single expression function" =
  let code = {|
fun main() {
  0;
}
|} in
  Test_parser_util.print_parse_result code;
  [%expect {| |}]
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
  Test_parser_util.print_parse_result code;
  [%expect {| |}]
;;

let%expect_test "dashes in identifiers" =
  let code = {|
val kebab-case = 0;
val x-y-z = 1;
val number3-letter = 2;
|} in
  Test_parser_util.print_parse_result code;
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Val ((id (Var kebab-case)) (type_ ()))
         ((statements ()) (last (Literal (Int 0))))))
       (Pure_declaration
        (Val ((id (Var x-y-z)) (type_ ()))
         ((statements ()) (last (Literal (Int 1))))))
       (Pure_declaration
        (Val ((id (Var number3-letter)) (type_ ()))
         ((statements ()) (last (Literal (Int 2))))))))) |}]
;;

let%expect_test "hex literals" =
  let code = {|
val abcd = 0x1234ABCD;
|} in
  Test_parser_util.print_parse_result code;
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Val ((id (Var abcd)) (type_ ()))
         ((statements ()) (last (Literal (Int 305441741))))))))) |}]
;;

let%expect_test "prime at end of identifier" =
  let code = {|
val f' = dif
f(f);
val f'' = diff(f')
  |} in
  Test_parser_util.print_parse_result code;
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
  Test_parser_util.print_parse_result code;
  [%expect {| |}]
;;

let%expect_test "negative integer literals" =
  let code = {|
val minus-fourty = -40;
|} in
  Test_parser_util.print_parse_result code;
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Val ((id (Var minus-fourty)) (type_ ()))
         ((statements ()) (last (Literal (Int -40))))))))) |}]
;;

let%expect_test "boolean literals" =
  let code = {|
val t = True;
val f = False;
  |} in
  Test_parser_util.print_parse_result code;
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Val ((id (Var t)) (type_ ()))
         ((statements ()) (last (Literal (Bool true))))))
       (Pure_declaration
        (Val ((id (Var f)) (type_ ()))
         ((statements ()) (last (Literal (Bool false))))))))) |}]
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
  Test_parser_util.print_parse_result code;
  [%expect {| |}]
;;

let%expect_test "nested if statements" =
  let code = {|
fun i() {
  if a then
    if b then
      c
  else d;
}
|} in
  Test_parser_util.print_parse_result code;
  [%expect {| |}]
;;

let%expect_test "dot application" =
  let code = {|
fun dot-application() {
  x.best.fst.pow(3).print;
;
  |} in
  Test_parser_util.print_parse_result code;
  [%expect {| |}]
;;

let%expect_test "trailing lambda application" =
  let code =
    {|
fun trailing-lambda() {
  f(x,y,z) { alpha } fn(b) {beta} {gamma};
  a.g(1).h(2) { zzz };
}
  |}
  in
  Test_parser_util.print_parse_result code;
  [%expect {| |}]
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
  println(x : string);
}
  |}
  in
  Test_parser_util.print_parse_result code;
  [%expect {| |}]
;;

let%expect_test "single line comments" =
  let code =
    {|
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
  in
  Test_parser_util.print_parse_result code;
  [%expect {| |}]
;;

let%expect_test "multiline comments" =
  let code =
    {|
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
  in
  Test_parser_util.print_parse_result code;
  [%expect
    {|
    (Ok
     (Program
      ((Pure_declaration
        (Val ((id (Var x)) (type_ ()))
         ((statements ()) (last (Literal (Int 1))))))
       (Pure_declaration
        (Val ((id (Var y)) (type_ ()))
         ((statements ())
          (last (Binary_op (Identifier (Var x)) Times (Literal (Int 5)))))))))) |}]
;;
