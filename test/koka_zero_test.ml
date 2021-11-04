open Core
open Koka_zero

let%expect_test "toplevel value declaration" =
  let code = {|
  val number : int = 1729;
|} in
  let ast = parse_string code in
  [%sexp (ast : (Syntax.program, string) Result.t)]
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {|
    (Ok
     (Program
      ((Pure_declaration
        (Val
         ((id (Var number))
          (type_ ((Type_atom (constructor Type_int) (arguments ())))))
         ((statements ((Expr (Literal (Int 1729))))))))))) |}]
;;

let correct_cases =
  [ "single expression function", {|
fun main() {
  0;
}
|}
  ; "toplevel value declaration", {|
val number : int = 1729;
|}
  ; ( "dashes in identifiers"
    , {|
val kebab-case = 0;
val x-y-z = 1;
val number3-letter = 2;
      |} )
  ; "hex literals", {|
val abcd = 0x1234;
|}
  ; "prime at end of identifier", {|
val f' = diff(f);
val f'' = diff(f')
  |}
  ; "boolean literals", {|
val t = True;
val f = False;
  |}
  ; "negative integer literals", {|
val minus-fourty = -40;
  |}
  ; ( "operators"
    , {|
fun hypotenuse(a : int, b : int) {
  val c-squared = a * a + b * b;
  val c = isqrt(c-squared);
  c;
}
val all = 12 + 33 * 44 - 36 / 4 + 91 % 7 + 11
va inside = 0 <= x && x < 7 || 100 < x && x <= 9000;
|}
    )
  ; ( "mixing infix & prefix operators"
    , {|
fun xnor(a, b) {
  a && b || !a && !b;
}
val sum = 3 + -1 * -7;
|} )
  ; ( "if statements"
    , {|
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
    )
  ; ( "nested if statements"
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
