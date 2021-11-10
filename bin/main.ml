open Core

let code = {|
fun foo() {
  val x = 1;
  // comment
  x * y;
};
|}

let () =
  match Koka_zero.parse_string code with
  | Ok ast -> print_s [%sexp (ast : Koka_zero.Syntax.program)]
  | Error message -> eprintf "%s\n" message
;;
