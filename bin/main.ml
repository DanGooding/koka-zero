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
  | Ok ast -> print_s [%sexp (ast : Koka_zero.Minimal_syntax.Program.t)]
  | Error error -> Koka_zero.Static_error.string_of_t error |> eprintf "%s\n"
;;
