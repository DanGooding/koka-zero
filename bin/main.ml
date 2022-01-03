open Core

let code =
  {|
fun foo(y) {
  val x = 1;
  // comment
  x * y;
};

fun main() {
  foo(3);
};
|}
;;

let () =
  let result =
    let%bind.Result ast = Koka_zero.parse_string code in
    print_s [%sexp (ast : Koka_zero.Minimal_syntax.Program.t)];
    Koka_zero_inference.check_program ast
  in
  match result with
  | Ok () -> ()
  | Error error -> Koka_zero.Static_error.string_of_t error |> eprintf "%s\n"
;;
