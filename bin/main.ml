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
  let open Result.Let_syntax in
  let result =
    let%bind program = Koka_zero.parse_string code in
    print_s [%sexp (program : Koka_zero.Minimal_syntax.Program.t)];
    let%map program_explicit = Koka_zero.infer_program program in
    print_s [%sexp (program_explicit : Koka_zero.Explicit_syntax.Program.t)];
    let program_monadic = Koka_zero.translate program_explicit in
    print_s
      [%sexp (program_monadic : Koka_zero.Evidence_passing_syntax.Program.t)]
  in
  match result with
  | Ok () -> ()
  | Error error -> Koka_zero.Static_error.string_of_t error |> eprintf "%s\n"
;;
