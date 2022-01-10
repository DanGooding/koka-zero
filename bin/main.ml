open Core

let code =
  {|
fun foo(y) {
  val x = 5;
  // comment
  x * y;
};

fun main() {
  foo(3) + 1;
};
|}
;;

let () =
  let open Result.Let_syntax in
  let result_program =
    let%bind program = Koka_zero.parse_string code in
    let%map program_explicit = Koka_zero.infer_program program in
    Koka_zero.translate program_explicit
  in
  match result_program with
  | Error error -> Koka_zero.Static_error.string_of_t error |> eprintf "%s\n"
  | Ok program ->
    let result_value = Koka_zero.interpret_program program in
    (match result_value with
    | Error error ->
      Koka_zero.Runtime_error.string_of_t error |> eprintf "runtime error: %s\n"
    | Ok value ->
      Koka_zero.Value.sexp_of_t value
      |> Sexp.to_string_hum
      |> printf "result:\n%s\n")
;;
