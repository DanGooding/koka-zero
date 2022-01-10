open Core

let code =
  {|
effect read {
  control ask(x : ()) : int;
};

fun foo() {
  ask(()) + ask(());
};

fun main() {
  with handler {
    control ask(unit) { resume(1); };
  };
  foo();
};
|}
;;

let limit_length ~limit s = String.slice s 0 (min limit (String.length s))

let () =
  let open Result.Let_syntax in
  let result_program =
    let%bind program = Koka_zero.parse_string code in
    let%map program_explicit = Koka_zero.infer_program program in
    let program_monadic = Koka_zero.translate program_explicit in
    (* Koka_zero_evidence_translation.Private.translate_no_prelude *)
    print_s
      [%sexp (program_monadic : Koka_zero.Evidence_passing_syntax.Program.t)];
    program_monadic
  in
  match result_program with
  | Error error -> Koka_zero.Static_error.string_of_t error |> eprintf "%s\n"
  | Ok program ->
    let result_value = Koka_zero.interpret_program program in
    (match result_value with
    | Error error ->
      Koka_zero.Runtime_error.string_of_t error
      |> limit_length ~limit:1000
      |> eprintf "runtime error: %s\n"
    | Ok value ->
      Koka_zero.Value.sexp_of_t value
      |> Sexp.to_string_hum
      |> limit_length ~limit:1000
      |> printf "result:\n%s\n")
;;
