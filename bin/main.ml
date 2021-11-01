open Core

let code = {|
fun foo() {
  val x = 1;
  // comment
  x * y;
}
|}

let () =
  match Koka_zero.parse_string code with
  | Ok _ast -> print_endline "success!"
  (* TODO: just print the message string *)
  | Error message -> printf "%s\n" message
;;
