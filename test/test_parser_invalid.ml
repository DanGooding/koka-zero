let%expect_test "function without parameters" =
  let code = {|
fun compute-the-answer {
  42;
};
|} in
  Test_parser_util.print_parse_result code;
  [%expect {| (Error "2:25 : syntax error") |}]
;;

let%expect_test "dash before number in identifier" =
  let code = {|
val n-3 = n - 3;
|} in
  Test_parser_util.print_parse_result code;
  [%expect
    {|
    (Error
     "2:8 : malformed identifier: a dash must be preceded by a letter or number, and followed by a letter") |}]
;;

let%expect_test "dash at end of identifier" =
  let code = {|
val n- = n;
|} in
  Test_parser_util.print_parse_result code;
  [%expect
    {|
    (Error
     "2:7 : malformed identifier: a dash must be preceded by a letter or number, and followed by a letter") |}]
;;

let%expect_test "dash at start of identifier" =
  let code = {|
  val -n = 0 - n;
  |} in
  Test_parser_util.print_parse_result code;
  [%expect {| (Error "2:8 : syntax error") |}]
;;

let%expect_test "operators and trailing lambdas" =
  (* note: this is likely to be parsed incorrectly with semicolon+brace
     insertion, so I'm leaving it here in case that gets added *)
  let code = {|
fun op-trailing-lambda-example() {
  5 * fn() 3 + 4;
};
|} in
  Test_parser_util.print_parse_result code;
  [%expect {| (Error "3:9 : syntax error") |}]
;;
