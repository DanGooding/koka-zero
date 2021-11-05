let%expect_test "function without parameters" =
  let code = {|
fun compute-the-answer {
  42;
}
|} in
  Test_parser_util.print_parse_result code;
  [%expect {|  |}]
;;

let%expect_test "dashes in wrong place in identifier" =
  let code = {|
val n-3 = n - 3;
val n- = n;
|} in
  Test_parser_util.print_parse_result code;
  [%expect {| |}]
;;

let _e =
  {e|

let%expect_test
  =
  let code =
  in Test_parser_util.print_parse_result code;
  [%expect {| |}]
;;

|e}
;;

let _wrong_cases =
  [ "function without parameters", {|
fun compute-the-answer {
  42;
}
|}
  ; ( "dashes in wrong place in identifier"
    , {|
val n-3 = n - 3;
val n- = n;
val -a = 1;
|} )
    (* TODO: more incorrect examples *)
  ]
;;
