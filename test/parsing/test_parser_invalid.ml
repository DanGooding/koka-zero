open! Core

let%expect_test "function without parameters" =
  let code =
    {|
fun compute-the-answer {
  42;
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Error
     ((kind Syntax_error) (error "parse error")
      (location (((filename ()) (line 2) (char 25))))))
    |}];
  Util.print_simplification_result syntax;
  [%expect {| |}]
;;

let%expect_test "dash before number in identifier" =
  let code =
    {|
val n-3 = n - 3;
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Error
     ((kind Syntax_error)
      (error
       "malformed identifier: a dash must be preceded by a letter or number, and followed by a letter")
      (location (((filename ()) (line 2) (char 8))))))
    |}];
  Util.print_simplification_result syntax;
  [%expect {| |}]
;;

let%expect_test "dash at end of identifier" =
  let code =
    {|
val n- = n;
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Error
     ((kind Syntax_error)
      (error
       "malformed identifier: a dash must be preceded by a letter or number, and followed by a letter")
      (location (((filename ()) (line 2) (char 7))))))
    |}];
  Util.print_simplification_result syntax;
  [%expect {| |}]
;;

let%expect_test "dash at start of identifier" =
  let code =
    {|
  val -n = 0 - n;
  |}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Error
     ((kind Syntax_error) (error "parse error")
      (location (((filename ()) (line 2) (char 8))))))
    |}];
  Util.print_simplification_result syntax;
  [%expect {| |}]
;;

let%expect_test "lambda as operand" =
  (* Koka forbids this - presumably applying an operator to a function like this
     is rare *)
  let code =
    {|
fun op-trailing-lambda-example() {
  5 * fn() 3 + 4;
}
|}
  in
  let syntax = Util.print_parse_to_syntax_result code in
  [%expect
    {|
    (Error
     ((kind Syntax_error) (error "parse error")
      (location (((filename ()) (line 3) (char 9))))))
    |}];
  Util.print_simplification_result syntax;
  [%expect {| |}]
;;
