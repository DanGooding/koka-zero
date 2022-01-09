let closure_of_value =
  let open Interpreter.Let_syntax in
  function
  | Value.Closure closure -> return closure
  | v -> Interpreter.type_error ~expected:"closure" Value.sexp_of_t v
  (* disable "fragile match" warning *) [@@warning "-4"]
;;

let marker_of_value =
  let open Interpreter.Let_syntax in
  function
  | Value.Marker m -> return m
  | v -> Interpreter.type_error ~expected:"marker" Value.sexp_of_t v
  (* disable "fragile match" warning *) [@@warning "-4"]
;;

let bool_of_value =
  let open Interpreter.Let_syntax in
  function
  | Value.Primitive (Value.Bool b) -> return b
  | v -> Interpreter.type_error ~expected:"bool" Value.sexp_of_t v
  (* disable "fragile match" warning *) [@@warning "-4"]
;;

let int_of_value =
  let open Interpreter.Let_syntax in
  function
  | Value.Primitive (Value.Int i) -> return i
  | v -> Interpreter.type_error ~expected:"int" Value.sexp_of_t v
  (* disable "fragile match" warning *) [@@warning "-4"]
;;
