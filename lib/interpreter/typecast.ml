open Core

let ctl_of_value =
  let open Interpreter.Let_syntax in
  function
  | Value.Ctl c -> return c
  | v -> Interpreter.type_error ~expected:"ctl" Value.sexp_of_t v
  (* disable "fragile match" warning *) [@@warning "-4"]
;;

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

let effect_label_of_value =
  let open Interpreter.Let_syntax in
  function
  | Value.Effect_label l -> return l
  | v -> Interpreter.type_error ~expected:"effect label" Value.sexp_of_t v
  (* disable "fragile match" warning *) [@@warning "-4"]
;;

let op_of_value =
  let open Interpreter.Let_syntax in
  function
  | Value.Op op -> return op
  | v -> Interpreter.type_error ~expected:"op" Value.sexp_of_t v
  (* disable "fragile match" warning *) [@@warning "-4"]
;;

let hnd_of_value =
  let open Interpreter.Let_syntax in
  function
  | Value.Hnd h -> return h
  | v -> Interpreter.type_error ~expected:"hnd" Value.sexp_of_t v
  (* disable "fragile match" warning *) [@@warning "-4"]
;;

let evidence_vector_of_value =
  let open Interpreter.Let_syntax in
  function
  | Value.Evidence_vector vector -> return vector
  | v -> Interpreter.type_error ~expected:"evidence vector" Value.sexp_of_t v
  (* disable "fragile match" warning *) [@@warning "-4"]
;;

let evidence_of_value =
  let open Interpreter.Let_syntax in
  function
  | Value.Evidence ev -> return ev
  | v -> Interpreter.type_error ~expected:"evidence" Value.sexp_of_t v
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

let zip_arguments ~params ~args =
  let open Interpreter.Let_syntax in
  match List.zip params args with
  | List.Or_unequal_lengths.Ok zipped -> return zipped
  | List.Or_unequal_lengths.Unequal_lengths ->
    let message =
      sprintf
        "wrong number of arguments: got %d, expecting %d"
        (List.length args)
        (List.length params)
    in
    Interpreter.impossible_error message
;;
