open Core
open Koka_zero_inference
open Koka_zero_util
module M = Minimal_syntax
module E = M.Expr

let print_explicit_program_result result =
  [%sexp (result : (Explicit_syntax.Program.t, Static_error.t) Result.t)]
  |> Sexp.to_string_hum
  |> print_endline
;;

let print_check_program_result program =
  let result = infer_program program in
  print_explicit_program_result result
;;

let print_check_program_without_main_result program =
  let result = Private.infer_program_without_main program in
  print_explicit_program_result result
;;

let print_expr_inference_result ?(declarations = []) expr =
  let result = Private.infer_expr_toplevel expr ~declarations in
  [%sexp
    (result
     : ( Type.Mono.t * Effect.t * Explicit_syntax.Expr.t
         , Static_error.t )
         Result.t)]
  |> Sexp.to_string_hum
  |> print_endline
;;

module Parameter = struct
  let var x = M.Parameter.Variable (Variable.of_user x)
  let wildcard = M.Parameter.Wildcard
end

module Expr = struct
  let var x = E.Value (E.Variable (Variable.of_user x))
  let lit_unit = E.Value (E.Literal M.Literal.Unit)
  let lit_bool b = E.Value (E.Literal (M.Literal.Bool b))
  let lit_int i = E.Value (E.Literal (M.Literal.Int i))

  let decl_main =
    M.Decl.Fun (M.Keyword.main, ([], E.Value (E.Literal M.Literal.Unit)))
  ;;

  (** construct the desugared equiavlent of the `handle h (subject ())`
      construct *)
  let make_handle_expr (h : E.handler) (subject : E.t) : E.t =
    E.Application (E.Value (E.Handler h), [ E.Value (E.Lambda ([], subject)) ])
  ;;

  let decl_read =
    let name = Effect.Label.of_string "read" in
    let op_ask =
      let shape = Operation_shape.Fun in
      let argument = Type.Mono.Primitive Type.Primitive.Unit in
      let answer = Type.Mono.Primitive Type.Primitive.Int in
      { M.Decl.Effect.Operation.shape; argument; answer }
    in
    let operations = Variable.Map.singleton (Variable.of_user "ask") op_ask in
    { M.Decl.Effect.name; operations }
  ;;

  let decl_exn : M.Decl.Effect.t =
    let name = Effect.Label.of_string "exn" in
    let op_ask =
      let shape = Operation_shape.Control in
      let argument = Type.Mono.Primitive Type.Primitive.Unit in
      (* TODO: this should be forall a. a, once polymorphic effects are added *)
      let answer = Type.Mono.Primitive Type.Primitive.Unit in
      { M.Decl.Effect.Operation.shape; argument; answer }
    in
    let operations = Variable.Map.singleton (Variable.of_user "throw") op_ask in
    { M.Decl.Effect.name; operations }
  ;;

  let decl_query : M.Decl.Effect.t =
    let name = Effect.Label.of_string "query" in
    let op_ask =
      let shape = Operation_shape.Fun in
      let argument = Type.Mono.Primitive Type.Primitive.Int in
      (* TODO: this should be forall a. a, once polymorphic effects are added *)
      let answer = Type.Mono.Primitive Type.Primitive.Bool in
      { M.Decl.Effect.Operation.shape; argument; answer }
    in
    let operations = Variable.Map.singleton (Variable.of_user "test") op_ask in
    { M.Decl.Effect.name; operations }
  ;;

  let decl_state =
    let name = Effect.Label.of_string "state" in
    let op_get =
      let shape = Operation_shape.Fun in
      let argument = Type.Mono.Primitive Type.Primitive.Unit in
      let answer = Type.Mono.Primitive Type.Primitive.Int in
      { M.Decl.Effect.Operation.shape; argument; answer }
    in
    let op_set =
      let shape = Operation_shape.Fun in
      let argument = Type.Mono.Primitive Type.Primitive.Int in
      let answer = Type.Mono.Primitive Type.Primitive.Unit in
      { M.Decl.Effect.Operation.shape; argument; answer }
    in
    let operations =
      Variable.Map.of_alist_exn
        [ Variable.of_user "get", op_get; Variable.of_user "set", op_set ]
    in
    { M.Decl.Effect.name; operations }
  ;;

  let decl_choose : M.Decl.Effect.t =
    let name = Effect.Label.of_string "choose" in
    let op_ask =
      let shape = Operation_shape.Control in
      let argument = Type.Mono.Primitive Type.Primitive.Unit in
      let answer = Type.Mono.Primitive Type.Primitive.Bool in
      { M.Decl.Effect.Operation.shape; argument; answer }
    in
    let operations =
      Variable.Map.singleton (Variable.of_user "choose") op_ask
    in
    { M.Decl.Effect.name; operations }
  ;;

  let singleton_handler
        ~(op_name : Variable.t)
        ~(op_argument : M.Parameter.t)
        ~(op_body : E.t)
        ~(shape : Operation_shape.t)
    : E.handler
    =
    let clause = { E.op_argument; op_body } in
    let operations = Variable.Map.singleton op_name (shape, clause) in
    { E.operations; return_clause = None }
  ;;

  let read_handler (value : int) : E.handler =
    (* handler { fun ask(_) { resume(value) } } *)
    let op_name = Variable.of_user "ask" in
    let op_argument = M.Parameter.Wildcard in
    let op_body = lit_int value in
    singleton_handler ~op_name ~op_argument ~op_body ~shape:Operation_shape.Fun
  ;;

  (* handler { control throw(_) { default } } *)
  let exn_handler (default : E.t) : E.handler =
    let throw_clause =
      let op_argument = M.Parameter.Wildcard in
      let op_body = default in
      { E.op_argument; op_body }
    in
    let operations =
      Variable.Map.singleton
        (Variable.of_user "throw")
        (Operation_shape.Control, throw_clause)
    in
    { E.operations; return_clause = None }
  ;;
end
