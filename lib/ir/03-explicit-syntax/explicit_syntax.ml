open! Core
open! Import

module Expr = struct
  type 'e t =
    | Value of 'e value
    | Let of Variable.t * 'e value * 'e t
    | Let_mono of Variable.t * 'e t * 'e t
    | Application of 'e t * 'e t list * 'e
    | Construction of Constructor.t * 'e t list
    | Seq of 'e t * 'e t
    | If_then_else of 'e t * 'e t * 'e t
    | Operator of 'e t * Operator.t * 'e t
    | Unary_operator of Operator.Unary.t * 'e t
    | Impure_built_in of 'e impure_built_in
  [@@deriving sexp_of]

  and 'e value =
    | Variable of Variable.t
    | Lambda of 'e lambda
    | Fix_lambda of 'e fix_lambda
    | Literal of Literal.t
    | Perform of perform
    | Handler of 'e handler
  [@@deriving sexp_of]

  and 'e lambda = Parameter.t list * 'e t [@@deriving sexp_of]
  and 'e fix_lambda = Variable.t * 'e lambda [@@deriving sexp_of]

  and perform =
    { operation : Variable.t
    ; performed_effect : Effect.Label.t
    }
  [@@deriving sexp_of]

  and 'e handler =
    { handled_effect : Effect.Label.t
    ; operations : (Operation_shape.t * 'e op_handler) Variable.Map.t
    ; return_clause : 'e op_handler option
    }
  [@@deriving sexp_of]

  and 'e op_handler =
    { op_argument : Parameter.t
    ; op_body : 'e t
    }
  [@@deriving sexp_of]

  and 'e impure_built_in =
    | Impure_println
    | Impure_print_int of
        { value : 'e t
        ; newline : bool
        }
    | Impure_read_int
  [@@deriving sexp_of]

  let rec map_effect t ~f : _ t =
    match t with
    | Value v -> Value (value_map_effect v ~f)
    | Let (name, subject, body) ->
      Let (name, value_map_effect subject ~f, map_effect body ~f)
    | Let_mono (name, subject, body) ->
      Let_mono (name, map_effect subject ~f, map_effect body ~f)
    | Application (fun_, args, effect_) ->
      Application
        (map_effect fun_ ~f, List.map args ~f:(map_effect ~f), f effect_)
    | Construction (constructor, args) ->
      Construction (constructor, List.map args ~f:(map_effect ~f))
    | Seq (first, second) -> Seq (map_effect first ~f, map_effect second ~f)
    | If_then_else (cond, yes, no) ->
      If_then_else (map_effect cond ~f, map_effect yes ~f, map_effect no ~f)
    | Operator (left, op, right) ->
      Operator (map_effect left ~f, op, map_effect right ~f)
    | Unary_operator (uop, arg) -> Unary_operator (uop, map_effect arg ~f)
    | Impure_built_in built_in ->
      Impure_built_in (impure_built_in_map_effect built_in ~f)

  and value_map_effect v ~f : _ value =
    match v with
    | Variable v -> Variable v
    | Literal lit -> Literal lit
    | Perform p -> Perform p
    | Lambda lambda -> Lambda (lambda_map_effect lambda ~f)
    | Fix_lambda fix_lambda -> Fix_lambda (fix_lambda_map_effect fix_lambda ~f)
    | Handler handler -> Handler (handler_map_effect handler ~f)

  and lambda_map_effect (name, body) ~f = name, map_effect body ~f

  and fix_lambda_map_effect (fun_, lambda) ~f =
    fun_, lambda_map_effect lambda ~f

  and handler_map_effect { handled_effect; operations; return_clause } ~f =
    let operations =
      Map.map operations ~f:(fun (op_shape, op_handler) ->
        op_shape, op_handler_map_effect op_handler ~f)
    in
    let return_clause =
      Option.map return_clause ~f:(op_handler_map_effect ~f)
    in
    { handled_effect; operations; return_clause }

  and op_handler_map_effect { op_argument; op_body } ~f =
    let op_body = map_effect op_body ~f in
    { op_argument; op_body }

  and impure_built_in_map_effect impure_built_in ~f =
    match impure_built_in with
    | Impure_println -> Impure_println
    | Impure_read_int -> Impure_read_int
    | Impure_print_int { value; newline } ->
      let value = map_effect value ~f in
      Impure_print_int { value; newline }
  ;;
end

module Decl = struct
  module Fun = struct
    type 'e t = 'e Expr.fix_lambda [@@deriving sexp_of]

    let map_effect t ~f = Expr.fix_lambda_map_effect t ~f
  end

  type 'e t =
    | Fun of 'e Fun.t
    | Effect of Effect_decl.t
  [@@deriving sexp_of]

  let map_effect t ~f =
    match t with
    | Fun fun_ -> Fun (Fun.map_effect fun_ ~f)
    | Effect e -> Effect e
  ;;
end

module Program = struct
  type 'e t = { declarations : 'e Decl.t list } [@@deriving sexp_of]

  let map_effect { declarations } ~f =
    let declarations = List.map declarations ~f:(Decl.map_effect ~f) in
    { declarations }
  ;;
end
