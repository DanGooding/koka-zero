open Core

module Literal = struct
  module T = struct
    type t =
      | Int of int
      | Bool of bool
      | Unit
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T
end

module Operator = struct
  module Int = struct
    module T = struct
      type t =
        | Plus
        | Minus
        | Times
        | Divide
        | Modulo
        | Equals
        | Not_equal
        | Less_than
        | Less_equal
        | Greater_than
        | Greater_equal
      [@@deriving sexp]
    end (* disable "fragile-match" for generated code *) [@warning "-4"]

    include T
  end

  module Bool = struct
    module Unary = struct
      module T = struct
        type t = Not [@@deriving sexp]
      end (* disable "fragile-match" for generated code *) [@warning "-4"]

      include T
    end

    module T = struct
      type t =
        | And
        | Or
      [@@deriving sexp]
    end (* disable "fragile-match" for generated code *) [@warning "-4"]

    include T
  end

  module Unary = struct
    module T = struct
      type t = Bool of Bool.Unary.t [@@deriving sexp]
    end (* disable "fragile-match" for generated code *) [@warning "-4"]

    include T
  end

  module T = struct
    type t =
      | Int of Int.t
      | Bool of Bool.t
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T
end

module Keyword = struct
  let resume = Variable.of_user "resume"
  let main = Variable.of_user "main"
  let entry_point = Variable.of_language_internal "main"
  let console_effect = Effect.Label.of_string "console"
  let println = Variable.of_user "println"
  let println_int = Variable.of_user "println-int"
  let print_int = Variable.of_user "print-int"
  let read_int = Variable.of_user "read-int"
end

module Parameter = struct
  module T = struct
    type t =
      | Variable of Variable.t
      | Wildcard
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T
end

module Expr = struct
  module T = struct
    type t =
      | Value of value
      | Let of Variable.t * value * t
      | Let_mono of Variable.t * t * t
      | Application of t * t list
      | Seq of t * t
      | If_then_else of t * t * t
      | Operator of t * Operator.t * t
      | Unary_operator of Operator.Unary.t * t
      | Impure_built_in of impure_built_in
    [@@deriving sexp]

    and value =
      | Variable of Variable.t
      | Lambda of lambda
      | Fix_lambda of fix_lambda
      | Literal of Literal.t
      | Handler of handler
    [@@deriving sexp]

    and lambda = Parameter.t list * t [@@deriving sexp]
    and fix_lambda = Variable.t * lambda [@@deriving sexp]

    and handler =
      { operations : (Operation_shape.t * op_handler) Variable.Map.t
      ; return_clause : op_handler option
      }
    [@@deriving sexp]

    and op_handler =
      { op_argument : Parameter.t
      ; op_body : t
      }
    [@@deriving sexp]

    and impure_built_in =
      | Impure_println
      | Impure_print_int of
          { value : t
          ; newline : bool
          }
      | Impure_read_int
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T
end

module Decl = struct
  module Effect = struct
    module Operation = struct
      type t =
        { shape : Operation_shape.t
        ; argument : Type.Mono.t
        ; answer : Type.Mono.t
        }
      [@@deriving sexp]
    end

    type t =
      { name : Effect.Label.t
      ; operations : Operation.t Variable.Map.t
      }
    [@@deriving sexp]

    let console =
      let name = Effect.Label.of_string "console" in
      let operations =
        [ ( Variable.of_user "println"
          , { Operation.shape = Operation_shape.Fun
            ; argument = Type.Mono.Primitive Type.Primitive.Unit
            ; answer = Type.Mono.Primitive Type.Primitive.Unit
            } )
        ; ( Variable.of_user "println-int"
          , { Operation.shape = Operation_shape.Fun
            ; argument = Type.Mono.Primitive Type.Primitive.Int
            ; answer = Type.Mono.Primitive Type.Primitive.Unit
            } )
        ; ( Variable.of_user "print-int"
          , { Operation.shape = Operation_shape.Fun
            ; argument = Type.Mono.Primitive Type.Primitive.Int
            ; answer = Type.Mono.Primitive Type.Primitive.Unit
            } )
        ; ( Variable.of_user "read-int"
          , { Operation.shape = Operation_shape.Fun
            ; argument = Type.Mono.Primitive Type.Primitive.Unit
            ; answer = Type.Mono.Primitive Type.Primitive.Int
            } )
        ]
        |> Variable.Map.of_alist_exn
      in
      { name; operations }
    ;;
  end

  module Fun = struct
    type t = Expr.fix_lambda [@@deriving sexp]
  end

  module T = struct
    type t =
      | Fun of Fun.t
      | Effect of Effect.t
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T
end

module Program = struct
  type t = { declarations : Decl.t list } [@@deriving sexp]

  (** {[
        fun entry_point() {
          with handler {
            fun println(_)     { impure_println(); };
            fun println-int(x) { impure_print_int(x, newline=true); };
            fun print-int(x)   { impure_print_int(x, newline=false); };
            fun read-int(_)    { impure_read_int(); };
          };
          main();
          ()
        }
      ]} *)
  let entry_point =
    let console_handler =
      let println_clause =
        { Expr.op_argument = Parameter.Wildcard
        ; op_body = Expr.Impure_built_in Expr.Impure_println
        }
      in
      let println_int_clause =
        let arg = Variable.of_language_internal "x" in
        let value = Expr.Value (Expr.Variable arg) in
        let newline = true in
        { Expr.op_argument = Parameter.Variable arg
        ; op_body =
            Expr.Impure_built_in (Expr.Impure_print_int { value; newline })
        }
      in
      let print_int_clause =
        let arg = Variable.of_language_internal "x" in
        let value = Expr.Value (Expr.Variable arg) in
        let newline = false in
        { Expr.op_argument = Parameter.Variable arg
        ; op_body =
            Expr.Impure_built_in (Expr.Impure_print_int { value; newline })
        }
      in
      let read_int_clause =
        { Expr.op_argument = Parameter.Wildcard
        ; op_body = Expr.Impure_built_in Expr.Impure_read_int
        }
      in
      let operations =
        [ Keyword.println, (Operation_shape.Fun, println_clause)
        ; Keyword.println_int, (Operation_shape.Fun, println_int_clause)
        ; Keyword.print_int, (Operation_shape.Fun, print_int_clause)
        ; Keyword.read_int, (Operation_shape.Fun, read_int_clause)
        ]
        |> Variable.Map.of_alist_exn
      in
      { Expr.operations; return_clause = None }
    in
    ( Keyword.entry_point
    , ( []
      , Expr.Seq
          ( Expr.Application
              ( Expr.Value (Expr.Handler console_handler)
              , [ Expr.Value (Expr.Variable Keyword.main) ] )
          , Expr.Value (Expr.Literal Literal.Unit) ) ) )
  ;;
end
