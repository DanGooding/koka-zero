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
end

module Expr = struct
  module T = struct
    type t =
      | Value of value
      | Let of Variable.t * value * t
      | Application of t * t list
      | Seq of t * t
      | If_then_else of t * t * t
      | Operator of t * Operator.t * t
      | Unary_operator of Operator.Unary.t * t
    [@@deriving sexp]

    and value =
      | Variable of Variable.t
      | Lambda of lambda
      | Fix_lambda of fix_lambda
      | Literal of Literal.t
      | Handler of handler
    [@@deriving sexp]

    and lambda = Variable.t list * t [@@deriving sexp]

    and fix_lambda = Variable.t * lambda [@@deriving sexp]

    and handler =
      { operations : op_handler Variable.Map.t
      ; return_clause : op_handler option
      }
    [@@deriving sexp]

    and op_handler =
      { op_argument : Variable.t
      ; op_body : t
      }
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T
end

module Decl = struct
  module Effect = struct
    module Operation = struct
      type t =
        { argument : Type.Mono.t
        ; answer : Type.Mono.t
        }
      [@@deriving sexp]
    end

    type t =
      { name : Effect.Label.t
      ; operations : Operation.t Variable.Map.t
      }
    [@@deriving sexp]
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

  let entry_point =
    (* {[ fun entry-point() { (fn(_result) { () }) (main()) } ]} *)
    let call_user_main =
      Expr.Application (Expr.Value (Expr.Variable Keyword.main), [])
    in
    ( Keyword.entry_point
    , ( []
      , Expr.Application
          ( Expr.Value
              (Expr.Lambda
                 ( [ Variable.of_language_internal "_result" ]
                 , Expr.Value (Expr.Literal Literal.Unit) ))
          , [ call_user_main ] ) ) )
  ;;
end
