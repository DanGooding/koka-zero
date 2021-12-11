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
        | Less_than
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

module Variable : Identifiable.S = String

module Keyword = struct
  let resume = Variable.of_string "resume"
end

module Expr = struct
  module T = struct
    type t =
      | Variable of Variable.t
      | Let of Variable.t * t * t
      | Lambda of Variable.t * t
      | Fix of Variable.t * t
      (* TODO: syntactically, `fix` can only wrap a lambda - perhaps enforce
         this? *)
      | Application of t * t
      | Literal of Literal.t
      | If_then_else of t * t * t
      | Operator of t * Operator.t * t
      | Unary_operator of Operator.Unary.t * t
      | Handle of handler * t
    [@@deriving sexp]

    and handler =
      { operations : op_handler Variable.Map.t
      ; return_clause : op_handler option
      }
    [@@deriving sexp]

    and op_handler =
      { op_argument : Variable.t
            (* TODO: extend to multiple args (requires checking against
               declaration) *)
      ; op_body : t
      }
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T
end

module Effect_decl = struct
  module Operation = struct
    type t =
      { argument : Type.Mono.t
      ; result : Type.Mono.t
      }
    [@@deriving sexp]
  end

  type t =
    { name : Effect.Label.t
    ; operations : Operation.t Variable.Map.t
    }
  [@@deriving sexp]
end

module Program = struct
  type t =
    { effect_declarations : Effect_decl.t list
    ; body : Expr.t
    }
  [@@deriving sexp]
end
