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
(* TODO: work out identifier/var_id/wildcard etc.*)

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
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T
  (* TODO: operators, handlers *)
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
