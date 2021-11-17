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
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T
  (* TODO: operators, handlers *)
end
