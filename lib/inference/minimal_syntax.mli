open Core

module Literal : sig
  type t =
    | Int of int
    | Bool of bool
    | Unit
  [@@deriving sexp]
end

module Operator : sig
  module Int : sig
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
  end

  module Bool : sig
    module Unary : sig
      type t = Not [@@deriving sexp]
    end

    type t =
      | And
      | Or
    [@@deriving sexp]
  end

  module Unary : sig
    type t = Bool of Bool.Unary.t [@@deriving sexp]
  end

  type t =
    | Int of Int.t
    | Bool of Bool.t
  [@@deriving sexp]
end

(* TODO: work out identifier/var_id/wildcard etc.*)
module Variable : Identifiable.S

(** names which aren't reserved, but have significance *)
module Keyword : sig
  (* TODO: ensure resume is not used in a first-class way *)
  val resume : Variable.t
  val main : Variable.t
  val entry_point : Variable.t
end

module Expr : sig
  type t =
    | Variable of Variable.t
    | Let of Variable.t * t * t (** [Let] provides polymorphic binding *)
    | Lambda of lambda
    | Fix_lambda of fix_lambda
    | Application of t * t list
    | Literal of Literal.t
    | If_then_else of t * t * t
    | Operator of t * Operator.t * t
    | Unary_operator of Operator.Unary.t * t
    | Handler of handler
        (** takes a nullary funciton to be called under this handler *)
  [@@deriving sexp]

  (** monomorphic binding *)
  and lambda = Variable.t list * t [@@deriving sexp]

  (** lambda which knows its own name *)
  and fix_lambda = Variable.t * lambda [@@deriving sexp]

  (** an effect handler *)
  and handler =
    { operations : op_handler Variable.Map.t
    ; return_clause : op_handler option
    }
  [@@deriving sexp]

  (** handler clause for a single operation - part of a [handler] *)
  and op_handler =
    { op_argument : Variable.t
          (* TODO: extend to multiple args (requires checking against
             declaration, and makes translation harder - each operation in an
             effect needs to pass a differnt amount of arguments through
             yield) *)
    ; op_body : t
    }
  [@@deriving sexp]
end

module Decl : sig
  module Effect : sig
    module Operation : sig
      type t =
        { (* TODO: different shapes (fun/var/ctl/except) *)
          argument : Type.Mono.t
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

  module Fun : sig
    (** toplevel function - implicitly generalised *)
    type t = Expr.fix_lambda [@@deriving sexp]
  end

  type t =
    | Fun of Fun.t
    | Effect of Effect.t
  [@@deriving sexp]
end

module Program : sig
  type t =
    { declarations : Decl.t list
    ; has_main : bool
    }
  [@@deriving sexp]

  (** wrapper funciton which calls the user's main function, which is appended
      to programs with [has_main = true]. It may provide toplevel handers for
      e.g. io/exn/div effects. *)
  val entry_point : Decl.Fun.t
end
