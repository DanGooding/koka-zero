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

module Variable : sig
  type t =
    | User of string
        (** user code can contain all possible names (except keywords), so we
            namespace them separately from internally used names *)
    | Language of string
        (** meaningful names internal to the language implementation *)
    | Generated of string
        (** sequentially generated names. Each generator is expected to use a
            unique prefix *)
  [@@deriving compare, sexp]

  val of_user : string -> t
  val of_language_internal : string -> t
  val of_generated : string -> t

  include Comparable.S with type t := t

  (* type changed to prevent accidental usage *)
  val of_string : string -> unit
    [@@deprecated "use one of the descriptive [of_...] constructor functions"]

  (** convert back to a string, retuning `User` strings unchanged, and the rest
      in sexp form *)
  val to_string_user : t -> string

  module Name_source : sig
    include Name_source.S with type Name.t := t
  end
end

(** names which aren't reserved, but have significance *)
module Keyword : sig
  val resume : Variable.t
  val main : Variable.t
  val entry_point : Variable.t
end

module Expr : sig
  type t =
    | Value of value
    | Let of Variable.t * value * t
        (** [Let] provides polymorphic binding over total terms, although I
            currently restrict this to values for the evidence translation step
            (this may be unnecessary) *)
    | Application of t * t list
    | Seq of t * t
        (** evaluates first expression, then second. both may be of any type *)
    | If_then_else of t * t * t
    | Operator of t * Operator.t * t
    | Unary_operator of Operator.Unary.t * t
  [@@deriving sexp]

  (** expressions which can't reduce/evaluate *)
  and value =
    | Variable of Variable.t
    | Lambda of lambda
    | Fix_lambda of fix_lambda
    | Literal of Literal.t
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
