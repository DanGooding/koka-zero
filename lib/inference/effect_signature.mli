module Operation : sig
  type t =
    { argument_type : Type.Mono.t
    ; result_type : Type.Mono.t
    }
  [@@deriving sexp]
  (* TODO: include number of arguments when this becomes varaiable *)
end

(** The signature of an effect is the set of operation names. This is used for
    structural matching to handlers *)
type t [@@deriving sexp]

include Comparable.S_plain with type t := t

val of_handler : Minimal_syntax.Expr.handler -> t

module Context : sig
  type signature := t
  (** maps signatures to their effect labels *)
  type t [@@deriving sexp]

  val empty : t
  val extend : t -> label:Effect.Label.t -> signature:signature -> t
  val find : t -> signature -> Effect.Label.t option
end
