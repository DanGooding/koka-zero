open Core

(** The signature of an effect is the set of operation names. This is used for
    structural matching to handlers *)
type t [@@deriving sexp]

include Comparable.S_plain with type t := t

val t_of_handler : Minimal_syntax.Expr.handler -> t

module Context : sig
  type signature := t

  (** maps signatures to their effect labels *)
  type t [@@deriving sexp]

  val empty : t

  val extend
    :  t
    -> label:Effect.Label.t
    -> signature:signature
    -> [ `Ok of t | `Duplicate ]

  val extend_decl
    :  t
    -> Minimal_syntax.Decl.Effect.t
    -> [ `Ok of t | `Duplicate ]

  val find : t -> signature -> Effect.Label.t option
end
