open! Core
open! Import

(** The signature of an effect is the set of operation names. This is used for
    structural matching to handlers *)
type t [@@deriving sexp_of]

include Comparable.S_plain with type t := t

val t_of_handler : Minimal_syntax.Expr.handler -> t

module Context : sig
  type signature := t

  (** Maps signatures to their effect labels and operation shapes. This is the
      extra information needed at a handler, but not when performing an
      operation (which only looks in [Context.t]) *)
  type t [@@deriving sexp_of]

  val empty : t

  val extend
    :  t
    -> label:Effect.Label.t
    -> operation_shapes:Operation_shape.t Variable.Map.t
    -> [ `Ok of t | `Duplicate ]

  val extend_decl : t -> Effect_decl.t -> [ `Ok of t | `Duplicate ]

  val find
    :  t
    -> signature
    -> (Effect.Label.t * Operation_shape.t Variable.Map.t) option
end
