open! Core
open! Import

module Or_cannot_shadow : sig
  (** holds the result of an operation which may fail, due to shadowing an
      unshadowable binding *)
  type 'a t =
    [ `Ok of 'a
    | `Cannot_shadow
    ]
  [@@deriving sexp_of]
end

module Binding : sig
  (** represents the different things a variable can map to *)
  type t =
    | Value of Type.t
    | Operation of
        { argument : Type.Mono.t
        ; label : Effect.Label.t
        ; answer : Type.Mono.t
        }
  [@@deriving sexp_of]
end

(** represents a typing context, mapping [Variable.t]s to [Binding.t]s, and
    tracking whether those variables can be shadowed *)
type t [@@deriving sexp_of]

(** add a new variable to the context, or shadow an existing one. This returns
    [`Cannot_shadow] if [var] is not shadowable *)
val extend : t -> var:Variable.t -> type_:Type.t -> t Or_cannot_shadow.t

(** for a toplevel environment, add a new variable - failing if [var] is already
    present: toplevel declarations must be unique, although they can be shadowed
    locally *)
val extend_toplevel
  :  t
  -> var:Variable.t
  -> type_:Type.t
  -> t Or_cannot_shadow.t

(** add a new variable, which cannot be shadowed later, to the context. This
    returns [`Cannot_shadow] if [var] is already present. The variable gets
    bound to [Binding.Operation (label, type_)] *)
val extend_operation
  :  t
  -> var:Variable.t
  -> label:Effect.Label.t
  -> argument:Type.Mono.t
  -> answer:Type.Mono.t
  -> t Or_cannot_shadow.t

(** lookup a variable's type in the context *)
val find : t -> Variable.t -> Binding.t option

(** a context with no names in scope *)
val empty : t
