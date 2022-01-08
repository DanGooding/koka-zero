open Core
open Koka_zero_util

(* TODO: having both [Infer] and [Inference] is terrible naming! *)

(** monad tracking the state of type inference

    encapsulates a global substitution, and type variable sources internally, as
    well as failures producing [Static_error.t]s *)
type 'a t

include Monad.S with type 'a t := 'a t
include Monad_utils.S with type 'a t := 'a t

val fresh_variable : Type.Variable.t t
val fresh_metavariable : Type.Metavariable.t t
val fresh_effect_variable : Effect.Variable.t t
val fresh_effect_metavariable : Effect.Metavariable.t t
val lookup_meta : Type.Metavariable.t -> Type.Mono.t option t
val lookup_effect_meta : Effect.Metavariable.t -> Effect.t option t

(* TODO: is this used externally? *)

(** unifies a metavariable with a concrete type, raising an exception if this
    has already been done for this metavariable *)
val substitute_meta_exn : var:Type.Metavariable.t -> type_:Type.Mono.t -> unit t

(** add a fresh metavaraible for a type's effect *)
val with_any_effect : 'a -> ('a * Effect.t) t

val type_error : string -> 'a t
val unification_error : Type.t -> Type.t -> 'a t
val unification_error_mono : Type.Mono.t -> Type.Mono.t -> 'a t
val unification_error_effect : Effect.t -> Effect.t -> 'a t
val unification_error_effect_row : Effect.Row.t -> Effect.Row.t -> 'a t

(** make two types equal if possible (by updating the metavariable substitution)
    or otherwise fail with a type error *)
val unify : Type.Mono.t -> Type.Mono.t -> unit t

val unify_effects : Effect.t -> Effect.t -> unit t
val instantiate : Type.Poly.t -> Type.Mono.t t

(** generalise a type, or fail if the effect is not total *)
val generalise : Type.Mono.t -> Effect.t -> env:Context.t -> Type.Poly.t t

(** generalise a type (assuming the corresponding effect is total) *)
val generalise_total : Type.Mono.t -> env:Context.t -> Type.Poly.t t

val run : 'a t -> ('a * Substitution.t, Static_error.t) Result.t
