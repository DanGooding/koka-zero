open Core
open Koka_zero_util

(* TODO: having both [Infer] and [Inference] is terrible naming! *)

(** monad tracking the state of type inference

    encapsulates a global substitution, and type variable sources internally, as
    well as failures producing [Static_error.t]s *)
type 'a t

include Monad.S with type 'a t := 'a t

(** sequence a list of computations into one, which returns a list of all their
    results if they all succeed, or the first error otherwise *)
val sequence : 'a t list -> 'a list t

(** a convenience form of [sequence] for when the computations only have side
    effects and don't produce values *)
val sequence_units : unit t list -> unit t

(** sequence computations given as a map's data into one computation, which
    returns a map of their pure results if they all succeed, or the first
    (according to the order of [Map.fold]) error otherwise *)
val sequence_map : ('a, 'b t, 'cmp) Map.t -> ('a, 'b, 'cmp) Map.t t

(** a more efficient form of [sequence_map] for when the computations only have
    side effects and don't produce values *)
val sequence_map_unit : ('a, unit t, 'cmp) Map.t -> unit t

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
val unify : Type.Mono.t -> Type.Mono.t -> unit t
val unify_effects : Effect.t -> Effect.t -> unit t
val instantiate : Type.Poly.t -> Type.Mono.t t

val generalise
  :  Type.Mono.t * Effect.t
  -> in_:Context.t
  -> (Type.t * Effect.t) t

val run : 'a t -> ('a * Substitution.t, Static_error.t) Result.t
