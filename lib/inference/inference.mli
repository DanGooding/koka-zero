open Core
open Koka_zero_util

(* TODO: having both [Infer] and [Inference] is terrible naming! *)

(** monad tracking the state of type inference

    encapsulates a global substitution, and type variable sources internally, as
    well as failures producing [Static_error.t]s *)
type 'a t

include Monad.S with type 'a t := 'a t

val fresh_variable : Type.Variable.t t
val fresh_metavariable : Type.Metavariable.t t
val lookup_meta : Type.Metavariable.t -> Type.Mono.t option t

(* TODO: is this used externally? *)

(** unifies a metavariable with a concrete type, raising an exception if this
    has already been done for this metavariable *)
val substitute_meta_exn : var:Type.Metavariable.t -> type_:Type.Mono.t -> unit t

val type_error : string -> 'a t
val unification_error : Type.t -> Type.t -> 'a t
val unification_error_mono : Type.Mono.t -> Type.Mono.t -> 'a t
val unify : Type.Mono.t -> Type.Mono.t -> unit t
val instantiate : Type.Poly.t -> Type.Mono.t t
val generalise : Type.Mono.t -> in_:Context.t -> Type.Poly.t t
val run : 'a t -> ('a * Substitution.t, Static_error.t) Result.t
