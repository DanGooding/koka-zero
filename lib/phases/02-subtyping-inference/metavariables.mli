open! Core
open! Import

module Location : sig
  type t =
    | Expr of Minimal_syntax.Expr.t
    | Parameter of Parameter.t
    | Pattern of Pattern.t
    | F_self of Variable.t (** the name of a recursive function *)
    | Application of Minimal_syntax.Expr.t * Minimal_syntax.Expr.t list
    (** the effect resulting specifically from the call, not from evaluating the arguments
    *)
    | Handler_subject of Minimal_syntax.Expr.handler
    (** the type/effect wrapped within the handler *)
    | Handler_result of Minimal_syntax.Expr.handler
    (** when a handler wraps a function and is run, this is the result type/effect
    *)
    | List_element of t (** for a type `list<a>` this is the element type `a` *)
    | Instantiation of t
    | Entry_point
  [@@deriving sexp_of]
end

(** manages the metadata of metavariables *)
type t [@@deriving sexp_of]

val create : unit -> t

val fresh_type_metavariable
  :  t
  -> level:int
  -> location:Location.t
  -> Type.Metavariable.t

val fresh_effect_metavariable
  :  t
  -> level:int
  -> location:Location.t
  -> Effect.Metavariable.t

val fresh_type : t -> level:int -> location:Location.t -> Type.Mono.t
val fresh_effect : t -> level:int -> location:Location.t -> Effect.t
val type_level_exn : t -> Type.Metavariable.t -> int
val effect_level_exn : t -> Effect.Metavariable.t -> int
val type_location_exn : t -> Type.Metavariable.t -> Location.t
val effect_location_exn : t -> Effect.Metavariable.t -> Location.t
val sexp_of_type : t -> Type.Mono.t -> Sexp.t
val sexp_of_effect : t -> Effect.t -> Sexp.t
