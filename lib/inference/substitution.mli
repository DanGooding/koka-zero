(* TODO: this isn't really a substitution anymore - more like a metavariable
   context *)
(** a substitution maps type metavariables to monotypes, and effect
    metavaraiables to effects, which may themselves contain more metavariables *)
type t [@@deriving sexp]

(** the empty substitution *)
val identity : t

(** try to add a new metavariable and the type it maps to, it should not already
    exist *)
val extend
  :  t
  -> var:Type.Metavariable.t
  -> type_:Type.Mono.t
  -> [ `Ok of t | `Duplicate ]

(** add a metavariable and its type, raising an exception if it already exists
    in the substitution*)
val extend_exn : t -> var:Type.Metavariable.t -> type_:Type.Mono.t -> t

(** add multiple new metavariable-type mappings, returning [`Duplicate] if any
    of the metavariables already exist *)
val extend_many
  :  t
  -> Type.Mono.t Type.Metavariable.Map.t
  -> [ `Ok of t | `Duplicate ]

(** add multiple new metavariable-type mappings, raising an exception if any of
    the metavariables already exist *)
val extend_many_exn : t -> Type.Mono.t Type.Metavariable.Map.t -> t

(** Lookup a metavariable's type. Like [apply] this recursively replaces
    metavariables in the result until the only unknown metavariables remain. If
    the queried metavariable itself is unknown, then the result is None *)
val lookup : t -> Type.Metavariable.t -> Type.Mono.t option

(** Replace all metavaraibes with their known types. Any unknown metavariables
    will still be present in the result *)
val apply : t -> Type.t -> Type.t

val apply_to_mono : t -> Type.Mono.t -> Type.Mono.t
val apply_to_primitive : t -> Type.Primitive.t -> Type.Primitive.t
val apply_to_effect : t -> Effect.t -> Effect.t
val apply_to_effect_row : t -> Effect.Row.t -> Effect.Row.t
