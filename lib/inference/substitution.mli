(* TODO: this isn't really a substitution anymore - more like a metavariable
   context *)
(** a substitution maps metavariabes to monotypes, which may themselves contain
    more metavariables *)
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

(** lookup a metavariable's type *)
val find : t -> Type.Metavariable.t -> Type.Mono.t option

(** replace all metavaraibes with their known types *)
val apply : t -> Type.t -> Type.t

val apply_to_mono : t -> Type.Mono.t -> Type.Mono.t
val apply_to_primitive : t -> Type.Primitive.t -> Type.Primitive.t
