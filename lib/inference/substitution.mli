type t [@@deriving sexp]

(* TODO: this isn't really a substitution anymore - more like a metavariable
   context *)

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

(* val apply : t -> -> ? *)

(** lookup a metavariable's type *)
val find : t -> Type.Metavariable.t -> Type.Mono.t option
