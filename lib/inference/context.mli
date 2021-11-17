(** represents a typing context, mapping [Minimal_syntax.Variable.t]s to
    [Type.t]s *)
type t [@@deriving sexp]

(** add a new variaable to the context, or shadow an existing one *)
val extend : t -> var:Minimal_syntax.Variable.t -> type_:Type.t -> t

(** lookup a variable's type in the context *)
val find : t -> Minimal_syntax.Variable.t -> Type.t option

(** a context with no names in scope*)
val empty : t
