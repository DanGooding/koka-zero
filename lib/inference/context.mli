(* TODO: ensure representation allow shadowing *)
(** represents a typing context, mapping [Minimal_syntax.Variable.t]s to
    [Type.t]s *)
type t [@@deriving sexp]

(* TODO: should this check for shadowning? *)
val extend : t -> var:Minimal_syntax.Variable.t -> type_:Type.t -> t
val find : t -> Minimal_syntax.Variable.t -> Type.t option
val empty : t
