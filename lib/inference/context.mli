(** represents a typing context, mapping [Minimal_syntax.Variable.t]s to
    [Type.t]s *)
type t [@@deriving sexp]

(** add a new variable to the context, or shadow an existing one. This returns
    [None] if [var] is not shadowable *)
val extend
  :  ?shadowable:bool
  -> t
  -> var:Minimal_syntax.Variable.t
  -> type_:Type.t
  -> t option

(** lookup a variable's type in the context *)
val find : t -> Minimal_syntax.Variable.t -> Type.t option

(** a context with no names in scope *)
val empty : t

(* TODO: [Context.apply_substitution] is inconsistent with
   [Substitution.apply_to_mono] *)
val apply_substitution : t -> Substitution.t -> t
val metavariables : t -> Type.Metavariable.Set.t * Effect.Metavariable.Set.t
