module Or_cannot_shadow : sig
  (** holds the result of an operation which may fail, due to shadowing an
      unshadowable binding *)
  type 'a t =
    [ `Ok of 'a
    | `Cannot_shadow
    ]
  [@@deriving sexp]
end

(** represents a typing context, mapping [Minimal_syntax.Variable.t]s to
    [Type.t]s, and tracking whether those variables can be shadowed *)
type t [@@deriving sexp]

(** add a new variable to the context, or shadow an existing one. This returns
    [`Cannot_shadow] if [var] is not shadowable *)
val extend
  :  t
  -> var:Minimal_syntax.Variable.t
  -> type_:Type.t
  -> t Or_cannot_shadow.t

(** add a new variable, which cannot be shadowed later, to the context. This
    returns [`Cannot_shadow] if [var] is already present *)
val extend_unshadowable
  :  t
  -> var:Minimal_syntax.Variable.t
  -> type_:Type.t
  -> t Or_cannot_shadow.t

(** lookup a variable's type in the context *)
val find : t -> Minimal_syntax.Variable.t -> Type.t option

(** a context with no names in scope *)
val empty : t

(* TODO: [Context.apply_substitution] is inconsistent with
   [Substitution.apply_to_mono] *)
val apply_substitution : t -> Substitution.t -> t
val metavariables : t -> Type.Metavariable.Set.t * Effect.Metavariable.Set.t
