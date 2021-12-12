open Core

module Entry = struct
  type t =
    { type_ : Type.t
    ; shadowable : bool
    }
  [@@deriving sexp]
end

module Or_cannot_shadow = struct
  type 'a t =
    [ `Ok of 'a
    | `Cannot_shadow
    ]
  [@@deriving sexp]
end

type t = Entry.t Minimal_syntax.Variable.Map.t [@@deriving sexp]

let can_bind t var =
  match Map.find t var with
  | Some { Entry.shadowable; _ } -> shadowable
  | None -> true
;;

let extend t ~var ~type_ =
  (* TODO: requiring two map lookups to do this is probably unnecessary *)
  if can_bind t var
  then (
    let entry = { Entry.shadowable = true; type_ } in
    `Ok (Map.set t ~key:var ~data:entry))
  else `Cannot_shadow
;;

let extend_unshadowable t ~var ~type_ =
  if Map.mem t var
  then `Cannot_shadow
  else (
    let entry = { Entry.shadowable = false; type_ } in
    `Ok (Map.set t ~key:var ~data:entry))
;;

let find t var =
  match Map.find t var with
  | None -> None
  | Some { Entry.type_; _ } -> Some type_
;;

let empty = Minimal_syntax.Variable.Map.empty

let apply_substitution t subst =
  Map.map t ~f:(fun entry ->
      let { Entry.type_; _ } = entry in
      let type_ = Substitution.apply subst type_ in
      { entry with Entry.type_ })
;;

let metavariables t =
  let metavariable_sets, effect_metavariable_sets =
    Map.map t ~f:(fun { Entry.type_; _ } -> Type.metavariables type_)
    |> Map.data
    |> List.unzip
  in
  ( Type.Metavariable.Set.union_list metavariable_sets
  , Effect.Metavariable.Set.union_list effect_metavariable_sets )
;;
