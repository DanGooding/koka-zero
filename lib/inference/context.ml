open Core

module Entry = struct
  type t =
    { type_ : Type.t
    ; shadowable : bool
    }
  [@@deriving sexp]
end

type t = Entry.t Minimal_syntax.Variable.Map.t [@@deriving sexp]

let can_bind t var =
  match Map.find t var with
  | Some { Entry.shadowable; _ } -> shadowable
  | None -> true
;;

let extend t ?(shadowable = true) ~var ~type_ =
  (* TODO: requiring two map lookups to do this is probably unnecessary *)
  if can_bind t var
  then
    Some
      (let new_entry = { Entry.shadowable; type_ } in
       Map.set t ~key:var ~data:new_entry)
  else None
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
