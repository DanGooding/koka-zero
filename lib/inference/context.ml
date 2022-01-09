open Core

module Binding = struct
  module T = struct
    type t =
      | Value of Type.t
      | Operation of Effect.Label.t * Type.t
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T

  let apply_substitution t subst =
    match t with
    | Value t -> Value (Substitution.apply subst t)
    | Operation (l, t) -> Operation (l, Substitution.apply subst t)
  ;;

  let metavariables = function
    | Value t -> Type.metavariables t
    | Operation (_l, t) -> Type.metavariables t
  ;;
end

module Entry = struct
  type t =
    { binding : Binding.t
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

type t = Entry.t Variable.Map.t [@@deriving sexp]

let can_bind t var =
  match Map.find t var with
  | Some { Entry.shadowable; _ } -> shadowable
  | None -> true
;;

let extend t ~var ~type_ =
  (* TODO: requiring two map lookups to do this is probably unnecessary *)
  if can_bind t var
  then (
    let binding = Binding.Value type_ in
    let entry = { Entry.shadowable = true; binding } in
    `Ok (Map.set t ~key:var ~data:entry))
  else `Cannot_shadow
;;

let extend_operation t ~var ~label ~type_ =
  if Map.mem t var
  then `Cannot_shadow
  else (
    let binding = Binding.Operation (label, type_) in
    let entry = { Entry.shadowable = false; binding } in
    `Ok (Map.set t ~key:var ~data:entry))
;;

let find t var =
  match Map.find t var with
  | None -> None
  | Some { Entry.binding; _ } -> Some binding
;;

let empty = Variable.Map.empty

let apply_substitution t subst =
  Map.map t ~f:(fun entry ->
      let { Entry.binding; _ } = entry in
      let binding = Binding.apply_substitution binding subst in
      { entry with Entry.binding })
;;

let metavariables t =
  let metavariable_sets, effect_metavariable_sets =
    Map.map t ~f:(fun { Entry.binding; _ } -> Binding.metavariables binding)
    |> Map.data
    |> List.unzip
  in
  ( Type.Metavariable.Set.union_list metavariable_sets
  , Effect.Metavariable.Set.union_list effect_metavariable_sets )
;;
