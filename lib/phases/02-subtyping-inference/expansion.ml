open! Core
open! Import

type t =
  { constraints : Constraints.t
  ; type_meta_to_var : Type.Variable.t Type.Metavariable.Table.t
  ; effect_meta_to_var : Effect.Variable.t Effect.Metavariable.Table.t
  ; type_variable_source : Type.Variable.Name_source.t
  ; effect_variable_source : Effect.Variable.Name_source.t
  }
[@@deriving sexp_of]

let create ~constraints ~type_variable_source ~effect_variable_source =
  let type_meta_to_var = Type.Metavariable.Table.create () in
  let effect_meta_to_var = Effect.Metavariable.Table.create () in
  { constraints
  ; type_variable_source
  ; effect_variable_source
  ; type_meta_to_var
  ; effect_meta_to_var
  }
;;

module Memo_table = struct
  type t =
    { types_by_polarity : Polar_type.t Type.Metavariable.Table.t Bool.Table.t
    ; effects_by_polarity :
        Polar_type.Effect.t Effect.Metavariable.Table.t Bool.Table.t
    }
  [@@deriving sexp_of]

  let create () =
    let types_by_polarity = Bool.Table.create () in
    let effects_by_polarity = Bool.Table.create () in
    { types_by_polarity; effects_by_polarity }
  ;;

  let find_or_add_type t meta ~polarity_positive ~default =
    let types =
      Hashtbl.find_or_add
        t.types_by_polarity
        polarity_positive
        ~default:Type.Metavariable.Table.create
    in
    Hashtbl.find_or_add types meta ~default
  ;;

  let find_or_add_effect t meta ~polarity_positive ~default =
    let effects =
      Hashtbl.find_or_add
        t.effects_by_polarity
        polarity_positive
        ~default:Effect.Metavariable.Table.create
    in
    Hashtbl.find_or_add effects meta ~default
  ;;
end

let rec expand_type_aux
          t
          (type_ : Type.Mono.t)
          ~polarity_positive
          ~(in_progress_type_metavariables : Type.Metavariable.Set.t)
          ~memo_table
  : Polar_type.t
  =
  match type_ with
  | Primitive p -> Primitive p
  | List element ->
    let element =
      expand_type_aux
        t
        element
        ~polarity_positive:true
        ~in_progress_type_metavariables
        ~memo_table
    in
    List element
  | Option element ->
    Option
      (expand_type_aux
         t
         element
         ~polarity_positive:true
         ~in_progress_type_metavariables
         ~memo_table)
  | Tuple elements ->
    Tuple
      (List.map
         elements
         ~f:
           (expand_type_aux
              t
              ~polarity_positive
              ~in_progress_type_metavariables
              ~memo_table))
  | Arrow (args, effect_, result) ->
    let args =
      List.map
        args
        ~f:
          (expand_type_aux
             t
             ~polarity_positive:(not polarity_positive)
             ~in_progress_type_metavariables
             ~memo_table)
    in
    let effect_ =
      expand_effect_aux
        t
        effect_
        ~polarity_positive
        ~in_progress_effect_metavariables:Effect.Metavariable.Set.empty
        ~memo_table
    in
    let result =
      expand_type_aux
        t
        result
        ~polarity_positive
        ~in_progress_type_metavariables
        ~memo_table
    in
    Arrow (args, effect_, result)
  | Metavariable meta ->
    expand_type_metavariable_aux
      t
      meta
      ~polarity_positive
      ~in_progress_type_metavariables
      ~memo_table

and expand_type_metavariable_aux
      t
      (meta : Type.Metavariable.t)
      ~polarity_positive
      ~(in_progress_type_metavariables : Type.Metavariable.Set.t)
      ~memo_table
  : Polar_type.t
  =
  let var =
    Hashtbl.find_or_add t.type_meta_to_var meta ~default:(fun () ->
      Type.Variable.Name_source.next_name t.type_variable_source)
  in
  match Set.mem in_progress_type_metavariables meta with
  | true ->
    (* recursive reference - leave unexpanded, it'll be bound in a Recursive type *)
    Variable var
  | false ->
    Memo_table.find_or_add_type
      memo_table
      meta
      ~polarity_positive
      ~default:(fun () ->
        let bound_types =
          let bounds = Constraints.get_type_bounds t.constraints meta in
          let bounds =
            match polarity_positive with
            | true -> Bounds.to_list bounds `lower
            | false -> Bounds.to_list bounds `upper
          in
          List.map bounds ~f:(fun bound_type ->
            expand_type_aux
              t
              bound_type
              ~polarity_positive
              ~in_progress_type_metavariables:
                (Set.add in_progress_type_metavariables meta)
              ~memo_table)
        in
        let bound_types = Polar_type.Variable var :: bound_types in
        let bound_types =
          List.dedup_and_sort bound_types ~compare:[%compare: Polar_type.t]
        in
        let combined : Polar_type.t =
          match bound_types with
          | [] -> assert false
          | [ bound ] -> bound
          | _ :: _ :: _ ->
            (match polarity_positive with
             | true -> Union bound_types
             | false -> Intersection bound_types)
        in
        let is_recursive =
          List.exists bound_types ~f:(fun type_ ->
            match (type_ : Polar_type.t) with
            | Variable _ -> false
            | Primitive _
            | Arrow _
            | List _
            | Option _
            | Tuple _
            | Union _
            | Intersection _
            | Recursive _ ->
              Set.mem (Polar_type.variables type_ ~polarity:`all) var)
        in
        match is_recursive with
        | false -> combined
        | true -> Recursive (var, combined))

and expand_effect_aux
      t
      (effect_ : Effect.t)
      ~polarity_positive
      ~(in_progress_effect_metavariables : Effect.Metavariable.Set.t)
      ~memo_table
  : Polar_type.Effect.t
  =
  match effect_ with
  | Labels labels -> Labels labels
  | Handled (labels, meta) ->
    let effect_ =
      expand_effect_metavariable_aux
        t
        meta
        ~polarity_positive
        ~in_progress_effect_metavariables
        ~memo_table
    in
    Handled (labels, effect_)
  | Metavariable meta ->
    expand_effect_metavariable_aux
      t
      meta
      ~polarity_positive
      ~in_progress_effect_metavariables
      ~memo_table

and expand_effect_metavariable_aux
      t
      (meta : Effect.Metavariable.t)
      ~polarity_positive
      ~in_progress_effect_metavariables
      ~memo_table
  : Polar_type.Effect.t
  =
  let var =
    Hashtbl.find_or_add t.effect_meta_to_var meta ~default:(fun () ->
      Effect.Variable.Name_source.next_name t.effect_variable_source)
  in
  match Set.mem in_progress_effect_metavariables meta with
  | true ->
    (* recursive reference - leave unexpanded, it'll be bound in a Recursive type *)
    Variable var
  | false ->
    Memo_table.find_or_add_effect
      memo_table
      meta
      ~polarity_positive
      ~default:(fun () ->
        let bound_effects =
          let bounds = Constraints.get_effect_bounds t.constraints meta in
          let bounds =
            match polarity_positive with
            | true -> Bounds.to_list bounds `lower
            | false -> Bounds.to_list bounds `upper
          in
          List.map bounds ~f:(fun bound_effect ->
            expand_effect_aux
              t
              bound_effect
              ~polarity_positive
              ~in_progress_effect_metavariables:
                (Set.add in_progress_effect_metavariables meta)
              ~memo_table)
        in
        let bound_effects =
          List.dedup_and_sort
            bound_effects
            ~compare:[%compare: Polar_type.Effect.t]
        in
        let bound_effects = Polar_type.Effect.Variable var :: bound_effects in
        let combined : Polar_type.Effect.t =
          match bound_effects with
          | [] -> assert false
          | [ bound ] -> bound
          | _ :: _ ->
            (match polarity_positive with
             | true -> Union bound_effects
             | false -> Intersection bound_effects)
        in
        let is_recursive =
          List.exists bound_effects ~f:(fun effect_ ->
            match (effect_ : Polar_type.Effect.t) with
            | Variable _ -> false
            | Labels _ | Handled _ | Union _ | Intersection _ | Recursive _ ->
              Set.mem (Polar_type.Effect.variables effect_ ~polarity:`all) var)
        in
        match is_recursive with
        | false -> combined
        | true -> Recursive (var, combined))
;;

let expand_type ?context_type ?(simplify = true) t type_ =
  let full_type =
    expand_type_aux
      t
      type_
      ~polarity_positive:true
      ~in_progress_type_metavariables:Type.Metavariable.Set.empty
      ~memo_table:(Memo_table.create ())
  in
  if simplify then Polar_type.simplify full_type ?context_type else full_type
;;

let expand_effect ?context_type ?(simplify = true) t effect_ =
  let full_effect =
    expand_effect_aux
      t
      effect_
      ~polarity_positive:true
      ~in_progress_effect_metavariables:Effect.Metavariable.Set.empty
      ~memo_table:(Memo_table.create ())
  in
  if simplify
  then Polar_type.simplify_effect ?context_type full_effect
  else full_effect
;;
