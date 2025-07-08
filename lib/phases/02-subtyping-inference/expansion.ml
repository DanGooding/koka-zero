open! Core
open! Import

type t =
  { constraints : Constraints.t
  ; type_meta_to_var : Type.Variable.t Type.Metavariable.Table.t
  ; effect_meta_to_var : Effect.Variable.t Effect.Metavariable.Table.t
  ; type_variable_source : Type.Variable.Name_source.t
  ; effect_variable_source : Effect.Variable.Name_source.t
  }

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

let rec expand_type_aux
          t
          (type_ : Type.Mono.t)
          ~polarity_positive
          ~(in_progress_type_metavariables : Type.Metavariable.Set.t)
  : Polar_type.t
  =
  match type_ with
  | Primitive p -> Primitive p
  | Arrow (args, effect_, result) ->
    let args =
      List.map
        args
        ~f:
          (expand_type_aux
             t
             ~polarity_positive:(not polarity_positive)
             ~in_progress_type_metavariables)
    in
    let effect_ =
      expand_effect_aux
        t
        effect_
        ~polarity_positive
        ~in_progress_effect_metavariables:Effect.Metavariable.Set.empty
    in
    let result =
      expand_type_aux
        t
        result
        ~polarity_positive
        ~in_progress_type_metavariables
    in
    Arrow (args, effect_, result)
  | Metavariable meta ->
    let var =
      Hashtbl.find_or_add t.type_meta_to_var meta ~default:(fun () ->
        Type.Variable.Name_source.next_name t.type_variable_source)
    in
    (match Set.mem in_progress_type_metavariables meta with
     | true ->
       (* recursive reference - leave unexpanded, it'll be bound in a Recursive type *)
       Variable var
     | false ->
       let bounds =
         Constraints.get_type_bounds t.constraints meta
         |> Option.value_map
              ~f:(fun (bounds : _ Bounds.t) ->
                match polarity_positive with
                | true -> bounds.lower_bounds
                | false -> bounds.upper_bounds)
              ~default:[]
       in
       let bound_types =
         List.map bounds ~f:(fun bound_type ->
           expand_type_aux
             t
             bound_type
             ~polarity_positive
             ~in_progress_type_metavariables:
               (Set.add in_progress_type_metavariables meta))
       in
       let is_recursive =
         List.exists bound_types ~f:(fun type_ ->
           Set.mem (Polar_type.variables type_) var)
       in
       let combined : Polar_type.t =
         match polarity_positive with
         | true -> Union bound_types
         | false -> Intersection bound_types
       in
       (match is_recursive with
        | true -> Recursive (var, combined)
        | false ->
          (match bound_types with
           | [] -> Variable var
           | _ :: _ -> combined)))

and expand_effect_aux
      t
      (effect_ : Effect.t)
      ~polarity_positive
      ~(in_progress_effect_metavariables : Effect.Metavariable.Set.t)
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
    in
    Handled (labels, effect_)
  | Metavariable meta ->
    expand_effect_metavariable_aux
      t
      meta
      ~polarity_positive
      ~in_progress_effect_metavariables

and expand_effect_metavariable_aux
      t
      (meta : Effect.Metavariable.t)
      ~polarity_positive
      ~in_progress_effect_metavariables
  : Polar_type.Effect.t
  =
  (* TODO: avoid duplication with the type case *)
  let var =
    Hashtbl.find_or_add t.effect_meta_to_var meta ~default:(fun () ->
      Effect.Variable.Name_source.next_name t.effect_variable_source)
  in
  match Set.mem in_progress_effect_metavariables meta with
  | true ->
    (* recursive reference - leave unexpanded, it'll be bound in a Recursive type *)
    Variable var
  | false ->
    let bounds =
      Constraints.get_effect_bounds t.constraints meta
      |> Option.value_map
           ~f:(fun (bounds : _ Bounds.t) ->
             match polarity_positive with
             | true -> bounds.lower_bounds
             | false -> bounds.upper_bounds)
           ~default:[]
    in
    let bound_effects =
      List.map bounds ~f:(fun bound_effect ->
        expand_effect_aux
          t
          bound_effect
          ~polarity_positive
          ~in_progress_effect_metavariables:
            (Set.add in_progress_effect_metavariables meta))
    in
    let is_recursive =
      List.exists bound_effects ~f:(fun type_ ->
        Set.mem (Polar_type.Effect.variables type_) var)
    in
    let combined : Polar_type.Effect.t =
      match polarity_positive with
      | true -> Union bound_effects
      | false -> Intersection bound_effects
    in
    (match is_recursive with
     | true -> Recursive (var, combined)
     | false ->
       (match bound_effects with
        | [] -> Variable var
        | _ :: _ -> combined))
;;

let expand_type t type_ =
  expand_type_aux
    t
    type_
    ~polarity_positive:true
    ~in_progress_type_metavariables:Type.Metavariable.Set.empty
  |> Polar_type.simplify
;;

let expand_effect t effect_ =
  expand_effect_aux
    t
    effect_
    ~polarity_positive:true
    ~in_progress_effect_metavariables:Effect.Metavariable.Set.empty
  |> Polar_type.Effect.simplify
;;
