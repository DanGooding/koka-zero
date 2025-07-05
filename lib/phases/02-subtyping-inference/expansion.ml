open! Core
open! Import

type t =
  { constraints : Constraints.t
  ; in_progress_type_metavariables : Type.Variable.t Type.Metavariable.Table.t
  ; in_progress_effect_metavariables :
      Effect.Variable.t Effect.Metavariable.Table.t
  ; mutable type_variable_source : Type.Variable.Name_source.t
  ; mutable effect_variable_source : Effect.Variable.Name_source.t
  }

let create ~constraints =
  let type_variable_source = Type.Variable.Name_source.fresh () ~prefix:"t" in
  let effect_variable_source =
    Effect.Variable.Name_source.fresh () ~prefix:"e"
  in
  let in_progress_type_metavariables = Type.Metavariable.Table.create () in
  let in_progress_effect_metavariables = Effect.Metavariable.Table.create () in
  { constraints
  ; type_variable_source
  ; effect_variable_source
  ; in_progress_type_metavariables
  ; in_progress_effect_metavariables
  }
;;

let fresh_type_variable t : Type.Variable.t =
  let var, name_source =
    Type.Variable.Name_source.next_name t.type_variable_source
  in
  t.type_variable_source <- name_source;
  var
;;

let fresh_effect_variable t : Effect.Variable.t =
  let var, name_source =
    Effect.Variable.Name_source.next_name t.effect_variable_source
  in
  t.effect_variable_source <- name_source;
  var
;;

let rec expand_type_aux t (type_ : Type.Mono.t) ~polarity_positive
  : Polar_type.t
  =
  match type_ with
  | Primitive p -> Primitive p
  | Arrow (args, effect_, result) ->
    let args =
      List.map
        args
        ~f:(expand_type_aux t ~polarity_positive:(not polarity_positive))
    in
    let effect_ = expand_effect_aux t effect_ ~polarity_positive in
    let result = expand_type_aux t result ~polarity_positive in
    Arrow (args, effect_, result)
  | Variable _ ->
    (* unclear if we'll encounter this once polymorphism is added *)
    raise_s [%message "unexpected variable when expanding type"]
  | Metavariable meta ->
    (match Hashtbl.find t.in_progress_type_metavariables meta with
     | Some var ->
       (* recursive reference - leave unexpanded, it'll be bound in a Recursive type *)
       Variable var
     | None ->
       let var = fresh_type_variable t in
       Hashtbl.add_exn t.in_progress_type_metavariables ~key:meta ~data:var;
       let bounds =
         Constraints.get_type_bounds t.constraints meta
         |> Option.value_map
              ~f:(fun (bounds : _ Bounds.t) ->
                match polarity_positive with
                | true -> bounds.lowerBounds
                | false -> bounds.upperBounds)
              ~default:[]
       in
       let bound_types =
         List.map bounds ~f:(fun bound_type ->
           expand_type_aux t bound_type ~polarity_positive)
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

and expand_effect_aux t (effect_ : Effect.t) ~polarity_positive
  : Polar_type.Effect.t
  =
  match effect_ with
  | Labels labels -> Labels labels
  | Handled (labels, unknown) ->
    let effect_ = expand_unknown_effect_aux t unknown ~polarity_positive in
    Handled (labels, effect_)
  | Unknown unknown -> expand_unknown_effect_aux t unknown ~polarity_positive

and expand_unknown_effect_aux t (effect_ : Effect.Unknown.t) ~polarity_positive
  : Polar_type.Effect.t
  =
  match effect_ with
  | Variable _ ->
    raise_s [%message "unexpected effect variable when expanding type"]
  | Metavariable meta ->
    (* TODO: avoid duplication with the type case *)
    (match Hashtbl.find t.in_progress_effect_metavariables meta with
     | Some var ->
       (* recursive reference - leave unexpanded, it'll be bound in a Recursive type *)
       Variable var
     | None ->
       let var = fresh_effect_variable t in
       Hashtbl.add_exn t.in_progress_effect_metavariables ~key:meta ~data:var;
       let bounds =
         Constraints.get_effect_bounds t.constraints meta
         |> Option.value_map
              ~f:(fun (bounds : _ Bounds.t) ->
                match polarity_positive with
                | true -> bounds.lowerBounds
                | false -> bounds.upperBounds)
              ~default:[]
       in
       let bound_effects =
         List.map bounds ~f:(fun bound_effect ->
           expand_effect_aux t bound_effect ~polarity_positive)
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
           | _ :: _ -> combined)))
;;

let expand_type t type_ = expand_type_aux t type_ ~polarity_positive:true

let expand_effect t effect_ =
  expand_effect_aux t effect_ ~polarity_positive:true
;;
