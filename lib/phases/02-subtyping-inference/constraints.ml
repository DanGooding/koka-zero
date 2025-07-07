open! Core
open! Import

type t =
  { type_constraints : Type.Mono.t Bounds.t Type.Metavariable.Table.t
  ; effect_constraints : Effect.t Bounds.t Effect.Metavariable.Table.t
  ; already_seen_constraints : Constraint.Hash_set.t
  }
[@@deriving sexp_of]

let create () =
  let type_constraints = Type.Metavariable.Table.create () in
  let effect_constraints = Effect.Metavariable.Table.create () in
  let already_seen_constraints = Constraint.Hash_set.create () in
  { type_constraints; effect_constraints; already_seen_constraints }
;;

let get_type_bounds t meta = Hashtbl.find t.type_constraints meta
let get_effect_bounds t meta = Hashtbl.find t.effect_constraints meta

(** add constraints for [type_lo <= type_hi]
    fails if we find something unsatisfiable - i.e. a type error *)
let rec constrain_type_at_most_exn
          t
          (type_lo : Type.Mono.t)
          (type_hi : Type.Mono.t)
  =
  let constraint_ = Constraint.Type_at_most { type_lo; type_hi } in
  match Hash_set.strict_add t.already_seen_constraints constraint_ with
  | Error _already_present -> ()
  | Ok () ->
    (match type_lo, type_hi with
     | ( Arrow (args_lo, effect_lo, result_lo)
       , Arrow (args_hi, effect_hi, result_hi) ) ->
       List.iter2_exn args_lo args_hi ~f:(fun arg_lo arg_hi ->
         constrain_type_at_most_exn t arg_hi arg_lo);
       constrain_effect_at_most_exn t effect_lo effect_hi;
       constrain_type_at_most_exn t result_lo result_hi
     | Primitive p, Primitive p' when [%equal: Type.Primitive.t] p p' -> ()
     | Primitive p, Primitive p' ->
       raise_s
         [%message
           "inconsistent types" (p : Type.Primitive.t) (p' : Type.Primitive.t)]
     | Metavariable m, type_hi ->
       let (m_bounds : _ Bounds.t) =
         Hashtbl.find_or_add t.type_constraints m ~default:Bounds.create
       in
       (* add [m <= type_hi]*)
       m_bounds.upperBounds <- type_hi :: m_bounds.upperBounds;
       (* add transitive closure *)
       List.iter m_bounds.lowerBounds ~f:(fun below_m ->
         constrain_type_at_most_exn t below_m type_hi)
     | type_lo, Metavariable m ->
       let (m_bounds : _ Bounds.t) =
         Hashtbl.find_or_add t.type_constraints m ~default:Bounds.create
       in
       (* add [type_less_than_m <= m] *)
       m_bounds.lowerBounds <- type_lo :: m_bounds.lowerBounds;
       (* add transitive closure *)
       List.iter m_bounds.upperBounds ~f:(fun above_m ->
         constrain_type_at_most_exn t type_lo above_m)
     | Variable v, _ | _, Variable v ->
       raise_s [%message "unexpected type variable" (v : Type.Variable.t)]
     | Arrow _, Primitive _ | Primitive _, Arrow _ ->
       raise_s
         [%message
           "type error: cannot relate"
             (type_lo : Type.Mono.t)
             (type_hi : Type.Mono.t)])

and constrain_effect_at_most_exn t (effect_lo : Effect.t) (effect_hi : Effect.t)
  =
  let constraint_ = Constraint.Effect_at_most { effect_lo; effect_hi } in
  match Hash_set.strict_add t.already_seen_constraints constraint_ with
  | Error _already_present -> ()
  | Ok () ->
    (match effect_lo, effect_hi with
     | Labels labels_lo, Labels labels_hi
       when Set.is_subset labels_lo ~of_:labels_hi -> ()
     | Labels labels_lo, Labels labels_hi ->
       raise_s
         [%message
           "less-than constraint doesn't hold"
             ~labels:(labels_lo : Effect.Label.Set.t)
             ~expected_at_most:(labels_hi : Effect.Label.Set.t)]
     | Handled (labels_lo, effect_lo), Labels labels_hi ->
       (* effect_lo - labels_lo <= labels_hi *)
       constrain_effect_at_most_exn
         t
         (Unknown effect_lo)
         (Labels (Set.union labels_lo labels_hi))
     | Labels labels_lo, Handled (labels_hi, effect_hi) ->
       (* labels_lo <= effect_hi - labels_hi *)
       (match Set.are_disjoint labels_lo labels_hi with
        | false ->
          (* we're requiring that a row containing L is <= a row not containing L
             for L being the intersection. This can never hold. *)
          raise_s
            [%message
              "less-than constraint doesn't hold"
                ~labels:(labels_lo : Effect.Label.Set.t)
                ~expected_not_to_contain:(labels_hi : Effect.Label.Set.t)]
        | true ->
          (* it's optional whether effect_hi contains labels' or not.
             so the constraint reduces to labels_lo <= effect_hi *)
          constrain_effect_at_most_exn t (Labels labels_lo) (Unknown effect_hi))
     | Handled (labels_lo, effect_lo), Handled (labels_hi, effect_hi) ->
       (* effect_lo - labels_lo <= effect_hi - labels_hi
        we reduce into multiple simpler but equiavalent constraints to make progress *)
       (* weaken - increase the upper bound, getting:
         effect_lo - labels_lo <= effect_hi *)
       constrain_effect_at_most_exn
         t
         (Handled (labels_lo, effect_lo))
         (Unknown effect_hi);
       (* add back the info that [effect_lo] does not contain any of [labels_hi], 
         except possibly those in [labels_lo]:
         effect_lo <= effect_lo - (labels_hi - labels_lo) *)
       (* TODO: can we actually make use of this constraint? 
         perhaps instead, for each upper bound effect_lo <= above
         add: effect_lo <= Handled(diff, above)
         (we'll want to add this for subsequently added bounds too)
      *)
       constrain_effect_at_most_exn
         t
         (Unknown effect_lo)
         (Handled (Set.diff labels_hi labels_lo, effect_lo))
     | Unknown (Metavariable m), above_m ->
       let m_bounds =
         Hashtbl.find_or_add t.effect_constraints m ~default:Bounds.create
       in
       m_bounds.upperBounds <- above_m :: m_bounds.upperBounds;
       List.iter m_bounds.lowerBounds ~f:(fun below_m ->
         constrain_effect_at_most_exn t below_m above_m)
     | below_m, Unknown (Metavariable m) ->
       let m_bounds =
         Hashtbl.find_or_add t.effect_constraints m ~default:Bounds.create
       in
       m_bounds.lowerBounds <- below_m :: m_bounds.lowerBounds;
       List.iter m_bounds.upperBounds ~f:(fun above_m ->
         constrain_effect_at_most_exn t below_m above_m)
     | Unknown (Variable v), _ | _, Unknown (Variable v) ->
       raise_s
         [%message
           "unexpected effect variable in constraint" (v : Effect.Variable.t)])
;;

let add_fresh_type_exn t meta bounds =
  Hashtbl.add_exn t.type_constraints ~key:meta ~data:bounds
;;

let add_fresh_effect_exn t meta bounds =
  Hashtbl.add_exn t.effect_constraints ~key:meta ~data:bounds
;;
