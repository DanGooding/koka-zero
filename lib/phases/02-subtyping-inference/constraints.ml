open! Core
open! Import

type t =
  { type_constraints : Type.Mono.t Bounds.t Type.Metavariable.Table.t
  ; effect_constraints : Effect.t Bounds.t Effect.Metavariable.Table.t
  ; already_seen_constraints : Constraint.Hash_set.t
  ; metavariables : Metavariables.t
  }
[@@deriving sexp_of]

let create ~metavariables =
  let type_constraints = Type.Metavariable.Table.create () in
  let effect_constraints = Effect.Metavariable.Table.create () in
  let already_seen_constraints = Constraint.Hash_set.create () in
  { type_constraints
  ; effect_constraints
  ; already_seen_constraints
  ; metavariables
  }
;;

let get_type_bounds t meta = Hashtbl.find t.type_constraints meta
let get_effect_bounds t meta = Hashtbl.find t.effect_constraints meta

let add_fresh_type_exn t meta bounds =
  Hashtbl.add_exn t.type_constraints ~key:meta ~data:bounds
;;

let add_fresh_effect_exn t meta bounds =
  Hashtbl.add_exn t.effect_constraints ~key:meta ~data:bounds
;;

let rec extrude_aux
          t
          (type_ : Type.Mono.t)
          ~to_level
          ~polarity_positive
          ~(cache : Type.Metavariable.t Bool.Table.t Type.Metavariable.Table.t)
  : Type.Mono.t
  =
  match type_ with
  | Variable v -> Variable v
  | Primitive p -> Primitive p
  | Arrow (args, effect_, result) ->
    let args =
      List.map
        args
        ~f:
          (extrude_aux
             t
             ~to_level
             ~polarity_positive:(not polarity_positive)
             ~cache)
    in
    let effect_ =
      extrude_effect_aux
        t
        effect_
        ~to_level
        ~polarity_positive
        ~cache:(Effect.Metavariable.Table.create ())
    in
    let result = extrude_aux t result ~to_level ~polarity_positive ~cache in
    Arrow (args, effect_, result)
  | Metavariable m
    when Metavariables.type_level_exn t.metavariables m > to_level ->
    (* need to lower m to [level] *)
    let polarity_cache =
      Hashtbl.find_or_add cache m ~default:Bool.Table.create
    in
    (match Hashtbl.find polarity_cache polarity_positive with
     | Some extruded -> Metavariable extruded
     | None ->
       let fresh =
         Metavariables.fresh_type_metavariable t.metavariables ~level:to_level
       in
       Hashtbl.add_exn polarity_cache ~key:polarity_positive ~data:fresh;
       Option.iter
         (get_type_bounds t m)
         ~f:(fun { Bounds.lowerBounds; upperBounds } ->
           let bounds =
             match polarity_positive with
             | true ->
               let lowerBounds =
                 List.map
                   lowerBounds
                   ~f:(extrude_aux t ~to_level ~polarity_positive ~cache)
               in
               { Bounds.lowerBounds; upperBounds }
             | false ->
               let upperBounds =
                 List.map
                   upperBounds
                   ~f:(extrude_aux t ~to_level ~polarity_positive ~cache)
               in
               { Bounds.lowerBounds; upperBounds }
           in
           add_fresh_type_exn t fresh bounds);
       Metavariable fresh)
  | Metavariable m -> Metavariable m

and extrude_effect_aux
      t
      (effect_ : Effect.t)
      ~to_level
      ~polarity_positive
      ~(cache : Effect.Metavariable.t Bool.Table.t Effect.Metavariable.Table.t)
  : Effect.t
  =
  match effect_ with
  | Labels labels -> Labels labels
  | Unknown unknown ->
    Unknown
      (extrude_unkown_effect_aux t unknown ~to_level ~polarity_positive ~cache)
  | Handled (labels, unknown) ->
    Handled
      ( labels
      , extrude_unkown_effect_aux t unknown ~to_level ~polarity_positive ~cache
      )

and extrude_unkown_effect_aux
      t
      (effect_ : Effect.Unknown.t)
      ~to_level
      ~polarity_positive
      ~cache
  =
  match effect_ with
  | Variable v -> Variable v
  | Metavariable m
    when Metavariables.effect_level_exn t.metavariables m > to_level ->
    let polarity_cache =
      Hashtbl.find_or_add cache m ~default:Bool.Table.create
    in
    (match Hashtbl.find polarity_cache polarity_positive with
     | Some fresh -> Metavariable fresh
     | None ->
       let fresh =
         Metavariables.fresh_effect_metavariable t.metavariables ~level:to_level
       in
       Hashtbl.add_exn polarity_cache ~key:polarity_positive ~data:fresh;
       Option.iter
         (get_effect_bounds t m)
         ~f:(fun { Bounds.lowerBounds; upperBounds } ->
           let bounds =
             match polarity_positive with
             | true ->
               let lowerBounds =
                 List.map
                   lowerBounds
                   ~f:(extrude_effect_aux t ~to_level ~polarity_positive ~cache)
               in
               { Bounds.lowerBounds; upperBounds }
             | false ->
               let upperBounds =
                 List.map
                   upperBounds
                   ~f:(extrude_effect_aux t ~to_level ~polarity_positive ~cache)
               in
               { Bounds.lowerBounds; upperBounds }
           in
           add_fresh_effect_exn t fresh bounds);
       Metavariable fresh)
  | Metavariable m -> Metavariable m
;;

let extrude t type_ ~to_level ~polarity_positive =
  extrude_aux
    t
    type_
    ~to_level
    ~polarity_positive
    ~cache:(Type.Metavariable.Table.create ())
;;

let extrude_effect t effect_ ~to_level ~polarity_positive =
  extrude_effect_aux
    t
    effect_
    ~to_level
    ~polarity_positive
    ~cache:(Effect.Metavariable.Table.create ())
;;

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
       let m_level = Metavariables.type_level_exn t.metavariables m in
       let hi_level =
         Type.Mono.max_level
           type_hi
           ~type_metavariable_level:
             (Metavariables.type_level_exn t.metavariables)
           ~effect_metavariable_level:
             (Metavariables.effect_level_exn t.metavariables)
       in
       (match hi_level <= m_level with
        | true ->
          let (m_bounds : _ Bounds.t) =
            Hashtbl.find_or_add t.type_constraints m ~default:Bounds.create
          in
          (* add [m <= type_hi]*)
          m_bounds.upperBounds <- type_hi :: m_bounds.upperBounds;
          (* add transitive closure *)
          List.iter m_bounds.lowerBounds ~f:(fun below_m ->
            constrain_type_at_most_exn t below_m type_hi)
        | false ->
          (* need to copy [type_hi] down to [m_level] *)
          let approx_type_hi =
            extrude t type_hi ~to_level:m_level ~polarity_positive:false
          in
          (* m (level_m) <= approx_type_hi (level_m) <= type_hi *)
          constrain_type_at_most_exn t approx_type_hi type_hi;
          constrain_type_at_most_exn t (Metavariable m) approx_type_hi)
     | type_lo, Metavariable m ->
       let m_level = Metavariables.type_level_exn t.metavariables m in
       let lo_level =
         Type.Mono.max_level
           type_lo
           ~type_metavariable_level:
             (Metavariables.type_level_exn t.metavariables)
           ~effect_metavariable_level:
             (Metavariables.effect_level_exn t.metavariables)
       in
       (match lo_level <= m_level with
        | true ->
          let (m_bounds : _ Bounds.t) =
            Hashtbl.find_or_add t.type_constraints m ~default:Bounds.create
          in
          (* add [type_less_than_m <= m] *)
          m_bounds.lowerBounds <- type_lo :: m_bounds.lowerBounds;
          (* add transitive closure *)
          List.iter m_bounds.upperBounds ~f:(fun above_m ->
            constrain_type_at_most_exn t type_lo above_m)
        | false ->
          let approx_type_lo =
            extrude t type_lo ~to_level:m_level ~polarity_positive:true
          in
          constrain_type_at_most_exn t type_lo approx_type_lo;
          constrain_type_at_most_exn t approx_type_lo (Metavariable m))
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
       let m_level = Metavariables.effect_level_exn t.metavariables m in
       let above_m_level =
         Effect.max_level
           above_m
           ~metavariable_level:(Metavariables.effect_level_exn t.metavariables)
       in
       (match above_m_level <= m_level with
        | true ->
          let m_bounds =
            Hashtbl.find_or_add t.effect_constraints m ~default:Bounds.create
          in
          m_bounds.upperBounds <- above_m :: m_bounds.upperBounds;
          List.iter m_bounds.lowerBounds ~f:(fun below_m ->
            constrain_effect_at_most_exn t below_m above_m)
        | false ->
          let above_approx =
            extrude_effect t above_m ~to_level:m_level ~polarity_positive:false
          in
          constrain_effect_at_most_exn t above_approx above_m;
          constrain_effect_at_most_exn t (Unknown (Metavariable m)) above_approx)
     | below_m, Unknown (Metavariable m) ->
       let m_level = Metavariables.effect_level_exn t.metavariables m in
       let below_m_level =
         Effect.max_level
           below_m
           ~metavariable_level:(Metavariables.effect_level_exn t.metavariables)
       in
       (match below_m_level <= m_level with
        | true ->
          let m_bounds =
            Hashtbl.find_or_add t.effect_constraints m ~default:Bounds.create
          in
          m_bounds.lowerBounds <- below_m :: m_bounds.lowerBounds;
          List.iter m_bounds.upperBounds ~f:(fun above_m ->
            constrain_effect_at_most_exn t below_m above_m)
        | false ->
          let approx_below_m =
            extrude_effect t below_m ~to_level:m_level ~polarity_positive:true
          in
          constrain_effect_at_most_exn t below_m approx_below_m;
          constrain_effect_at_most_exn
            t
            approx_below_m
            (Unknown (Metavariable m)))
     | Unknown (Variable v), _ | _, Unknown (Variable v) ->
       raise_s
         [%message
           "unexpected effect variable in constraint" (v : Effect.Variable.t)])
;;
