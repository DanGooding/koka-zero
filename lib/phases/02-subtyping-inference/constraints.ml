open! Core
open! Import

let debug () = Sys.getenv "DEBUG_PRINT_CONSTRAINTS" |> Option.is_some

type t =
  { type_constraints : Type.Mono.t Bounds.t Type.Metavariable.Table.t
  ; effect_constraints : Effect.t Bounds.t Effect.Metavariable.Table.t
  ; already_seen_constraints : (Constraint.Hash_set.t[@sexp.opaque])
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
          ~(effect_cache :
             Effect.Metavariable.t Bool.Table.t Effect.Metavariable.Table.t)
  : Type.Mono.t
  =
  match type_ with
  | Primitive p -> Primitive p
  | List element ->
    let element =
      extrude_aux t element ~to_level ~polarity_positive ~cache ~effect_cache
    in
    List element
  | Tuple elements ->
    Tuple
      (List.map
         elements
         ~f:(extrude_aux t ~to_level ~polarity_positive ~cache ~effect_cache))
  | Arrow (args, effect_, result) ->
    let args =
      List.map
        args
        ~f:
          (extrude_aux
             t
             ~to_level
             ~polarity_positive:(not polarity_positive)
             ~cache
             ~effect_cache)
    in
    let effect_ =
      extrude_effect_aux
        t
        effect_
        ~to_level
        ~polarity_positive
        ~cache:effect_cache
    in
    let result =
      extrude_aux t result ~to_level ~polarity_positive ~cache ~effect_cache
    in
    Arrow (args, effect_, result)
  | Metavariable m
    when Metavariables.type_level_exn t.metavariables m <= to_level ->
    Metavariable m
  | Metavariable m ->
    (* need to lower m to [level] *)
    let polarity_cache =
      Hashtbl.find_or_add cache m ~default:Bool.Table.create
    in
    (match Hashtbl.find polarity_cache polarity_positive with
     | Some extruded -> Metavariable extruded
     | None ->
       let fresh =
         Metavariables.fresh_type_metavariable
           t.metavariables
           ~level:to_level
           ~location:
             (Instantiation (Metavariables.type_location_exn t.metavariables m))
       in
       Hashtbl.add_exn polarity_cache ~key:polarity_positive ~data:fresh;
       let bounds = get_type_bounds t m in
       if debug ()
       then (
         let sexp_of_type_ = Metavariables.sexp_of_type t.metavariables in
         print_s
           [%message
             "extruding"
               ~meta:(Metavariable m : type_)
               (bounds : type_ Bounds.t option)]);
       let fresh_bounds =
         match bounds with
         | None -> Bounds.create ()
         | Some { Bounds.lower_bounds; upper_bounds } ->
           (match polarity_positive with
            | true ->
              let lower_bounds =
                List.map
                  lower_bounds
                  ~f:
                    (extrude_aux
                       t
                       ~to_level
                       ~polarity_positive
                       ~cache
                       ~effect_cache)
              in
              { Bounds.lower_bounds; upper_bounds }
            | false ->
              let upper_bounds =
                List.map
                  upper_bounds
                  ~f:
                    (extrude_aux
                       t
                       ~to_level
                       ~polarity_positive
                       ~cache
                       ~effect_cache)
              in
              { Bounds.lower_bounds; upper_bounds })
       in
       add_fresh_type_exn t fresh fresh_bounds;
       Metavariable fresh)

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
  | Metavariable meta ->
    Metavariable
      (extrude_effect_metavariable_aux
         t
         meta
         ~to_level
         ~polarity_positive
         ~cache)
  | Handled (labels, meta) ->
    Handled
      ( labels
      , extrude_effect_metavariable_aux
          t
          meta
          ~to_level
          ~polarity_positive
          ~cache )

and extrude_effect_metavariable_aux
      t
      (m : Effect.Metavariable.t)
      ~to_level
      ~polarity_positive
      ~cache
  : Effect.Metavariable.t
  =
  match Metavariables.effect_level_exn t.metavariables m > to_level with
  | false -> m
  | true ->
    let polarity_cache =
      Hashtbl.find_or_add cache m ~default:Bool.Table.create
    in
    (match Hashtbl.find polarity_cache polarity_positive with
     | Some fresh -> fresh
     | None ->
       let fresh =
         Metavariables.fresh_effect_metavariable
           t.metavariables
           ~level:to_level
           ~location:
             (Instantiation
                (Metavariables.effect_location_exn t.metavariables m))
       in
       Hashtbl.add_exn polarity_cache ~key:polarity_positive ~data:fresh;
       let bounds = get_effect_bounds t m in
       if debug ()
       then (
         let sexp_of_effect_ = Metavariables.sexp_of_effect t.metavariables in
         print_s
           [%message
             "extruding"
               ~meta:(Metavariable m : effect_)
               (bounds : effect_ Bounds.t option)]);
       let fresh_bounds =
         match bounds with
         | None -> Bounds.create ()
         | Some bounds ->
           (match polarity_positive with
            | true ->
              let lower_bounds =
                List.map
                  bounds.lower_bounds
                  ~f:(extrude_effect_aux t ~to_level ~polarity_positive ~cache)
              in
              bounds.upper_bounds <- Metavariable fresh :: bounds.upper_bounds;
              { Bounds.lower_bounds; upper_bounds = [] }
            | false ->
              let upper_bounds =
                List.map
                  bounds.upper_bounds
                  ~f:(extrude_effect_aux t ~to_level ~polarity_positive ~cache)
              in
              bounds.lower_bounds <- Metavariable fresh :: bounds.upper_bounds;
              { Bounds.lower_bounds = []; upper_bounds })
       in
       add_fresh_effect_exn t fresh fresh_bounds;
       fresh)
;;

let extrude t type_ ~to_level ~polarity_positive =
  if debug ()
  then
    print_s
      [%message
        "extrude type"
          ~type_:(Metavariables.sexp_of_type t.metavariables type_ : Sexp.t)
          (to_level : int)
          (polarity_positive : bool)];
  extrude_aux
    t
    type_
    ~to_level
    ~polarity_positive
    ~cache:(Type.Metavariable.Table.create ())
    ~effect_cache:(Effect.Metavariable.Table.create ())
;;

let extrude_effect t effect_ ~to_level ~polarity_positive =
  if debug ()
  then
    print_s
      [%message
        "extrude effect"
          ~effect_:
            (Metavariables.sexp_of_effect t.metavariables effect_ : Sexp.t)
          (to_level : int)
          (polarity_positive : bool)];
  extrude_effect_aux
    t
    effect_
    ~to_level
    ~polarity_positive
    ~cache:(Effect.Metavariable.Table.create ())
;;

(** add constraints for [type_lo <= type_hi]
    fails if we find something unsatisfiable - i.e. a type error *)
let rec constrain_type_at_most
          t
          (type_lo : Type.Mono.t)
          (type_hi : Type.Mono.t)
          ~(location : Metavariables.Location.t)
  : unit Or_static_error.t
  =
  let open Result.Let_syntax in
  if debug ()
  then
    print_s
      [%message
        "constrain_type_at_most"
          ~type_lo:(Metavariables.sexp_of_type t.metavariables type_lo : Sexp.t)
          ~type_hi:(Metavariables.sexp_of_type t.metavariables type_hi : Sexp.t)
          (location : Metavariables.Location.t)];
  let constraint_ = Constraint.Type_at_most { type_lo; type_hi } in
  match Hash_set.strict_add t.already_seen_constraints constraint_ with
  | Error _already_present -> return ()
  | Ok () ->
    (match type_lo, type_hi with
     | ( Arrow (args_lo, effect_lo, result_lo)
       , Arrow (args_hi, effect_hi, result_hi) ) ->
       (match%bind
          Or_static_error.list_iter2 args_lo args_hi ~f:(fun arg_lo arg_hi ->
            constrain_type_at_most t arg_hi arg_lo ~location)
        with
        | Ok () ->
          let%bind () =
            constrain_effect_at_most t effect_lo effect_hi ~location
          in
          constrain_type_at_most t result_lo result_hi ~location
        | Unequal_lengths ->
          Error
            (Static_error.type_error_s
               [%message
                 "function type has wrong number of arguments"
                   (args_lo : Type.Mono.t list)
                   (args_hi : Type.Mono.t list)]))
     | List elem_lo, List elem_hi ->
       constrain_type_at_most t elem_lo elem_hi ~location
     | Tuple elements_lo, Tuple elements_hi ->
       (match List.zip elements_lo elements_hi with
        | Unequal_lengths ->
          Error
            (Static_error.type_error_s
               [%message
                 "tuple has wrong number of arguments"
                   ~n:(List.length elements_lo : int)
                   ~m:(List.length elements_hi : int)])
        | Ok paired_elements ->
          Or_static_error.list_iter
            paired_elements
            ~f:(fun (elem_lo, elem_hi) ->
              constrain_type_at_most t elem_lo elem_hi ~location))
     | Primitive p, Primitive p' when [%equal: Type.Primitive.t] p p' ->
       return ()
     | Primitive p, Primitive p' ->
       Error
         (Static_error.type_error_s
            [%message
              "inconsistent types"
                (p : Type.Primitive.t)
                (p' : Type.Primitive.t)])
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
          m_bounds.upper_bounds <- type_hi :: m_bounds.upper_bounds;
          (* add transitive closure *)
          Or_static_error.list_iter m_bounds.lower_bounds ~f:(fun below_m ->
            constrain_type_at_most t below_m type_hi ~location)
        | false ->
          (* need to copy [type_hi] down to [m_level] *)
          let approx_type_hi =
            extrude t type_hi ~to_level:m_level ~polarity_positive:false
          in
          (* m (level_m) <= approx_type_hi (level_m) <= type_hi *)
          constrain_type_at_most t (Metavariable m) approx_type_hi ~location)
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
          m_bounds.lower_bounds <- type_lo :: m_bounds.lower_bounds;
          (* add transitive closure *)
          Or_static_error.list_iter m_bounds.upper_bounds ~f:(fun above_m ->
            constrain_type_at_most t type_lo above_m ~location)
        | false ->
          let approx_type_lo =
            extrude t type_lo ~to_level:m_level ~polarity_positive:true
          in
          constrain_type_at_most t approx_type_lo (Metavariable m) ~location)
     | Arrow _, (Primitive _ | List _ | Tuple _)
     | Primitive _, (Arrow _ | List _ | Tuple _)
     | List _, (Arrow _ | Primitive _ | Tuple _)
     | Tuple _, (Arrow _ | Primitive _ | List _) ->
       Error
         (Static_error.type_error_s
            [%message
              "type error: cannot relate"
                (type_lo : Type.Mono.t)
                (type_hi : Type.Mono.t)]))

and constrain_effect_at_most
      t
      (effect_lo : Effect.t)
      (effect_hi : Effect.t)
      ~(location : Metavariables.Location.t)
  : unit Or_static_error.t
  =
  let open Result.Let_syntax in
  if debug ()
  then
    print_s
      [%message
        "constrain_effect_at_most"
          ~effect_lo:
            (Metavariables.sexp_of_effect t.metavariables effect_lo : Sexp.t)
          ~effect_hi:
            (Metavariables.sexp_of_effect t.metavariables effect_hi : Sexp.t)
          (location : Metavariables.Location.t)];
  let constraint_ = Constraint.Effect_at_most { effect_lo; effect_hi } in
  match Hash_set.strict_add t.already_seen_constraints constraint_ with
  | Error _already_present -> return ()
  | Ok () ->
    (match effect_lo, effect_hi with
     | Labels labels_lo, Labels labels_hi
       when Set.is_subset labels_lo ~of_:labels_hi -> return ()
     | Labels labels_lo, Labels labels_hi ->
       Error
         (Static_error.type_error_s
            [%message
              "constraint doesn't hold"
                ~labels:(labels_lo : Effect.Label.Set.t)
                ~expected_at_most:(labels_hi : Effect.Label.Set.t)])
     | Handled (labels_lo, effect_lo), Labels labels_hi ->
       (* effect_lo - labels_lo <= labels_hi *)
       constrain_effect_at_most
         t
         (Metavariable effect_lo)
         (Labels (Set.union labels_lo labels_hi))
         ~location
     | Labels labels_lo, Handled (labels_hi, effect_hi) ->
       (* labels_lo <= effect_hi - labels_hi *)
       (match Set.are_disjoint labels_lo labels_hi with
        | false ->
          (* we're requiring that a row containing L is <= a row not containing L
             for L being the intersection. This can never hold. *)
          Error
            (Static_error.type_error_s
               [%message
                 "less-than constraint doesn't hold"
                   ~labels:(labels_lo : Effect.Label.Set.t)
                   ~expected_not_to_contain:(labels_hi : Effect.Label.Set.t)])
        | true ->
          (* it's optional whether effect_hi contains labels' or not.
             so the constraint reduces to labels_lo <= effect_hi *)
          constrain_effect_at_most
            t
            (Labels labels_lo)
            (Metavariable effect_hi)
            ~location)
     | Handled (labels_lo, effect_lo), Handled (labels_hi, effect_hi) ->
       (* effect_lo - labels_lo <= effect_hi - labels_hi
        we reduce into multiple simpler but equiavalent constraints to make progress *)
       (* weaken - increase the upper bound, getting:
         effect_lo - labels_lo <= effect_hi *)
       let%bind () =
         constrain_effect_at_most
           t
           (Handled (labels_lo, effect_lo))
           (Metavariable effect_hi)
           ~location
         (* add back the info that [effect_lo] does not contain any of [labels_hi],
            except possibly those in [labels_lo]:
            effect_lo <= effect_lo - (labels_hi - labels_lo) *)
       in
       constrain_effect_at_most
         t
         (Metavariable effect_lo)
         (Handled (Set.diff labels_hi labels_lo, effect_lo))
         ~location
     | Metavariable m, above_m ->
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
          m_bounds.upper_bounds <- above_m :: m_bounds.upper_bounds;
          Or_static_error.list_iter m_bounds.lower_bounds ~f:(fun below_m ->
            constrain_effect_at_most t below_m above_m ~location)
        | false ->
          let above_approx =
            extrude_effect t above_m ~to_level:m_level ~polarity_positive:false
          in
          constrain_effect_at_most t (Metavariable m) above_approx ~location)
     | below_m, Metavariable m ->
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
          m_bounds.lower_bounds <- below_m :: m_bounds.lower_bounds;
          Or_static_error.list_iter m_bounds.upper_bounds ~f:(fun above_m ->
            constrain_effect_at_most t below_m above_m ~location)
        | false ->
          let approx_below_m =
            extrude_effect t below_m ~to_level:m_level ~polarity_positive:true
          in
          constrain_effect_at_most t approx_below_m (Metavariable m) ~location))
;;

let constrain_type_at_most
      t
      (type_lo : Type.Mono.t)
      (type_hi : Type.Mono.t)
      ~location
  =
  constrain_type_at_most t type_lo type_hi ~location
  |> Result.map_error
       ~f:
         (Static_error.tag_s
            ~tag:
              (let constraint_ =
                 [%sexp { type_lo : Type.Mono.t; type_hi : Type.Mono.t }]
               in
               [%message
                 "error when expanding constraint"
                   ~_:(constraint_ : Sexp.t)
                   (location : Metavariables.Location.t)]))
;;

let constrain_effect_at_most
      t
      (effect_lo : Effect.t)
      (effect_hi : Effect.t)
      ~location
  =
  constrain_effect_at_most t effect_lo effect_hi ~location
  |> Result.map_error
       ~f:
         (Static_error.tag_s
            ~tag:
              (let constraint_ =
                 [%sexp { effect_lo : Effect.t; effect_hi : Effect.t }]
               in
               [%message
                 "error when expanding constraint"
                   ~_:(constraint_ : Sexp.t)
                   (location : Metavariables.Location.t)]))
;;

let to_graph t : Dot_graph.t =
  let graph = Dot_graph.create () in
  let add_constraints meta (bounds : _ Bounds.t) ~node_id ~add_tree_to_graph =
    let meta_id = node_id meta in
    let disambiguator = Dot_graph.Edge_disambiguator.of_string "at-most" in
    add_tree_to_graph meta graph;
    List.iter bounds.lower_bounds ~f:(fun lower_bound ->
      let lower_bound_id = node_id lower_bound in
      add_tree_to_graph lower_bound graph;
      Dot_graph.add_edge graph ~from:lower_bound_id ~to_:meta_id ~disambiguator);
    List.iter bounds.upper_bounds ~f:(fun upper_bound ->
      let upper_bound_id = node_id upper_bound in
      add_tree_to_graph upper_bound graph;
      Dot_graph.add_edge graph ~from:meta_id ~to_:upper_bound_id ~disambiguator)
  in
  Hashtbl.iteri
    t.type_constraints
    ~f:(fun ~key:meta ~data:(bounds : _ Bounds.t) ->
      add_constraints
        (Type.Mono.Metavariable meta)
        bounds
        ~node_id:Type.Mono.node_id
        ~add_tree_to_graph:Type.Mono.add_tree_to_graph);
  Hashtbl.iteri
    t.effect_constraints
    ~f:(fun ~key:meta ~data:(bounds : _ Bounds.t) ->
      add_constraints
        (Effect.Metavariable meta)
        bounds
        ~node_id:Effect.node_id
        ~add_tree_to_graph:Effect.add_tree_to_graph);
  graph
;;

let print_as_graph t =
  let graph = to_graph t in
  Dot_graph.write graph Out_channel.stdout
;;
