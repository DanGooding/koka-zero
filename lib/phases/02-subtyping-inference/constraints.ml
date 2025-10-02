open! Core
open! Import

let root_debug_trace =
  lazy
    (Debug_trace.create_enabled_by_environment_variable
       "DEBUG_PRINT_CONSTRAINTS")
;;

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
  | Option element ->
    Option
      (extrude_aux t element ~to_level ~polarity_positive ~cache ~effect_cache)
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
       let fresh_bounds =
         match bounds with
         | None -> Bounds.create (module Type.Mono)
         | Some bounds ->
           (* TODO: this is inconsistent with the effect case!
              [ ] we don't add an extra metavariable 'fresh'
              [ ] we don't set one of upper/lower to empty
           *)
           (match polarity_positive with
            | true ->
              Bounds.freshen
                bounds
                ~upper:(`Map (fun bound -> bound))
                ~lower:
                  (`Map
                      (extrude_aux
                         t
                         ~to_level
                         ~polarity_positive
                         ~cache
                         ~effect_cache))
            | false ->
              Bounds.freshen
                bounds
                ~lower:(`Map (fun bound -> bound))
                ~upper:
                  (`Map
                      (extrude_aux
                         t
                         ~to_level
                         ~polarity_positive
                         ~cache
                         ~effect_cache)))
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
       let fresh_bounds =
         match bounds with
         | None ->
           (* TODO: surely it's wrong to do nothing here *)
           Bounds.create (module Effect)
         | Some bounds ->
           (match polarity_positive with
            | true ->
              Bounds.freshen
                bounds
                ~upper:`Empty
                ~lower:
                  (`Map
                      (extrude_effect_aux t ~to_level ~polarity_positive ~cache))
            | false ->
              Bounds.freshen
                bounds
                ~lower:`Empty
                ~upper:
                  (`Map
                      (extrude_effect_aux t ~to_level ~polarity_positive ~cache)))
       in
       (* TODO: in what cases do we run this add: *)
       add_fresh_effect_exn t fresh fresh_bounds;
       fresh)
;;

let extrude t type_ ~to_level ~polarity_positive ~debug_trace =
  let (_ : Debug_trace.t) =
    Debug_trace.enter debug_trace (fun () ->
      [%message
        "extrude type"
          ~type_:(Metavariables.sexp_of_type t.metavariables type_ : Sexp.t)
          (to_level : int)
          (polarity_positive : bool)])
  in
  extrude_aux
    t
    type_
    ~to_level
    ~polarity_positive
    ~cache:(Type.Metavariable.Table.create ())
    ~effect_cache:(Effect.Metavariable.Table.create ())
;;

let extrude_effect t effect_ ~to_level ~polarity_positive ~debug_trace =
  let (_ : Debug_trace.t) =
    Debug_trace.enter debug_trace (fun () ->
      [%message
        "extrude effect"
          ~effect_:
            (Metavariables.sexp_of_effect t.metavariables effect_ : Sexp.t)
          (to_level : int)
          (polarity_positive : bool)])
  in
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
          ~debug_trace
  : unit Or_static_error.t
  =
  let open Result.Let_syntax in
  let debug_trace =
    Debug_trace.enter debug_trace (fun () ->
      [%message
        "constrain_type_at_most"
          ~type_lo:(Metavariables.sexp_of_type t.metavariables type_lo : Sexp.t)
          ~type_hi:(Metavariables.sexp_of_type t.metavariables type_hi : Sexp.t)
          (location : Metavariables.Location.t)])
  in
  let constraint_ = Constraint.Type_at_most { type_lo; type_hi } in
  match Hash_set.strict_add t.already_seen_constraints constraint_ with
  | Error _already_present -> return ()
  | Ok () ->
    (match type_lo, type_hi with
     | ( Arrow (args_lo, effect_lo, result_lo)
       , Arrow (args_hi, effect_hi, result_hi) ) ->
       (match%bind
          Or_static_error.list_iter2 args_lo args_hi ~f:(fun arg_lo arg_hi ->
            constrain_type_at_most t arg_hi arg_lo ~location ~debug_trace)
        with
        | Ok () ->
          let%bind () =
            constrain_effect_at_most
              t
              effect_lo
              effect_hi
              ~location
              ~debug_trace
          in
          constrain_type_at_most t result_lo result_hi ~location ~debug_trace
        | Unequal_lengths ->
          Error
            (Static_error.type_error_s
               [%message
                 "function type has wrong number of arguments"
                   (args_lo : Type.Mono.t list)
                   (args_hi : Type.Mono.t list)]))
     | List elem_lo, List elem_hi ->
       constrain_type_at_most t elem_lo elem_hi ~location ~debug_trace
     | Option elem_lo, Option elem_hi ->
       constrain_type_at_most t elem_lo elem_hi ~location ~debug_trace
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
              constrain_type_at_most t elem_lo elem_hi ~location ~debug_trace))
     | Primitive p, Primitive p' when [%equal: Type.Primitive.t] p p' ->
       return ()
     | Primitive p, Primitive p' ->
       Error
         (Static_error.type_error_s
            [%message
              "inconsistent types"
                (p : Type.Primitive.t)
                (p' : Type.Primitive.t)])
     | Metavariable m, Metavariable n when [%equal: Type.Metavariable.t] m n ->
       return ()
     | Metavariable m, Metavariable n ->
       let%bind () =
         add_upper_bound t m (Type.Mono.Metavariable n) ~location ~debug_trace
       in
       add_lower_bound t (Type.Mono.Metavariable m) n ~location ~debug_trace
     | Metavariable m, type_hi ->
       add_upper_bound t m type_hi ~location ~debug_trace
     | type_lo, Metavariable m ->
       add_lower_bound t type_lo m ~location ~debug_trace
     | Arrow _, (Primitive _ | List _ | Tuple _ | Option _)
     | Primitive _, (Arrow _ | List _ | Tuple _ | Option _)
     | List _, (Arrow _ | Primitive _ | Tuple _ | Option _)
     | Tuple _, (Arrow _ | Primitive _ | List _ | Option _)
     | Option _, (Arrow _ | Primitive _ | List _ | Tuple _) ->
       Error
         (Static_error.type_error_s
            [%message
              "type error: cannot relate"
                (type_lo : Type.Mono.t)
                (type_hi : Type.Mono.t)]))

and add_upper_bound t m type_hi ~location ~debug_trace =
  let debug_trace =
    Debug_trace.enter debug_trace (fun () ->
      [%message
        "add_upper_bound"
          (m : Type.Metavariable.t)
          (type_hi : Type.Mono.t)
          (location : Metavariables.Location.t)])
  in
  let m_level = Metavariables.type_level_exn t.metavariables m in
  let hi_level =
    Type.Mono.max_level
      type_hi
      ~type_metavariable_level:(Metavariables.type_level_exn t.metavariables)
      ~effect_metavariable_level:
        (Metavariables.effect_level_exn t.metavariables)
  in
  match hi_level <= m_level with
  | true ->
    let (m_bounds : _ Bounds.t) =
      Hashtbl.find_or_add t.type_constraints m ~default:(fun () ->
        Bounds.create (module Type.Mono))
    in
    (* add [m <= type_hi]*)
    Bounds.add m_bounds `upper type_hi;
    (* add transitive closure *)
    Bounds.result_iter m_bounds `lower ~f:(fun below_m ->
      constrain_type_at_most t below_m type_hi ~location ~debug_trace)
  | false ->
    (* need to copy [type_hi] down to [m_level] *)
    let approx_type_hi =
      extrude t type_hi ~to_level:m_level ~polarity_positive:false ~debug_trace
    in
    (* m (level_m) <= approx_type_hi (level_m) <= type_hi *)
    add_upper_bound t m approx_type_hi ~location ~debug_trace

and add_lower_bound t type_lo m ~location ~debug_trace =
  let debug_trace =
    Debug_trace.enter debug_trace (fun () ->
      [%message
        "add_lower_bound"
          (type_lo : Type.Mono.t)
          (m : Type.Metavariable.t)
          (location : Metavariables.Location.t)])
  in
  let m_level = Metavariables.type_level_exn t.metavariables m in
  let lo_level =
    Type.Mono.max_level
      type_lo
      ~type_metavariable_level:(Metavariables.type_level_exn t.metavariables)
      ~effect_metavariable_level:
        (Metavariables.effect_level_exn t.metavariables)
  in
  match lo_level <= m_level with
  | true ->
    let (m_bounds : _ Bounds.t) =
      Hashtbl.find_or_add t.type_constraints m ~default:(fun () ->
        Bounds.create (module Type.Mono))
    in
    (* add [type_less_than_m <= m] *)
    Bounds.add m_bounds `lower type_lo;
    (* add transitive closure *)
    Bounds.result_iter m_bounds `upper ~f:(fun above_m ->
      constrain_type_at_most t type_lo above_m ~location ~debug_trace)
  | false ->
    let approx_type_lo =
      extrude t type_lo ~to_level:m_level ~polarity_positive:true ~debug_trace
    in
    add_lower_bound t approx_type_lo m ~location ~debug_trace

and constrain_effect_at_most
      t
      (effect_lo : Effect.t)
      (effect_hi : Effect.t)
      ~(location : Metavariables.Location.t)
      ~debug_trace
  : unit Or_static_error.t
  =
  let open Result.Let_syntax in
  let debug_trace =
    Debug_trace.enter debug_trace (fun () ->
      [%message
        "constrain_effect_at_most"
          ~effect_lo:
            (Metavariables.sexp_of_effect t.metavariables effect_lo : Sexp.t)
          ~effect_hi:
            (Metavariables.sexp_of_effect t.metavariables effect_hi : Sexp.t)
          (location : Metavariables.Location.t)])
  in
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
         ~debug_trace
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
            ~location
            ~debug_trace)
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
           ~debug_trace
         (* add back the info that [effect_lo] does not contain any of [labels_hi],
            except possibly those in [labels_lo]:
            effect_lo <= effect_lo - (labels_hi - labels_lo) *)
       in
       constrain_effect_at_most
         t
         (Metavariable effect_lo)
         (Handled (Set.diff labels_hi labels_lo, effect_lo))
         ~location
         ~debug_trace
     | Metavariable m, Metavariable n when [%equal: Effect.Metavariable.t] m n
       -> return ()
     | Metavariable m, Metavariable n ->
       let%bind () =
         add_effect_upper_bound
           t
           m
           (Effect.Metavariable n)
           ~location
           ~debug_trace
       in
       add_effect_lower_bound t (Effect.Metavariable m) n ~location ~debug_trace
     | Metavariable m, above_m ->
       add_effect_upper_bound t m above_m ~location ~debug_trace
     | below_m, Metavariable m ->
       add_effect_lower_bound t below_m m ~location ~debug_trace)

and add_effect_upper_bound t m above_m ~location ~debug_trace =
  let debug_trace =
    Debug_trace.enter debug_trace (fun () ->
      [%message
        "add_effect_upper_bound"
          (m : Effect.Metavariable.t)
          (above_m : Effect.t)
          (location : Metavariables.Location.t)])
  in
  let m_level = Metavariables.effect_level_exn t.metavariables m in
  let above_m_level =
    Effect.max_level
      above_m
      ~metavariable_level:(Metavariables.effect_level_exn t.metavariables)
  in
  match above_m_level <= m_level with
  | true ->
    let m_bounds =
      Hashtbl.find_or_add t.effect_constraints m ~default:(fun () ->
        Bounds.create (module Effect))
    in
    Bounds.add m_bounds `upper above_m;
    let debug_trace =
      Debug_trace.enter debug_trace (fun () ->
        [%message "hash_set_iter" ~addr:(Obj.magic m_bounds : int)])
    in
    Bounds.result_iter m_bounds `lower ~f:(fun below_m ->
      constrain_effect_at_most t below_m above_m ~location ~debug_trace)
  | false ->
    let above_approx =
      extrude_effect
        t
        above_m
        ~to_level:m_level
        ~polarity_positive:false
        ~debug_trace
    in
    add_effect_upper_bound t m above_approx ~location ~debug_trace

and add_effect_lower_bound t below_m m ~location ~debug_trace =
  let debug_trace =
    Debug_trace.enter debug_trace (fun () ->
      [%message
        "add_effect_lower_bound"
          (below_m : Effect.t)
          (m : Effect.Metavariable.t)
          (location : Metavariables.Location.t)])
  in
  let m_level = Metavariables.effect_level_exn t.metavariables m in
  let below_m_level =
    Effect.max_level
      below_m
      ~metavariable_level:(Metavariables.effect_level_exn t.metavariables)
  in
  match below_m_level <= m_level with
  | true ->
    let m_bounds =
      Hashtbl.find_or_add t.effect_constraints m ~default:(fun () ->
        Bounds.create (module Effect))
    in
    Bounds.add m_bounds `lower below_m;
    Bounds.result_iter m_bounds `upper ~f:(fun above_m ->
      constrain_effect_at_most t below_m above_m ~location ~debug_trace)
  | false ->
    let approx_below_m =
      extrude_effect
        t
        below_m
        ~to_level:m_level
        ~polarity_positive:true
        ~debug_trace
    in
    add_effect_lower_bound t approx_below_m m ~location ~debug_trace
;;

let constrain_type_at_most
      t
      (type_lo : Type.Mono.t)
      (type_hi : Type.Mono.t)
      ~location
  =
  constrain_type_at_most
    t
    type_lo
    type_hi
    ~location
    ~debug_trace:(Lazy.force root_debug_trace)
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
  constrain_effect_at_most
    t
    effect_lo
    effect_hi
    ~location
    ~debug_trace:(Lazy.force root_debug_trace)
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
    Bounds.iter bounds `lower ~f:(fun lower_bound ->
      let lower_bound_id = node_id lower_bound in
      add_tree_to_graph lower_bound graph;
      Dot_graph.add_edge graph ~from:lower_bound_id ~to_:meta_id ~disambiguator);
    Bounds.iter bounds `upper ~f:(fun upper_bound ->
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
