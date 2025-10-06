open! Core
open! Import

let flip = function
  | `all -> `all
  | `positive -> `negative
  | `negative -> `positive
;;

module Effect = struct
  module Variable = Effect.Variable
  module Label = Effect.Label

  type t =
    | Variable of Effect.Variable.t
    | Labels of Effect.Label.Set.t
    | Handled of Effect.Label.Set.t * t
    | Union of t list
    | Intersection of t list
    | Recursive of Effect.Variable.t * t
  [@@deriving sexp_of, compare]

  let rec variables t ~polarity =
    match t with
    | Variable v ->
      (match polarity with
       | `all | `positive -> Effect.Variable.Set.singleton v
       | `negative -> Effect.Variable.Set.empty)
    | Labels _labels -> Effect.Variable.Set.empty
    | Handled (_lables, effect_) -> variables effect_ ~polarity
    | Union effects | Intersection effects ->
      Effect.Variable.Set.union_list (List.map effects ~f:(variables ~polarity))
    | Recursive (v, effect_) -> Set.remove (variables effect_ ~polarity) v
  ;;

  let rec map_variables t ~f =
    match t with
    | Variable v -> f v
    | Labels labels -> Labels labels
    | Handled (labels, effect_) -> Handled (labels, map_variables effect_ ~f)
    | Union effects -> Union (List.map effects ~f:(map_variables ~f))
    | Intersection effects ->
      Intersection (List.map effects ~f:(map_variables ~f))
    | Recursive (v, t) -> Recursive (v, map_variables t ~f)
  ;;

  let rec simplify t =
    match t with
    | Intersection ts ->
      (match List.concat_map ts ~f:simplify_within_intersection with
       | [ t ] -> t
       | ts -> Intersection ts)
    | Union ts ->
      (match List.concat_map ts ~f:simplify_within_union with
       | [ t ] -> t
       | ts -> Union ts)
    | Variable v -> Variable v
    | Labels labels -> Labels labels
    | Handled (handled_labels, t) ->
      (match (simplify t : t) with
       | Labels labels ->
         let labels = Set.diff labels handled_labels in
         Labels labels
       | Handled (more_handled_labels, t) ->
         let handled_labels = Set.union handled_labels more_handled_labels in
         Handled (handled_labels, t)
       | Intersection _ | Union _ | Variable _ | Recursive _ ->
         Handled (handled_labels, t))
    | Recursive (v, t) -> Recursive (v, simplify t)

  and simplify_within_union t =
    match simplify t with
    | Union ts -> ts
    | (Intersection _ | Variable _ | Labels _ | Handled _ | Recursive _) as t'
      -> [ t' ]

  and simplify_within_intersection t =
    match simplify t with
    | Intersection ts -> ts
    | (Union _ | Variable _ | Labels _ | Handled _ | Recursive _) as t' ->
      [ t' ]
  ;;

  let is_total t =
    match simplify t with
    | Labels labels when Set.is_empty labels -> true
    | Labels _ | Intersection _ | Union _ | Variable _ | Handled _ | Recursive _
      -> false
  ;;
end

type t =
  | Variable of Type.Variable.t
  | Primitive of Type.Primitive.t
  | Arrow of t list * Effect.t * t
  | List of t
  | Tuple of t list
  | Option of t
  | Union of t list
  | Intersection of t list
  | Recursive of Type.Variable.t * t
[@@deriving sexp_of, compare]

let rec variables t ~polarity =
  match t with
  | Variable v ->
    (match polarity with
     | `all | `positive -> Type.Variable.Set.singleton v
     | `negative -> Type.Variable.Set.empty)
  | Primitive _ -> Type.Variable.Set.empty
  | List t | Option t -> variables t ~polarity
  | Tuple ts ->
    Type.Variable.Set.union_list (List.map ts ~f:(variables ~polarity))
  | Arrow (args, _effect, result) ->
    Type.Variable.Set.union_list
      (variables result ~polarity
       :: List.map args ~f:(variables ~polarity:(flip polarity)))
  | Union types | Intersection types ->
    Type.Variable.Set.union_list (List.map types ~f:(variables ~polarity))
  | Recursive (v, type_) -> Set.remove (variables type_ ~polarity) v
;;

let rec effect_variables t ~polarity =
  match t with
  | Variable _ -> Effect.Variable.Set.empty
  | Primitive _ -> Effect.Variable.Set.empty
  | List t | Option t -> effect_variables t ~polarity
  | Tuple ts ->
    Effect.Variable.Set.union_list (List.map ts ~f:(effect_variables ~polarity))
  | Arrow (args, effect_, result) ->
    Effect.Variable.Set.union_list
      (effect_variables result ~polarity
       :: Effect.variables effect_ ~polarity
       :: List.map args ~f:(effect_variables ~polarity:(flip polarity)))
  | Union types | Intersection types ->
    Effect.Variable.Set.union_list
      (List.map types ~f:(effect_variables ~polarity))
  | Recursive (_, type_) -> effect_variables type_ ~polarity
;;

let rec map_variables t ~f =
  match t with
  | Variable v -> f v
  | Primitive p -> Primitive p
  | Arrow (args, effect_, result) ->
    Arrow (List.map args ~f:(map_variables ~f), effect_, map_variables result ~f)
  | List t -> List (map_variables t ~f)
  | Option t -> Option (map_variables t ~f)
  | Tuple ts -> Tuple (List.map ts ~f:(map_variables ~f))
  | Union ts -> Union (List.map ts ~f:(map_variables ~f))
  | Intersection ts -> Intersection (List.map ts ~f:(map_variables ~f))
  | Recursive (v, t) -> Recursive (v, map_variables t ~f)
;;

(* Remove variables occurring only positively or only negatively.
   These add no information since they can always be set to bottom/top. *)
let remove_unipolar_type_variables t =
  let positive = variables t ~polarity:`positive in
  let negative = variables t ~polarity:`negative in
  map_variables t ~f:(fun v ->
    match Set.mem positive v, Set.mem negative v with
    | true, true | false, false -> Variable v
    | true, false -> Union []
    | false, true -> Intersection [])
;;

let rec map_effect t ~f =
  match t with
  | Variable v -> Variable v
  | Primitive p -> Primitive p
  | Arrow (args, effect_, result) ->
    Arrow (List.map args ~f:(map_effect ~f), f effect_, map_effect result ~f)
  | List t -> List (map_effect t ~f)
  | Option t -> Option (map_effect t ~f)
  | Tuple ts -> Tuple (List.map ts ~f:(map_effect ~f))
  | Union ts -> Union (List.map ts ~f:(map_effect ~f))
  | Intersection ts -> Intersection (List.map ts ~f:(map_effect ~f))
  | Recursive (v, t) -> Recursive (v, map_effect t ~f)
;;

let remove_unipolar_effect_variables t =
  let positive = effect_variables t ~polarity:`positive in
  let negative = effect_variables t ~polarity:`negative in
  map_effect
    t
    ~f:
      (Effect.map_variables ~f:(fun v ->
         match Set.mem positive v, Set.mem negative v with
         | true, true | false, false -> Variable v
         | true, false -> Union []
         | false, true -> Intersection []))
;;

let remove_unipolar_variables t =
  remove_unipolar_type_variables t |> remove_unipolar_effect_variables
;;

(* this could be much more sophisticated,
   notably there are many cases where we can remove variables *)
let rec simplify t =
  match t with
  | Intersection ts ->
    (match List.concat_map ts ~f:simplify_within_intersection with
     | [ t ] -> t
     | ts -> Intersection ts)
  | Union ts ->
    (match List.concat_map ts ~f:simplify_within_union with
     | [ t ] -> t
     | ts -> Union ts)
  | Variable _ | Primitive _ -> t
  | List t -> List (simplify t)
  | Option t -> Option (simplify t)
  | Tuple ts -> Tuple (List.map ts ~f:simplify)
  | Arrow (args, effect_, result) ->
    Arrow (List.map args ~f:simplify, Effect.simplify effect_, simplify result)
  | Recursive (v, t) -> Recursive (v, simplify t)

and simplify_within_union t =
  match simplify t with
  | Union ts -> ts
  | ( Intersection _
    | Variable _
    | Primitive _
    | List _
    | Option _
    | Tuple _
    | Arrow _
    | Recursive _ ) as t' -> [ t' ]

and simplify_within_intersection t =
  match simplify t with
  | Intersection ts -> ts
  | ( Union _
    | Variable _
    | Primitive _
    | List _
    | Option _
    | Tuple _
    | Arrow _
    | Recursive _ ) as t' -> [ t' ]
;;

let simplify t = remove_unipolar_variables t |> simplify

let%expect_test "remove unipolar variables" =
  let type_source = Type.Variable.Name_source.fresh () ~prefix:"t" in
  let a = Type.Variable.Name_source.next_name type_source in
  let b = Type.Variable.Name_source.next_name type_source in
  let c = Type.Variable.Name_source.next_name type_source in
  let d = Type.Variable.Name_source.next_name type_source in
  let effect_source = Effect.Variable.Name_source.fresh () ~prefix:"e" in
  let e = Effect.Variable.Name_source.next_name effect_source in
  let f = Effect.Variable.Name_source.next_name effect_source in
  let g = Effect.Variable.Name_source.next_name effect_source in
  let original =
    Arrow
      ( [ Intersection [ Option (Intersection []); Option (Variable a) ]
        ; Arrow
            ( [ Variable a ]
            , Intersection [ Variable e; Variable f; Variable g ]
            , Variable b )
        ]
      , Union [ Variable g; Labels Effect.Label.Set.empty ]
      , Union [ Variable c; Option (Variable b); Option (Variable d) ] )
  in
  print_s [%sexp (original : t)];
  [%expect
    {|
    (Arrow
     ((Intersection ((Option (Intersection ())) (Option (Variable t0))))
      (Arrow ((Variable t0))
       (Intersection ((Variable e0) (Variable e1) (Variable e2))) (Variable t1)))
     (Union ((Variable e2) (Labels ())))
     (Union ((Variable t2) (Option (Variable t1)) (Option (Variable t3)))))
  |}];
  print_s
    [%message (variables original ~polarity:`positive : Type.Variable.Set.t)];
  print_s
    [%message (variables original ~polarity:`negative : Type.Variable.Set.t)];
  print_s
    [%message
      (effect_variables original ~polarity:`positive : Effect.Variable.Set.t)];
  print_s
    [%message
      (effect_variables original ~polarity:`negative : Effect.Variable.Set.t)];
  [%expect
    {|
    ("variables original ~polarity:`positive" (t0 t1 t2 t3))
    ("variables original ~polarity:`negative" (t0 t1))
    ("effect_variables original ~polarity:`positive" (e2))
    ("effect_variables original ~polarity:`negative" (e0 e1 e2))
    |}];
  let simplified = remove_unipolar_variables original in
  print_s [%sexp (simplified : t)];
  [%expect
    {|
    (Arrow
     ((Intersection ((Option (Intersection ())) (Option (Variable t0))))
      (Arrow ((Variable t0))
       (Intersection ((Intersection ()) (Intersection ()) (Variable e2)))
       (Variable t1)))
     (Union ((Variable e2) (Labels ())))
     (Union ((Union ()) (Option (Variable t1)) (Option (Union ())))))
    |}];
  let simplified = simplify simplified in
  print_s [%sexp (simplified : t)];
  [%expect
    {|
    (Arrow
     ((Intersection ((Option (Intersection ())) (Option (Variable t0))))
      (Arrow ((Variable t0)) (Variable e2) (Variable t1)))
     (Union ((Variable e2) (Labels ())))
     (Union ((Option (Variable t1)) (Option (Union ())))))
    |}]
;;
