open! Core
open! Import

module Location = struct
  type t =
    | Expr of Minimal_syntax.Expr.t
    | Parameter of Parameter.t
    | Pattern of Pattern.t
    | F_self of Variable.t
    | Application of Minimal_syntax.Expr.t * Minimal_syntax.Expr.t list
    | Handler_subject of Minimal_syntax.Expr.handler
    | Handler_result of Minimal_syntax.Expr.handler
    | List_element of t
    | Instantiation of t
  [@@deriving sexp_of]
end

type t =
  { type_metavariable_source : Type.Metavariable.Name_source.t
  ; effect_metavariable_source : Effect.Metavariable.Name_source.t
  ; type_metavariable_levels : int Type.Metavariable.Table.t
  ; effect_metavariable_levels : int Effect.Metavariable.Table.t
  ; type_metavariable_locations : Location.t Type.Metavariable.Table.t
  ; effect_metavariable_locations : Location.t Effect.Metavariable.Table.t
  }
[@@deriving sexp_of]

let create () =
  let type_metavariable_source =
    Type.Metavariable.Name_source.fresh () ~prefix:"tm"
  in
  let effect_metavariable_source =
    Effect.Metavariable.Name_source.fresh () ~prefix:"em"
  in
  let type_metavariable_levels = Type.Metavariable.Table.create () in
  let effect_metavariable_levels = Effect.Metavariable.Table.create () in
  let type_metavariable_locations = Type.Metavariable.Table.create () in
  let effect_metavariable_locations = Effect.Metavariable.Table.create () in
  { type_metavariable_source
  ; effect_metavariable_source
  ; type_metavariable_levels
  ; effect_metavariable_levels
  ; type_metavariable_locations
  ; effect_metavariable_locations
  }
;;

let fresh_type_metavariable t ~level ~location : Type.Metavariable.t =
  let meta =
    Type.Metavariable.Name_source.next_name t.type_metavariable_source
  in
  Hashtbl.add_exn t.type_metavariable_levels ~key:meta ~data:level;
  Hashtbl.add_exn t.type_metavariable_locations ~key:meta ~data:location;
  meta
;;

let fresh_type t ~level ~location : Type.Mono.t =
  Metavariable (fresh_type_metavariable t ~level ~location)
;;

let fresh_effect_metavariable t ~level ~location : Effect.Metavariable.t =
  let meta =
    Effect.Metavariable.Name_source.next_name t.effect_metavariable_source
  in
  Hashtbl.add_exn t.effect_metavariable_levels ~key:meta ~data:level;
  Hashtbl.add_exn t.effect_metavariable_locations ~key:meta ~data:location;
  meta
;;

let fresh_effect t ~level ~location : Effect.t =
  Metavariable (fresh_effect_metavariable t ~level ~location)
;;

let type_level_exn t meta = Hashtbl.find_exn t.type_metavariable_levels meta
let effect_level_exn t meta = Hashtbl.find_exn t.effect_metavariable_levels meta

let type_location_exn t meta =
  Hashtbl.find_exn t.type_metavariable_locations meta
;;

let effect_location_exn t meta =
  Hashtbl.find_exn t.effect_metavariable_locations meta
;;

let sexp_of_type t (type_ : Type.Mono.t) =
  match type_ with
  | Metavariable meta ->
    let level = type_level_exn t meta in
    let location = type_location_exn t meta in
    [%sexp { meta : Type.Metavariable.t; level : int; location : Location.t }]
  | Primitive _ | Arrow _ | List _ | Tuple _ ->
    let max_level =
      Type.Mono.max_level
        type_
        ~type_metavariable_level:(type_level_exn t)
        ~effect_metavariable_level:(effect_level_exn t)
    in
    [%sexp { type_ : Type.Mono.t; max_level : int }]
;;

let sexp_of_effect t (effect_ : Effect.t) =
  match effect_ with
  | Metavariable meta ->
    let level = effect_level_exn t meta in
    let location = effect_location_exn t meta in
    [%sexp { meta : Effect.Metavariable.t; level : int; location : Location.t }]
  | Labels _ | Handled _ ->
    let max_level =
      Effect.max_level effect_ ~metavariable_level:(effect_level_exn t)
    in
    [%sexp { effect_ : Effect.t; max_level : int }]
;;
