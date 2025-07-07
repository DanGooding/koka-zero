open! Core
open! Import

type t =
  { type_metavariable_source : Type.Metavariable.Name_source.t
  ; effect_metavariable_source : Effect.Metavariable.Name_source.t
  ; type_metavariable_levels : int Type.Metavariable.Table.t
  ; effect_metavariable_levels : int Effect.Metavariable.Table.t
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
  { type_metavariable_source
  ; effect_metavariable_source
  ; type_metavariable_levels
  ; effect_metavariable_levels
  }
;;

let fresh_type_metavariable t ~level : Type.Metavariable.t =
  let meta =
    Type.Metavariable.Name_source.next_name t.type_metavariable_source
  in
  Hashtbl.add_exn t.type_metavariable_levels ~key:meta ~data:level;
  meta
;;

let fresh_type t ~level : Type.Mono.t =
  Metavariable (fresh_type_metavariable t ~level)
;;

let fresh_effect_metavariable t ~level : Effect.Metavariable.t =
  let meta =
    Effect.Metavariable.Name_source.next_name t.effect_metavariable_source
  in
  Hashtbl.add_exn t.effect_metavariable_levels ~key:meta ~data:level;
  meta
;;

let fresh_effect t ~level : Effect.t =
  Unknown (Metavariable (fresh_effect_metavariable t ~level))
;;

let type_level_exn t meta = Hashtbl.find_exn t.type_metavariable_levels meta
let effect_level_exn t meta = Hashtbl.find_exn t.effect_metavariable_levels meta
