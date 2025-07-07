open! Core
open! Import

module Binding = struct
  type t =
    | Value of Type.t
    | Operation of
        { argument : Type.Mono.t
        ; label : Effect.Label.t
        ; answer : Type.Mono.t
        }
  [@@deriving sexp_of]
end

module Entry = struct
  type t =
    { binding : Binding.t
    ; shadowable : bool
    }
  [@@deriving sexp_of]
end

module Or_cannot_shadow = struct
  type 'a t =
    [ `Ok of 'a
    | `Cannot_shadow
    ]
  [@@deriving sexp_of]
end

type t = Entry.t Variable.Map.t [@@deriving sexp_of]

let can_bind t var =
  match Map.find t var with
  | Some { Entry.shadowable; _ } -> shadowable
  | None -> true
;;

let extend t ~var ~type_ =
  if can_bind t var
  then (
    let binding = Binding.Value type_ in
    let entry = { Entry.shadowable = true; binding } in
    `Ok (Map.set t ~key:var ~data:entry))
  else `Cannot_shadow
;;

let extend_toplevel t ~var ~type_ =
  let binding = Binding.Value type_ in
  let entry = { Entry.shadowable = true; binding } in
  match Map.add t ~key:var ~data:entry with
  | `Duplicate -> `Cannot_shadow
  | `Ok t' -> `Ok t'
;;

let extend_operation t ~var ~label ~argument ~answer =
  if Map.mem t var
  then `Cannot_shadow
  else (
    let binding = Binding.Operation { label; argument; answer } in
    let entry = { Entry.shadowable = false; binding } in
    `Ok (Map.set t ~key:var ~data:entry))
;;

let find t var =
  match Map.find t var with
  | None -> None
  | Some { Entry.binding; _ } -> Some binding
;;

let empty = Variable.Map.empty
