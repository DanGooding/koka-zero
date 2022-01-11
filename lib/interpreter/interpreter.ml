open Core
open Import
open Koka_zero_util

module State = struct
  type t = { next_marker : int } [@@deriving sexp]

  let initial = { next_marker = 0 }
end

module T = struct
  type 'a t = State.t -> ('a * State.t) Or_runtime_error.t

  let bind m ~f s =
    let%bind.Result x, s' = m s in
    f x s'
  ;;

  let return x s = Result.Ok (x, s)

  let map =
    let map m ~f s =
      let%map.Result x, s' = m s in
      f x, s'
    in
    `Custom map
  ;;
end

module T' = struct
  include T
  include Monad.Make (T)
end

include T'
include Monad_utils.Make (T')

let fresh_marker { State.next_marker } =
  Result.Ok (next_marker, { State.next_marker = next_marker + 1 })
;;

let run (m : 'a t) : 'a Or_runtime_error.t =
  let%map.Result x, _s = m State.initial in
  x
;;

let type_error
    ~(expected : string)
    (sexp_of_actual : 'a -> Sexp.t)
    (actual : 'a)
    (_ : State.t)
  =
  let message =
    sprintf
      "type error: expected %s, got: %s"
      expected
      (sexp_of_actual actual |> Sexp.to_string_hum)
  in
  Result.Error (Runtime_error.impossible_error message)
;;

let impossible_error message _state =
  Result.Error (Runtime_error.impossible_error message)
;;

let unsupported_feature_error message _state =
  Result.Error (Runtime_error.unsupported_feature_error message)
;;

let try_io_with ~message f state =
  let%map.Result x =
    Result.try_with f
    |> Result.map_error ~f:(fun _err -> Runtime_error.io_error message)
  in
  x, state
;;
