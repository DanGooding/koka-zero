open Core
open Koka_zero_util

module T = struct
  type 'a t = bool * 'a

  let return x = (* arbitrary choice *) false, x

  let map =
    let map (changed, x) ~f = changed, f x in
    `Custom map
  ;;

  let bind (changed, x) ~f =
    let changed', y = f x in
    changed || changed', y
  ;;
end

module T' = struct
  include T
  include Monad.Make (T)
end

include T'
include Monad_utils.Make (T')

let original x = false, x
let modified x = true, x
let inspect (changed, x) = if changed then `Modified x else `Unchanged
let value (_, x) = x

let rec apply_while_changes x ~f =
  let changed, x' = f x in
  if changed
  then (
    let _, x_final = apply_while_changes x' ~f in
    true, x_final)
  else false, x'
;;

let original_for_none f x =
  match f x with
  | None -> original x
  | Some x' -> modified x'
;;
