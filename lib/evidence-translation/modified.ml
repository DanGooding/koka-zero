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
