open Core

module T = struct
  type t =
    | User of string
        (** user code can contain all possible names (except keywords), so we
            namespace them separately from internally used names *)
    | Language of string
        (** meaningful names internal to the language implementation *)
    | Generated of string
        (** sequentially generated names. Each generator is excepted to use a
            unique prefix *)
  [@@deriving compare, sexp]

  let of_generated_name s = Generated s
end (* disable "fragile-match" for generated code *) [@warning "-4"]

include T
include Comparable.Make (T)

let of_user x = User x
let of_language_internal x = Language x
let of_generated x = Generated x

(* deprecated - use [of_user] instead - type changed to prevent accidental
   usage *)
let of_string _x = ()

let to_string_user = function
  | User x -> x
  | (Language _ | Generated _) as t -> sexp_of_t t |> Sexp.to_string_hum
;;

module Name_source = Name_source.Make (T)
