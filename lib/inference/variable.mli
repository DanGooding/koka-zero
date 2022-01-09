open Core

type t =
  | User of string
      (** user code can contain all possible names (except keywords), so we
          namespace them separately from internally used names *)
  | Language of string
      (** meaningful names internal to the language implementation *)
  | Generated of string
      (** sequentially generated names. Each generator is expected to use a
          unique prefix *)
[@@deriving compare, sexp]

val of_user : string -> t
val of_language_internal : string -> t
val of_generated : string -> t

include Comparable.S with type t := t

(* type changed to prevent accidental usage *)
val of_string : string -> unit
  [@@deprecated "use one of the descriptive [of_...] constructor functions"]

(** convert back to a string, retuning `User` strings unchanged, and the rest in
    sexp form *)
val to_string_user : t -> string

module Name_source : sig
  include Name_source.S with type Name.t := t
end
