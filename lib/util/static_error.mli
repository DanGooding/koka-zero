(** An issue in supplied koka code (e.g. syntax error / type error) *)
type t [@@deriving sexp]

module Or_static_error : sig
  type nonrec 'a t = ('a, t) Result.t
end

val syntax_error : ?at:Source_location.t -> string -> t
val unsupported_syntax : ?at:Source_location.t -> string -> t
val type_error : ?at:Source_location.t -> string -> t
val string_of_t : t -> string

(* TODO: an ['a Or_static_error.t] would be really useful *)
