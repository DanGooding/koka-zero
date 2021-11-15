(** An issue in supplied koka code (e.g. syntax error / type error) *)
type t [@@deriving sexp]

val syntax_error : ?at:Source_location.t -> string -> t
val string_of_t : t -> string

(* TODO: an ['a Or_static_error.t] would be really useful *)
