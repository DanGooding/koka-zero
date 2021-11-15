(** represents a position in a source file *)
type t [@@deriving sexp]

val t_of_lexing_position : Lexing.position -> t
val string_of_t : t -> string
