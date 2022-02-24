(** An issue in supplied koka code (e.g. syntax error / type error) *)
type t [@@deriving sexp]

module Or_static_error : sig
  type nonrec 'a t = ('a, t) Result.t
end

val syntax_error : ?at:Source_location.t -> string -> t
val unsupported_feature : ?at:Source_location.t -> string -> t
val type_error : ?at:Source_location.t -> string -> t
val string_of_t : t -> string

(** swap the order of a nested [ Or_static_error option ], preserving the inner
    value, or the error, if either exists. The equivalent of [Result.all] which
    operates on option values rather than list items *)
val all_option : 'a Or_static_error.t option -> 'a option Or_static_error.t

(** equivalent of [all] for non-empty lists *)
val all_non_empty
  :  'a Or_static_error.t Non_empty_list.t
  -> 'a Non_empty_list.t Or_static_error.t

(** a version of [List.fold_right] which runs in this monad. It is the dual of
    [List.fold_result] (which does a leftward fold). *)
val list_fold_right
  :  'a list
  -> init:'accum
  -> f:('a -> 'accum -> 'accum Or_static_error.t)
  -> 'accum Or_static_error.t
