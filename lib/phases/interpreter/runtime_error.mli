type t [@@deriving sexp]

(** reports a runtime type (or similar) error which should have been caught at
    compile time *)
val impossible_error : string -> t

(** reports an error due to trying to evaluate an unimplmeneted construct *)
val unsupported_feature_error : string -> t

(** reports an error performing a built-in io operation *)
val io_error : string -> t

val string_of_t : t -> string

module Or_runtime_error : sig
  type nonrec 'a t = ('a, t) Result.t
end
