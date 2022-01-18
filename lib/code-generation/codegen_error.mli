open Core

type t [@@deriving sexp]

(** reports an error which should have been caught at the type inference stage -
    essentially a nicer [assert false].

    (bug free code is far from impossible of course!) *)
val impossible_error : string -> t

(** reports an error due to trying to compile an unimplemented construct *)
val unsupported_feature_error : string -> t

(** reports an invaid function/module, with a validation report as its message *)
val verifier_error : string -> t

val string_of_t : t -> string

module Or_codegen_error : sig
  type nonrec 'a t = ('a, t) Result.t
end
