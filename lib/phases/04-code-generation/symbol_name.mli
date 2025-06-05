open Import

module Suffix_source : sig
  (** generates a unique suffix for local non-unique names *)
  type t [@@deriving sexp]

  val fresh : t
end

(** a symbol is the name of an llvm global varaiable or function *)
type t [@@deriving sexp]

(** get as a string to pass to llvm creation functions *)
val to_string : t -> string

(** name of the entry point symbol `main` *)
val main : t

(** convert a toplevel name (which is therefore unique) to a symbol *)
val of_toplevel : Variable.t -> t

(** construct from the name of a runtime library function, failing if the name
    is not correctly prefixed *)
val of_runtime_exn : string -> t

(** convert a non-unique local name into a unique symbol by prefixing with its
    containing function's name, and adding unique suffix *)
val of_local
  :  containing:t
  -> Variable.t
  -> Suffix_source.t
  -> t * Suffix_source.t

(** create a unique symbol for a local anonymous function *)
val of_local_anonymous : containing:t -> Suffix_source.t -> t * Suffix_source.t
