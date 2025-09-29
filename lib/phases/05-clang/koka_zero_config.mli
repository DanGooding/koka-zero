open! Core
open! Import

module Frontend : sig
  type t = { prelude_path : Filename.t option } [@@deriving sexp_of]

  val load : Filename.t -> t Or_error.t
  val write : t -> Filename.t -> unit Or_error.t
end

module Backend : sig
  type t =
    { clang_exe : Filename.t
    ; runtime_path : Filename.t
    ; gc_path : Filename.t option
    }
  [@@deriving sexp_of]
end

type t =
  { frontend_config : Frontend.t
  ; backend_config : Backend.t
  }
[@@deriving sexp_of]

val load : Filename.t -> t Or_error.t
val write : t -> Filename.t -> unit Or_error.t

(** an example config that works on my machine *)
val example : t
