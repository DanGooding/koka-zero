open! Core
open! Import

type t =
  { clang_exe : Filename.t
  ; runtime_path : Filename.t
  ; gc_path : Filename.t option
  }
[@@deriving sexp_of]

val load : Filename.t -> t Or_error.t

(** an example config that works on my machine *)
val example : t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp]
  end
end
