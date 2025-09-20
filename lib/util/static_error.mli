open! Core

(** An issue in supplied koka code (e.g. syntax error / type error) *)
type t [@@deriving sexp_of]

module Or_static_error : sig
  type nonrec 'a t = ('a, t) Result.t [@@deriving sexp_of]

  val ok_exn : 'a t -> 'a

  include Monad.S with type 'a t := 'a t
  include Monad_utils.S with type 'a t := 'a t
end

val syntax_error : ?at:Source_location.t -> string -> t
val syntax_error_s : ?at:Source_location.t -> Sexp.t -> t
val unsupported_feature : ?at:Source_location.t -> string -> t
val type_error : ?at:Source_location.t -> string -> t
val type_error_s : ?at:Source_location.t -> Sexp.t -> t
val type_errorf : ?at:Source_location.t -> ('a, unit, string, t) format4 -> 'a
val type_error_of_error : ?at:Source_location.t -> Error.t -> t
val to_string : t -> string
val tag_s : t -> tag:Sexp.t -> t
