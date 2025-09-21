open Core
open Import
open Koka_zero_util

type 'a t

include Monad.S with type 'a t := 'a t
include Monad_utils.S with type 'a t := 'a t

val fresh_marker : Value.Marker.t t
val impossible_error : string -> 'a t
val unsupported_feature_error : string -> 'a t
val no_matching_pattern_error : string -> 'a t

(** try performing an i/o operation, returning a [Runtime_error.io_error] with
    the given message on exception *)
val try_io_with : message:string -> (unit -> 'a) -> 'a t

val type_error : expected:string -> ('a -> Sexp.t) -> 'a -> 'b t
val run : 'a t -> 'a Or_runtime_error.t
