open! Core
open! Import

(** names which aren't reserved, but have significance *)

val resume : Variable.t
val main : Variable.t
val entry_point : Variable.t
val console_effect : Effect.Label.t
val println : Variable.t
val print_int : Variable.t
val println_int : Variable.t
val read_int : Variable.t
