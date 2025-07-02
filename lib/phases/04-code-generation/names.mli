open! Core
open! Import

(** convert a [Variable.t] to a string usable as a register name *)
val register_name_of_variable : Variable.t -> string
