open! Core
open! Import
open Evidence_passing_syntax

(** apply optimising tree transformations to a program *)
val rewrite_program : Program.t -> Program.t Generation.t
