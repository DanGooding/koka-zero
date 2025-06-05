open! Core
open! Import
module EPS = Evidence_passing_syntax

(** compile a program, then write the resulting module as textual `.ll` code to
    a file *)
val compile_then_write_program
  :  ?module_name:string
  -> filename:string
  -> EPS.Program.t
  -> unit Or_codegen_error.t

(** compile a program, then write the resulting module as textual `.ll` to
    stderr *)
val compile_then_dump_program
  :  ?module_name:string
  -> EPS.Program.t
  -> unit Or_codegen_error.t
