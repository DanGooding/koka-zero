open Import
open Koka_zero_evidence_translation
module Codegen_error = Codegen_error

(** compile a program, then write the resulting module as textual `.ll` code to
    a file *)
val compile_then_write_program
  :  ?module_name:string
  -> filename:string
  -> Evidence_passing_syntax.Program.t
  -> unit Or_codegen_error.t

(** compile a program, then write the resulting module as textual `.ll` to
    stderr *)
val compile_then_dump_program
  :  ?module_name:string
  -> Evidence_passing_syntax.Program.t
  -> unit Or_codegen_error.t
