open Import
module EPS = Koka_zero_evidence_translation.Evidence_passing_syntax

(** compile a program, then write the resulting module as textual `.ll` code to
    a file *)
val compile_then_write_program
  :  EPS.Program.t
  -> filename:string
  -> unit Or_codegen_error.t

(** compile a program, then write the resulting module as textual `.ll` to
    stderr *)
val compile_then_dump_program : EPS.Program.t -> unit Or_codegen_error.t
