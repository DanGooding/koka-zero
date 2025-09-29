open! Core
open! Import

val compile_ir_to_exe
  :  ir_filename:string
  -> config:Koka_zero_config.Backend.t
  -> exe_filename:string
  -> optimise:bool
  -> enable_run_stats:bool
  -> unit Or_error.t
