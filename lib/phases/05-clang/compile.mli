open! Core
open! Import

val compile_ir_to_exe
  :  ir_filename:string
  -> config:Koka_zero_config.t
  -> exe_filename:string
  -> optimise:bool
  -> unit Or_error.t
