open! Core
open! Import

let run () ~prog ~args =
  let child_pid = Core_unix.fork_exec () ~prog ~argv:(prog :: args) in
  let result = Core_unix.waitpid child_pid in
  Core_unix.Exit_or_signal.or_error result
;;

let compile_ir_to_exe
      ~ir_filename
      ~(config : Koka_zero_config.t)
      ~exe_filename
      ~optimise
  =
  let opt_flag = Option.some_if optimise "-O3" |> Option.to_list in
  let gc_flags =
    match config.gc_path with
    | None ->
      eprintf "compiling without garbage collector";
      []
    | Some gc_path ->
      [ "-DENABLE_GC"
      ; [%string "-L%{gc_path}/lib"]
      ; [%string "-I%{gc_path}/include"]
      ; "-lgc"
      ]
  in
  let warning_flags = [ "-Wall"; "-Wno-override-module" ] in
  let flags = warning_flags @ opt_flag @ gc_flags in
  let inputs = [ ir_filename; config.runtime_path ] in
  let args = flags @ inputs @ [ "-o"; exe_filename ] in
  run () ~prog:config.clang_exe ~args
;;
