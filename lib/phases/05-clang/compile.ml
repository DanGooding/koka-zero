open! Core
open! Import

let run () ~prog ~args ~allow_output =
  match
    let { Core_unix.Process_info.pid; stdin = _; stdout; stderr } =
      Core_unix.create_process ~prog ~args
    in
    let output = Core_unix.in_channel_of_descr stdout in
    let error_output = Core_unix.in_channel_of_descr stderr in
    let result = Core_unix.waitpid pid in
    let output = In_channel.input_all output in
    let error_output = In_channel.input_all error_output in
    let result = Core_unix.Exit_or_signal.or_error result in
    result, output, error_output
  with
  | Ok (), "", "" -> Ok ()
  | Ok (), _stdout, "" when allow_output -> Ok ()
  | Ok (), stdout, "" ->
    Or_error.error_s
      [%message
        "unexpected output when running"
          (prog : string)
          (args : string list)
          (stdout : string)]
  | ((Ok () | Error _) as result), stdout, stderr ->
    Or_error.error_s
      [%message
        "error when running"
          (prog : string)
          (args : string list)
          (result : unit Or_error.t)
          (stdout : string)
          (stderr : string)]
  | exception exn ->
    Or_error.error_s
      [%message
        "failed to run" (prog : string) (args : string list) (exn : Exn.t)]
;;

let compile_ir_to_exe
      ~ir_filename
      ~(config : Koka_zero_config.t)
      ~exe_filename
      ~optimise
  =
  let opt_flag = Option.some_if optimise "-O3" |> Option.to_list in
  let gc_flags, gc_lib =
    match config.gc_path with
    | None ->
      eprintf "compiling without garbage collector\n";
      [], []
    | Some gc_path ->
      ( [ "-DENABLE_GC"
        ; [%string "-L%{gc_path}/lib"]
        ; [%string "-I%{gc_path}/include"]
        ]
      , [ "-lgc" ] )
  in
  let warning_flags = [ "-Wall"; "-Wno-override-module" ] in
  let flags = warning_flags @ opt_flag @ gc_flags in
  let inputs = [ ir_filename; config.runtime_path ] in
  (* it's important for linking that gc library is listed _after_ user code *)
  let args = flags @ inputs @ gc_lib @ [ "-o"; exe_filename ] in
  run () ~prog:config.clang_exe ~args ~allow_output:false
;;
