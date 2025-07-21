open! Core

let error_of_static_error (result : 'a Koka_zero.Static_error.Or_static_error.t)
  : 'a Or_error.t
  =
  Result.map_error result ~f:(fun static_error ->
    Koka_zero.Static_error.to_string static_error |> Error.of_string)
;;

let typecheck_and_compile_to_expl filename ~print_constraint_graph =
  let open Result.Let_syntax in
  let%bind program =
    match In_channel.with_file filename ~f:Koka_zero.parse_channel with
    | program -> error_of_static_error program
    | exception Sys_error message -> Or_error.error_string message
  in
  Koka_zero.infer_program program ~print_constraint_graph
  |> error_of_static_error
;;

let compile_to_eps filename ~optimise ~print_constraint_graph =
  let open Result.Let_syntax in
  let%bind program_explicit =
    typecheck_and_compile_to_expl filename ~print_constraint_graph
  in
  let%bind program_eps =
    Koka_zero.translate program_explicit |> error_of_static_error
  in
  if optimise
  then Koka_zero.rewrite_program program_eps |> error_of_static_error
  else return program_eps
;;

let compile_to_ir
      ~in_filename
      ~optimise
      ~print_constraint_graph
      ~print_eps
      ~out_filename
  =
  let open Result.Let_syntax in
  let%bind program_eps =
    compile_to_eps in_filename ~optimise ~print_constraint_graph
  in
  if print_eps
  then
    Koka_zero.Evidence_passing_syntax.Program.sexp_of_t program_eps |> print_s
  else ();
  Koka_zero.compile_program
    program_eps
    ~module_name:in_filename
    ~filename:out_filename
  |> Result.map_error ~f:(fun error ->
    Error.of_string [%string "codegen error: %{error#Koka_zero.Codegen_error}"])
;;

let compile_to_exe
      ~in_filename
      ~optimise
      ~print_constraint_graph
      ~print_eps
      ~koka_zero_config
  =
  let open Result.Let_syntax in
  let ir_filename =
    String.chop_suffix_if_exists in_filename ~suffix:".kk" ^ ".ll"
  in
  let%bind () =
    compile_to_ir
      ~in_filename
      ~out_filename:ir_filename
      ~optimise
      ~print_constraint_graph
      ~print_eps
  in
  let exe_filename =
    match String.chop_suffix in_filename ~suffix:".kk" with
    | Some base -> base
    | None -> in_filename ^ ".exe"
  in
  Koka_zero.compile_ir_to_exe
    ~ir_filename
    ~exe_filename
    ~config:koka_zero_config
    ~optimise
;;

let interpret_eps filename =
  let open Result.Let_syntax in
  let%bind program =
    compile_to_eps ~optimise:false ~print_constraint_graph:false filename
  in
  match Koka_zero.interpret_program program with
  | Ok _unit -> Ok ()
  | Error error ->
    Koka_zero.Runtime_error.to_string error
    |> Koka_zero.Util.String_utils.limit_length ~limit:1000
    |> sprintf "runtime error: %s\n"
    |> Or_error.error_string
;;

let typecheck filename ~print_constraint_graph =
  let open Result.Let_syntax in
  let%map _program =
    typecheck_and_compile_to_expl filename ~print_constraint_graph
  in
  ()
;;

let print_example_config () =
  print_s
    [%sexp
      (Koka_zero.Koka_zero_config.example
       : Koka_zero.Koka_zero_config.Stable.V1.t)]
;;

module Flags = struct
  open Command.Param

  let in_filename = anon ("filename" %: string)
  let out_filename = flag "-o" (required string) ~doc:"output filename"

  let config_filename =
    flag
      "-config"
      (required string)
      ~doc:"language config file - sexp containing paths to required libraries"
  ;;

  let optimise =
    flag "-optimise" no_arg ~doc:"apply tree rewriting optimisations"
  ;;

  let print_constraint_graph =
    flag
      "-dump-constraint-graph"
      no_arg
      ~doc:"print the type constraints as a DOT graphviz graph"
  ;;

  let print_eps =
    flag "-dump-eps" no_arg ~doc:"print the intermediate evidence passing AST"
  ;;
end

let command_compile =
  Command.basic_or_error
    ~summary:"compile a program"
    (let%map.Command in_filename = Flags.in_filename
     and config_filename = Flags.config_filename
     and optimise = Flags.optimise
     and print_constraint_graph = Flags.print_constraint_graph
     and print_eps = Flags.print_eps in
     fun () ->
       let open Result.Let_syntax in
       let%bind koka_zero_config =
         Koka_zero.Koka_zero_config.load config_filename
       in
       compile_to_exe
         ~in_filename
         ~optimise
         ~print_constraint_graph
         ~print_eps
         ~koka_zero_config)
;;

let command_compile_to_ir =
  Command.basic_or_error
    ~summary:"compile a program, stopping at the IR phase"
    (let%map.Command in_filename = Flags.in_filename
     and out_filename = Flags.out_filename
     and optimise = Flags.optimise
     and print_constraint_graph = Flags.print_constraint_graph
     and print_eps = Flags.print_eps in
     fun () ->
       compile_to_ir
         ~in_filename
         ~optimise
         ~print_constraint_graph
         ~print_eps
         ~out_filename)
;;

let command_interpret =
  Command.basic_or_error
    ~summary:"interpret a program"
    (let%map.Command in_filename = Flags.in_filename in
     fun () -> interpret_eps in_filename)
;;

let command_typecheck =
  Command.basic_or_error
    ~summary:"type check a program"
    (let%map.Command in_filename = Flags.in_filename
     and print_constraint_graph = Flags.print_constraint_graph in
     fun () -> typecheck in_filename ~print_constraint_graph)
;;

let command_example_config =
  Command.basic
    ~summary:"print an example config file"
    (Command.Param.return @@ fun () -> print_example_config ())
;;

let command =
  Command.group
    ~summary:"Koka compiler"
    [ "compile", command_compile
    ; "compile-to-ir", command_compile_to_ir
    ; "interpret", command_interpret
    ; "check", command_typecheck
    ; "example-config", command_example_config
    ]
;;

let () = Command_unix.run command
