open! Core

(** print the given message to stderr and exit the process with a nonzero return
    code (failure) *)
let exit_with_error_messsage message =
  eprintf "%s\n" message;
  exit 1
;;

let typecheck_and_compile_to_expl filename ~print_constraint_graph =
  let open Result.Let_syntax in
  let%bind program =
    try In_channel.with_file filename ~f:Koka_zero.parse_channel with
    | Sys_error message -> exit_with_error_messsage message
  in
  Koka_zero.infer_program program ~print_constraint_graph
;;

let compile_to_eps filename ~optimise ~print_constraint_graph =
  let open Result.Let_syntax in
  let%bind program_explicit =
    typecheck_and_compile_to_expl filename ~print_constraint_graph
  in
  let%bind program_eps = Koka_zero.translate program_explicit in
  if optimise then Koka_zero.rewrite_program program_eps else return program_eps
;;

let compile
      ~in_filename
      ~optimise
      ~print_constraint_graph
      ~print_eps
      ~out_filename
  =
  match compile_to_eps in_filename ~optimise ~print_constraint_graph with
  | Error error ->
    Koka_zero.Static_error.string_of_t error |> exit_with_error_messsage
  | Ok program_eps ->
    if print_eps
    then
      Koka_zero.Evidence_passing_syntax.Program.sexp_of_t program_eps |> print_s
    else ();
    (match
       Koka_zero.compile_program
         program_eps
         ~module_name:in_filename
         ~filename:out_filename
     with
     | Error error ->
       Koka_zero.Codegen_error.string_of_t error |> exit_with_error_messsage
     | Ok () -> ())
;;

let interpret_eps filename =
  match
    compile_to_eps ~optimise:false ~print_constraint_graph:false filename
  with
  | Error error ->
    Koka_zero.Static_error.string_of_t error |> exit_with_error_messsage
  | Ok program ->
    (match Koka_zero.interpret_program program with
     | Error error ->
       Koka_zero.Runtime_error.string_of_t error
       |> Koka_zero.Util.String_utils.limit_length ~limit:1000
       |> eprintf "runtime error: %s\n"
     | Ok _unit -> ())
;;

let typecheck filename ~print_constraint_graph =
  match typecheck_and_compile_to_expl filename ~print_constraint_graph with
  | Error error ->
    Koka_zero.Static_error.string_of_t error |> exit_with_error_messsage
  | Ok _program -> ()
;;

module Flags = struct
  open Command.Param

  let filename = anon ("filename" %: string)
  let out_filename = flag "-o" (required string) ~doc:"output filename"

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
  Command.basic
    ~summary:"compile a program"
    (let%map.Command filename = Flags.filename
     and out_filename = Flags.out_filename
     and optimise = Flags.optimise
     and print_constraint_graph = Flags.print_constraint_graph
     and print_eps = Flags.print_eps in
     fun () ->
       compile
         ~in_filename:filename
         ~optimise
         ~print_constraint_graph
         ~print_eps
         ~out_filename)
;;

let command_interpret =
  Command.basic
    ~summary:"interpret a program"
    (let%map.Command filename = Flags.filename in
     fun () -> interpret_eps filename)
;;

let command_typecheck =
  Command.basic
    ~summary:"type check a program"
    (let%map.Command filename = Flags.filename
     and print_constraint_graph = Flags.print_constraint_graph in
     fun () -> typecheck filename ~print_constraint_graph)
;;

let command =
  Command.group
    ~summary:"Koka compiler"
    [ "compile", command_compile
    ; "interpret", command_interpret
    ; "check", command_typecheck
    ]
;;

let () = Command_unix.run command
