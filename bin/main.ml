open! Core
module Or_static_error = Koka_zero.Static_error.Or_static_error

(** represents two failure modes:
    Static_error: the user gave us bad koka code,
    internal Error: internal bugs / unwriteable output directories etc. *)
module Or_static_or_internal_error = struct
  type 'a t = 'a Or_static_error.t Or_error.t [@@deriving sexp_of]

  let exit (t : unit t) =
    match t with
    | Ok (Ok ()) -> exit 0
    | Ok (Error static_error) ->
      Stdio.prerr_endline (Koka_zero.Static_error.to_string static_error);
      exit 1
    | Error internal_error ->
      Stdio.prerr_endline (Error.to_string_hum internal_error);
      exit 2
  ;;

  let command_basic ~summary ?readme param =
    Command.basic
      ~summary
      ?readme
      (let%map.Command run = param in
       fun () -> run () |> exit)
  ;;
end

let typecheck_and_compile_to_expl filename ~print_constraint_graph =
  let%map.Or_error program =
    match In_channel.with_file filename ~f:Koka_zero.parse_channel with
    | program -> Ok program
    | exception Sys_error message -> Or_error.error_string message
  in
  let%bind.Or_static_error program = program in
  Koka_zero.infer_program program ~print_constraint_graph
;;

let compile_to_eps filename ~optimise ~print_constraint_graph =
  let%map.Or_error program_explicit =
    typecheck_and_compile_to_expl filename ~print_constraint_graph
  in
  let%bind.Or_static_error program_explicit = program_explicit in
  let%bind.Or_static_error program_eps = Koka_zero.translate program_explicit in
  if optimise then Koka_zero.rewrite_program program_eps else Ok program_eps
;;

let compile_to_ir
      ~in_filename
      ~optimise
      ~print_constraint_graph
      ~print_eps
      ~out_filename
  =
  match%bind.Or_error
    compile_to_eps in_filename ~optimise ~print_constraint_graph
  with
  | Error static_error -> Ok (Error static_error)
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
     | Ok () -> Ok (Ok ())
     | Error codegen_error ->
       (* codegen errors are not user errors *)
       Or_error.error_string
         [%string "codegen error: %{codegen_error#Koka_zero.Codegen_error}"])
;;

let compile_to_exe
      ~in_filename
      ~optimise
      ~exe_filename
      ~where_to_save_temps
      ~print_constraint_graph
      ~print_eps
      ~koka_zero_config
      ~enable_run_stats
  =
  let exe_filename =
    match exe_filename with
    | Some exe_filename -> exe_filename
    | None ->
      (match String.chop_suffix in_filename ~suffix:".kk" with
       | Some base -> base
       | None -> in_filename ^ ".exe")
  in
  let ir_filename =
    let temp_dir =
      match where_to_save_temps with
      | `With_input -> Filename.dirname in_filename
      | `With_output -> Filename.dirname exe_filename
    in
    let in_basename = Filename.basename in_filename in
    let temp_basename =
      String.chop_suffix_if_exists in_basename ~suffix:".kk" ^ ".ll"
    in
    temp_dir ^/ temp_basename
  in
  match%bind.Or_error
    compile_to_ir
      ~in_filename
      ~out_filename:ir_filename
      ~optimise
      ~print_constraint_graph
      ~print_eps
  with
  | Error static_error -> Ok (Error static_error)
  | Ok () ->
    let%map.Or_error () =
      Koka_zero.compile_ir_to_exe
        ~ir_filename
        ~exe_filename
        ~config:koka_zero_config
        ~optimise
        ~enable_run_stats
    in
    Ok ()
;;

let interpret_eps filename =
  match%bind.Or_error
    compile_to_eps ~optimise:false ~print_constraint_graph:false filename
  with
  | Error static_error -> Ok (Error static_error)
  | Ok program ->
    (match Koka_zero.interpret_program program with
     | Ok _unit -> Ok (Ok ())
     | Error error ->
       Koka_zero.Runtime_error.to_string error
       |> Koka_zero.Util.String_utils.limit_length ~limit:1000
       |> sprintf "runtime error: %s\n"
       |> Or_error.error_string)
;;

let typecheck filename ~print_constraint_graph =
  let%map.Or_error maybe_program =
    typecheck_and_compile_to_expl filename ~print_constraint_graph
  in
  let%map.Or_static_error _program = maybe_program in
  ()
;;

let create_config ~clang_exe ~runtime_path ~gc_path ~prelude_path ~out_filename =
  let config =
    { Koka_zero.Koka_zero_config.clang_exe
    ; runtime_path
    ; gc_path
    ; prelude_path = Some prelude_path
    }
  in
  Koka_zero.Koka_zero_config.write config out_filename
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
  let out_filename = flag "-o" (required string) ~doc:"FILE output filename"

  let exe_filename =
    flag
      "-o"
      (optional string)
      ~doc:"FILE exe output filename - default is derived from input filename"
  ;;

  let where_to_save_temps =
    let arg_type =
      Arg_type.of_alist_exn
        [ "input", `With_input; "output", `With_output ]
        ~list_values_in_help:true
    in
    flag
      "-save-temps-with"
      (optional_with_default `With_input arg_type)
      ~doc:"intput|output where to save temporary files"
  ;;

  let config_filename =
    flag
      "-config"
      (required string)
      ~doc:
        "FILE language config file - sexp containing paths to required \
         libraries"
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

  let prelude_path =
    flag "-prelude" (required string) ~doc:"FILE path to koka prelude.kk file"
  ;;

  let clang_exe =
    flag "-clang" (required string) ~doc:"FILE path to clang binary"
  ;;

  let runtime_path =
    flag "-runtime" (required string) ~doc:"FILE path to koka runtime.c file"
  ;;

  let gc_path =
    flag
      "-gc"
      (optional string)
      ~doc:
        "DIR path to libgc include/ and lib/ subdirectories. If excluded, will \
         compile without garbage collection."
  ;;

  let enable_run_stats =
    flag
      "-enable-run-stats"
      no_arg
      ~doc:
        "report running-time statistics to the filename in env var \
         KOKA_WRITE_RUN_STATS"
  ;;
end

let command_compile =
  Or_static_or_internal_error.command_basic
    ~summary:"compile a program"
    (let%map.Command in_filename = Flags.in_filename
     and config_filename = Flags.config_filename
     and optimise = Flags.optimise
     and exe_filename = Flags.exe_filename
     and where_to_save_temps = Flags.where_to_save_temps
     and print_constraint_graph = Flags.print_constraint_graph
     and print_eps = Flags.print_eps
     and enable_run_stats = Flags.enable_run_stats in
     fun () ->
       let open Result.Let_syntax in
       let%bind koka_zero_config =
         Koka_zero.Koka_zero_config.load config_filename
       in
       compile_to_exe
         ~in_filename
         ~optimise
         ~exe_filename
         ~where_to_save_temps
         ~print_constraint_graph
         ~print_eps
         ~koka_zero_config
         ~enable_run_stats)
;;

let command_compile_to_ir =
  Or_static_or_internal_error.command_basic
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
  Or_static_or_internal_error.command_basic
    ~summary:"interpret a program"
    (let%map.Command in_filename = Flags.in_filename in
     fun () -> interpret_eps in_filename)
;;

let command_typecheck =
  Command.basic_or_error
    ~summary:"type check a program"
    (let%map.Command in_filename = Flags.in_filename
     and print_constraint_graph = Flags.print_constraint_graph in
     fun () ->
       typecheck in_filename ~print_constraint_graph
       |> Or_static_or_internal_error.exit)
;;

let command_example_config =
  Command.basic
    ~summary:"print an example config file"
    (Command.Param.return @@ fun () -> print_example_config ())
;;

let command_create_config =
  Command.basic_or_error
    ~summary:"create and populate config file"
    (let%map.Command prelude_path = Flags.prelude_path
     and clang_exe = Flags.clang_exe
     and runtime_path = Flags.runtime_path
     and gc_path = Flags.gc_path
     and out_filename = Flags.out_filename in
     fun () ->
       create_config
         ~prelude_path
         ~clang_exe
         ~runtime_path
         ~gc_path
         ~out_filename)
;;

let command =
  Command.group
    ~summary:"Koka compiler"
    [ "compile", command_compile
    ; "compile-to-ir", command_compile_to_ir
    ; "interpret", command_interpret
    ; "check", command_typecheck
    ; "example-config", command_example_config
    ; "create-config", command_create_config
    ]
;;

let () = Command_unix.run command
