open Core

let limit_length ~limit s = String.slice s 0 (min limit (String.length s))

let compile_to_eps filename =
  let open Result.Let_syntax in
  let%bind program = In_channel.with_file filename ~f:Koka_zero.parse_channel in
  let%map program_explicit = Koka_zero.infer_program program in
  Koka_zero.translate program_explicit
;;

let compile ~in_filename ~print_eps ~out_filename =
  match compile_to_eps in_filename with
  | Error error -> Koka_zero.Static_error.string_of_t error |> eprintf "%s\n"
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
      Koka_zero.Codegen_error.string_of_t error |> eprintf "compile error: %s\n"
    | Ok () -> ())
;;

let interpret_eps filename =
  match compile_to_eps filename with
  | Error error -> Koka_zero.Static_error.string_of_t error |> eprintf "%s\n"
  | Ok program ->
    (match Koka_zero.interpret_program program with
    | Error error ->
      Koka_zero.Runtime_error.string_of_t error
      |> limit_length ~limit:1000
      |> eprintf "runtime error: %s\n"
    | Ok _unit -> ())
;;

let command_compile =
  Command.basic
    ~summary:"compile a program"
    [%map_open.Command
      let filename = anon ("filename" %: string)
      and out_filename = flag "-o" (required string) ~doc:"output filename"
      and print_eps =
        flag
          "-dump-eps"
          no_arg
          ~doc:"print the intermediate evidence passing AST"
      in
      fun () -> compile ~in_filename:filename ~print_eps ~out_filename]
;;

let command_interpret =
  Command.basic
    ~summary:"interpret a program"
    Command.Param.(
      map
        (anon ("filename" %: string))
        ~f:(fun filename () -> interpret_eps filename))
;;

let command =
  Command.group
    ~summary:"Koka compiler"
    [ "compile", command_compile; "interpret", command_interpret ]
;;

let () = Command.run command
