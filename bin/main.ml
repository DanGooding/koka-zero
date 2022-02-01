open Core

(** print the given message to stderr and exit the process with a nonzero return
    code (failure) *)
let exit_with_error_messsage message =
  eprintf "%s\n" message;
  exit 1
;;

let limit_length ~limit s = String.slice s 0 (min limit (String.length s))

let typecheck_and_compile_to_expl filename =
  let open Result.Let_syntax in
  let%bind program = In_channel.with_file filename ~f:Koka_zero.parse_channel in
  Koka_zero.infer_program program
;;

let compile_to_eps filename =
  let open Result.Let_syntax in
  let%map program_explicit = typecheck_and_compile_to_expl filename in
  Koka_zero.translate program_explicit
;;

let compile ~in_filename ~print_eps ~out_filename =
  match compile_to_eps in_filename with
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
  match compile_to_eps filename with
  | Error error ->
    Koka_zero.Static_error.string_of_t error |> exit_with_error_messsage
  | Ok program ->
    (match Koka_zero.interpret_program program with
    | Error error ->
      Koka_zero.Runtime_error.string_of_t error
      |> limit_length ~limit:1000
      |> sprintf "runtime error: %s\n"
      |> exit_with_error_messsage
    | Ok _unit -> ())
;;

let typecheck filename =
  match typecheck_and_compile_to_expl filename with
  | Error error ->
    Koka_zero.Static_error.string_of_t error |> exit_with_error_messsage
  | Ok _program -> ()
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

let command_typecheck =
  Command.basic
    ~summary:"type check a program"
    Command.Param.(
      map
        (anon ("filename" %: string))
        ~f:(fun filename () -> typecheck filename))
;;

let command =
  Command.group
    ~summary:"Koka compiler"
    [ "compile", command_compile
    ; "interpret", command_interpret
    ; "check", command_typecheck
    ]
;;

let () = Command.run command
