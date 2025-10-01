open! Core
open! Import

module State = struct
  type t =
    { stack : Sexp.t list
    ; indent : int
    ; when_to_print : [ `every_enter | `only_stack ]
    }
  [@@deriving sexp_of]

  let create () ~when_to_print = { stack = []; indent = 4; when_to_print }

  let line_prefix ~depth ~indent ~is_first_line =
    let prefix_length = depth * indent in
    if is_first_line
    then
      String.init prefix_length ~f:(fun i ->
        if i = prefix_length - indent then '*' else ' ')
    else String.init prefix_length ~f:(fun _ -> ' ')
  ;;

  let print_info summary ~depth ~indent ~is_entry =
    let summary_lines = Sexp.to_string_hum summary |> String.split_lines in
    let summary_lines =
      List.mapi summary_lines ~f:(fun i line ->
        let prefix =
          line_prefix ~depth ~indent ~is_first_line:(i = 0 && is_entry)
        in
        prefix ^ line)
    in
    List.iter summary_lines ~f:print_endline
  ;;

  let enter t sexp_info =
    let summary = sexp_info () in
    (match t.when_to_print with
     | `only_stack -> ()
     | `every_enter ->
       print_info
         summary
         ~depth:(List.length t.stack)
         ~indent:t.indent
         ~is_entry:true);
    { t with stack = summary :: t.stack }
  ;;

  let log t sexp_info =
    match t.when_to_print with
    | `only_stack -> ()
    | `every_enter ->
      print_info
        (sexp_info ())
        ~depth:(List.length t.stack)
        ~indent:t.indent
        ~is_entry:false
  ;;

  let print_stack t =
    let frames = List.rev t.stack in
    List.iteri frames ~f:(fun depth summary ->
      print_info summary ~depth ~indent:t.indent ~is_entry:true)
  ;;

  let%expect_test "indents based on depth" =
    let g y ~t =
      let t = enter t (fun () -> [%message "g" (y : int)]) in
      log t (fun () -> [%message "body of g" (y : int)]);
      if y = 3
      then (
        log t (fun () -> [%message "printing current stack"]);
        print_stack t;
        log t (fun () -> [%message "end of current stack"]))
    in
    let f x ~t =
      let t = enter t (fun () -> [%message "f" (x : int)]) in
      log t (fun () -> [%message "calling g twice"]);
      g 2 ~t;
      g 3 ~t;
      g 4 ~t
    in
    let t = create () ~when_to_print:`every_enter in
    f 1 ~t;
    [%expect
      {|
      (f (x 1))
          "calling g twice"
      *   (g (y 2))
              ("body of g" (y 2))
      *   (g (y 3))
              ("body of g" (y 3))
              "printing current stack"
      (f (x 1))
      *   (g (y 3))
              "end of current stack"
      *   (g (y 4))
              ("body of g" (y 4))
      |}]
  ;;
end

type t = State.t option [@@deriving sexp_of]

let create_enbled () ~when_to_print = Some (State.create () ~when_to_print)
let create_disabled () = None

let create_enabled_by_environment_variable var =
  match Sys.getenv var with
  | Some "only-stack" -> create_enbled () ~when_to_print:`only_stack
  | Some _ -> create_enbled () ~when_to_print:`every_enter
  | None -> create_disabled ()
;;

let enter t sexp_info =
  Option.map t ~f:(fun state -> State.enter state sexp_info)
;;

let log t sexp_info = Option.iter t ~f:(fun state -> State.log state sexp_info)
let print_stack t = Option.iter t ~f:State.print_stack
