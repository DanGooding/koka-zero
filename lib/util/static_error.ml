open Core

module Kind = struct
  type t =
    | Syntax_error
    | Unsupported_feature
    | Type_error
  [@@deriving sexp_of]

  let string_of_t = function
    | Syntax_error -> "syntax error"
    | Unsupported_feature -> "unsupported feature"
    | Type_error -> "type error"
  ;;
end

type t =
  { kind : Kind.t
  ; error : Error.t
  ; location : Source_location.t option
  }
[@@deriving sexp_of]

let raise { kind; error; location } =
  let kind = Kind.string_of_t kind in
  raise_s
    [%message kind (error : Error.t) (location : Source_location.t option)]
;;

module Or_static_error = struct
  type nonrec 'a t = ('a, t) Result.t [@@deriving sexp_of]

  let ok_exn t =
    match t with
    | Ok x -> x
    | Error err -> raise err
  ;;
end

let error_of_kind ?at kind error = { kind; error; location = at }

let syntax_error ?at message =
  error_of_kind ?at Syntax_error (Error.of_string message)
;;

let syntax_error_s ?at sexp =
  error_of_kind ?at Syntax_error (Error.create_s sexp)
;;

let unsupported_feature ?at message =
  error_of_kind ?at Kind.Unsupported_feature (Error.of_string message)
;;

let type_error_s ?at sexp = error_of_kind ?at Type_error (Error.create_s sexp)
let type_error_of_error ?at error = error_of_kind ?at Type_error error

let type_error ?at message =
  error_of_kind ?at Type_error (Error.of_string message)
;;

let type_errorf ?at format = Printf.ksprintf (type_error ?at) format
let tag_s t ~tag = { t with error = Error.tag_s t.error ~tag }

let string_of_t t =
  let { kind; error; location } = t in
  let location_part =
    match location with
    | Some location -> Source_location.string_of_t location ^ " "
    | None -> ""
  in
  let description_part =
    let kind = Kind.string_of_t kind in
    sprintf "%s: %s" kind (Error.to_string_hum error)
  in
  location_part ^ description_part
;;

(* TODO move into [Monad_utils.S2] *)
let all_option = function
  | None -> Result.Ok None
  | Some (Result.Ok x) -> Result.Ok (Some x)
  | Some (Result.Error err) -> Result.Error err
;;

let all_non_empty (Non_empty_list.Cons (t, ts)) =
  let open Result.Let_syntax in
  let%bind x = t in
  let%map xs = Result.all ts in
  Non_empty_list.Cons (x, xs)
;;

let list_fold_right xs ~init ~f =
  let open Result.Let_syntax in
  List.fold_right xs ~init:(return init) ~f:(fun x acc ->
    let%bind acc = acc in
    f x acc)
;;
