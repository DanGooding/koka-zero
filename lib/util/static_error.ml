open Core

module Kind = struct
  module T = struct
    type t =
      | Syntax_error
      | Unsupported_feature
      | Type_error
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  (* submodule used to limit the scope for which the above warning is
     disabled *)
  include T

  let string_of_t = function
    | Syntax_error -> "syntax error"
    | Unsupported_feature -> "unsupported feature"
    | Type_error -> "type error"
  ;;
end

type t =
  { kind : Kind.t
  ; message : string
  ; location : Source_location.t option
  }
[@@deriving sexp]

let raise { kind; message; location } =
  let kind = Kind.string_of_t kind in
  raise_s [%message kind message (location : Source_location.t option)]
;;

module Or_static_error = struct
  type nonrec 'a t = ('a, t) Result.t [@@deriving sexp]

  let ok_exn t =
    match t with
    | Ok x -> x
    | Error err -> raise err
  ;;
end

let error_of_kind kind ?at message = { kind; message; location = at }
let syntax_error = error_of_kind Kind.Syntax_error
let unsupported_feature = error_of_kind Kind.Unsupported_feature
let type_error = error_of_kind Kind.Type_error

let type_error_of_error error =
  let message = Error.to_string_hum error in
  type_error message
;;

let string_of_t t =
  let { kind; message; location } = t in
  let location_part =
    match location with
    | Some location -> Source_location.string_of_t location ^ " "
    | None -> ""
  in
  let description_part =
    let kind = Kind.string_of_t kind in
    sprintf "%s: %s" kind message
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
