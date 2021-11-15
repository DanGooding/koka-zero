open Core

module Kind = struct
  type t = Syntax_error [@@deriving sexp]

  let string_of_t = function
    | Syntax_error -> "syntax_error"
  ;;
end

type t =
  { kind : Kind.t
  ; message : string
  ; location : Source_location.t option
  }
[@@deriving sexp]

let syntax_error ?at message =
  { kind = Kind.Syntax_error; message; location = at }
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
