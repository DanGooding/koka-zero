open Core

module Kind = struct
  module T = struct
    type t =
      | Impossible_error
      (** type errors should be impossible after the type inference phase *)
      | Unsupported_feature_error (** features not yet implemented *)
      | IO_error (** errors performing built-in io operations *)
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T

  let to_string = function
    | Impossible_error -> "impossible error"
    | Unsupported_feature_error -> "unsupported feature"
    | IO_error -> "i/o error"
  ;;
end

type t =
  { kind : Kind.t
  ; message : string
  }
[@@deriving sexp]

let impossible_error message = { kind = Kind.Impossible_error; message }

let unsupported_feature_error message =
  { kind = Kind.Impossible_error; message }
;;

let io_error message = { kind = Kind.IO_error; message }
let to_string { kind; message } = sprintf "%s: %s" (Kind.to_string kind) message

module Or_runtime_error = struct
  type nonrec 'a t = ('a, t) Result.t
end
