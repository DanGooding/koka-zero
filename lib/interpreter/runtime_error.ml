open Core

module Kind = struct
  module T = struct
    type t =
      | Impossible_error
          (** type errors should be impossible after the type inference phase *)
    [@@deriving sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T

  let string_of_t = function
    | Impossible_error -> "impossible error"
  ;;
end

type t =
  { kind : Kind.t
  ; message : string
  }
[@@deriving sexp]

let impossible_error message = { kind = Kind.Impossible_error; message }

let string_of_t { kind; message } =
  sprintf "%s: %s" (Kind.string_of_t kind) message
;;

module Or_runtime_error = struct
  type nonrec 'a t = ('a, t) Result.t
end
