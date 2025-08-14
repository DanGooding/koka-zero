open Core

module Kind = struct
  module T = struct
    type t =
      | Impossible_error
      (** type error which should have been caught at the type inference
          phase - essentially a nicer [assert_false] *)
      | Unsupported_feature_error (** features not yet implemented *)
      | Verifier_error (** error reported by llvm verifier *)
      | Write_error (** failure to write the .ll file *)
    [@@deriving sexp_of]
  end

  include T

  let to_string = function
    | Impossible_error -> "impossible error"
    | Unsupported_feature_error -> "unsupported feature"
    | Verifier_error -> "verifier error"
    | Write_error -> "write error"
  ;;
end

type t =
  { message : string
  ; kind : Kind.t
  }
[@@deriving sexp_of]

let impossible_error message = { kind = Kind.Impossible_error; message }

let unsupported_feature_error message =
  { kind = Kind.Unsupported_feature_error; message }
;;

let verifier_error message = { kind = Kind.Verifier_error; message }
let write_error message = { kind = Kind.Write_error; message }
let to_string { kind; message } = sprintf "%s: %s" (Kind.to_string kind) message

module Or_codegen_error = struct
  type nonrec 'a t = ('a, t) Result.t
end
