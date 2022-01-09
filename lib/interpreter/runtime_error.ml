open Core

type t = { message : string } [@@deriving sexp]

module Or_runtime_error = struct
  type nonrec 'a t = ('a, t) Result.t
end
