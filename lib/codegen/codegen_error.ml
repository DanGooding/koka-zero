open Core

type t = string [@@deriving sexp]

module Or_codegen_error = struct
  type nonrec 'a t = ('a, t) Result.t
end
