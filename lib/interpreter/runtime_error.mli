type t = { message : string } [@@deriving sexp]

module Or_runtime_error : sig
  type nonrec 'a t = ('a, t) Result.t
end
