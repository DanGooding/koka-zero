open Core
open Multiset_intf

(** a multiset is a set which allows duplicates - tracking frequencies of each
    object *)
module type S = S

module Make (Element : sig
  type t [@@deriving sexp]

  include Comparable.S with type t := t
end) : S with type Multiset.Element.t := Element.t
