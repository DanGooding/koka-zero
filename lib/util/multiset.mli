open Core
open Multiset_intf

(** a multiset is a set which allows duplicates - tracking frequencies of each
    object *)
module type S = S

(** functor building a multiset implementation on a comparable and sexpable
    type.

    usage:

    {[
      module Thing : sig
        type t [@@deriving compare, sexp] (* TODO: is bin_io + hashable required too? *)

        include Comparable.S with type t := t

        module Multiset : sig
          include Multiset.S with type Element.t := T.t
        end
      end
    ]}
    {[
      module Thing = struct
        module T = struct
          ...
        end
        include T
        include Comparable.Make (T)

        module Multiset = Multiset.Make (T)
      end
    ]} *)
module Make (Element : sig
  type t [@@deriving sexp]

  include Comparable.S with type t := t
end) : S with type Element.t := Element.t
