open Core
open Has_multiset_intf

(** a multiset is a set which allows duplicates - tracking frequencies of each
    object *)
module type S = S

(** functor building a Multiset + Non_empty_multiset implementation on a
    comparable and sexpable type.

    usage:

    {[
      module Thing : sig
        type t [@@deriving compare, sexp]

        include Comparable.S with type t := t

        include Has_multiset.S with type t := t
      end
    ]}
    {[
      module Thing = struct
        module T = struct
          ...
        end
        include T
        include Comparable.Make (T)

        include Has_multiset.Make (T)
      end
    ]} *)
module Make (Element : sig
    type t [@@deriving sexp]

    include Comparable.S with type t := t
  end) : S with type t := Element.t
