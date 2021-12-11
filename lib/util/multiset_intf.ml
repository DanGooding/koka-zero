module type S = sig
  (* TODO: this extra wrapper layer is counterintuitive, as is the syntax used
     for creation: [ include Multiset.S with type Multiset.Element.t := t ].
     Could add another module e.g. [Has_multiset] which fulfils the same role as
     [Comparable] does for map and set *)

  module Multiset : sig
    module Element : sig
      type t [@@deriving compare, sexp]
    end

    type t [@@deriving sexp]

    val of_list : Element.t list -> t
    val to_list : t -> Element.t list
    val empty : t
    val add : t -> Element.t -> t
    val union : t -> t -> t

    (** intersection: if an element appears `m` times in one multiset and `n`
        times in the other, then it appears `min m n` in their intersection *)
    val inter : t -> t -> t

    (** difference: if an element appears `m` times in the first argument, and
        `n` times in the second, then it appears `max (m - n) 0` times in the
        result *)
    val diff : t -> t -> t

    val is_empty : t -> bool
  end
end
