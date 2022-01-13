module type S = sig
  (** the element type *)
  type t

  module Multiset : sig
    type element := t
    type t [@@deriving sexp]

    val of_list : element list -> t
    val to_list : t -> element list
    val empty : t
    val add : t -> element -> t
    val union : t -> t -> t

    (** intersection: if an element appears `m` times in one multiset and `n`
        times in the other, then it appears `min m n` in their intersection *)
    val inter : t -> t -> t

    (** difference: if an element appears `m` times in the first argument, and
        `n` times in the second, then it appears `max (m - n) 0` times in the

        result *)
    val diff : t -> t -> t

    val is_empty : t -> bool

    module Non_empty : sig
      type multiset := t

      (** multiset which is guaranteed to have at least one element *)
      type t [@@deriving sexp]

      val of_multiset : multiset -> t option
      val of_multiset_exn : multiset -> t
      val of_multiset_verbose : multiset -> [ `Empty | `Non_empty of t ]
      val of_list : element list -> t option
      val of_list_exn : element list -> t
      val of_non_empty_list : element Non_empty_list.t -> t
      val to_list : t -> element list
      val to_non_empty_list : t -> element Non_empty_list.t
      val to_multiset : t -> multiset
      val add : t -> element -> t
      val union : t -> t -> t
      val union_mixed : t -> multiset -> t
      val inter_to_multiset : t -> t -> multiset
      val diff_to_multiset : t -> t -> multiset
      val inter : t -> t -> [ `Empty | `Non_empty of t ]
      val diff : t -> t -> [ `Empty | `Non_empty of t ]
      val inter_opt : t -> t -> t option
      val diff_opt : t -> t -> t option
    end
  end
end
