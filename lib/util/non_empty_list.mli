(** a list with at least one element *)
type 'a t = Cons of 'a * 'a list [@@deriving sexp]

val of_list : 'a list -> 'a t option
val of_list_exn : 'a list -> 'a t
val to_list : 'a t -> 'a list
val map : 'a t -> f:('a -> 'b) -> 'b t
