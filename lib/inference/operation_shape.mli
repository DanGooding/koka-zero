(** the resumption behaviour of an effect operation *)
type t =
  | Control (** first class, multi-shot resumption *)
  | Fun (** implicit tail resumption *)
[@@deriving sexp]

(** [can_implement ~handler ~declaration] returns true if [handler] is no more
    powerful than [declaration].

    for the currrently supported shapes: [Fun < Control] *)
val can_implement : handler:t -> declaration:t -> bool
