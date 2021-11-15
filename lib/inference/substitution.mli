type t

val identity : t
val compose : t -> t -> t (* option? *)

(* TODO: substitutable probably needs to be a 'friend' of this? *)
val extend : t -> ? -> ? -> t
val apply : t -> ? -> t
