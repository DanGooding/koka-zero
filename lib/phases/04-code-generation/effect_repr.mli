open Import

(** type describing the runtime representation of a specific effect label's and
    its handlers *)
type t =
  { id : int
    (** value used as the rumtime representation of this effect label *)
  ; operations : Variable.t list
    (** the operations of this effect, in the order they appear as fields in
        the [hnd_type] struct *)
  }
