open Import

(** type describing the runtime representation of a specific effect label's and
    its handlers *)
type t =
  { id : int
        (** value used as the rumtime representation of this effect label *)
  ; hnd_type : Llvm.lltype
        (** struct representing a handler - holds one clause per operation *)
  ; operation_indices : int Variable.Map.t
        (** field indices of operations in the [hnd_type] struct *)
  }
