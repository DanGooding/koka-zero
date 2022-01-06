open Name_source_intf

(** A [Name_source.t] allows generating unique names, from sequential (prefixed)
    strings. The source is pure, suitable for wrapping in a state monad *)
module type S = S

(** functor building a [Name_source] from a type with an [of_generated_name]
    construction function. See [Multiset] for usage documentation *)
module Make (Name : Name_S) : S with type Name.t := Name.t
