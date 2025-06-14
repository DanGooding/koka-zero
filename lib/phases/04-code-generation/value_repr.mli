open! Core
open! Import

module Packed : sig
  (** a value of the single uniform polymorphic representation. *)
  type t = Llvm.llvalue
end

module Unpacked : sig
  module Function : sig
    type t =
      | Code_pointer of Llvm.llvalue
      | Closure of Llvm.llvalue
    [@@deriving sexp_of]

    val pack : t -> Packed.t Codegen.t

    val unpack
      :  Packed.t
      -> f:(t -> 'a Codegen.t)
      -> phi_builder:'a Control_flow.Phi_builder.t
      -> 'a Codegen.t
  end
end

module Lazily_packed : sig
  type t =
    | Function of Unpacked.Function.t
    | Packed of Packed.t
  [@@deriving sexp_of]
  (* It _may_ make sense to add varaints to this for Int and Bool, but
     the optimiser may be good enough to remove unpack(pack(...)) for those.
     In particular, expect it cannot remove ptr_of_int(int_of_ptr(...)), but
     it is allowed to remove int_of_ptr(ptr_of_int(...)) *)

  val pack : t -> Packed.t Codegen.t

  (* generates code to unpack a packed value, or directly uses a lazily-packed value *)
  val unpack_function
    :  t
    -> f:(Unpacked.Function.t -> 'a Codegen.t)
    -> phi_builder:'a Control_flow.Phi_builder.t
    -> 'a Codegen.t
end
