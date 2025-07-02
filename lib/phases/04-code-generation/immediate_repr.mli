open! Core
open! Import

module Bool : sig
  (** an llvalue of type [Types.bool] *)
  type t

  val const_false : unit -> t Codegen.t
  val const_true : unit -> t Codegen.t
  val const_bool : bool -> t Codegen.t
  val of_bool_llvalue : Llvm.llvalue -> t
  val to_opaque : t -> Llvm.llvalue Codegen.t
  val of_opaque : Llvm.llvalue -> t Codegen.t
  val of_i1 : Llvm.llvalue -> t Codegen.t
  val to_i1 : t -> Llvm.llvalue Codegen.t

  val compile_binary_operation
    :  t
    -> Operator.Bool.t
    -> t
    -> Llvm.llvalue Codegen.t

  val compile_unary_operation
    :  Operator.Bool.Unary.t
    -> t
    -> Llvm.llvalue Codegen.t
end

module Int : sig
  (** an llvalue value of type [Types.int] *)
  type t

  val const : int -> t Codegen.t
  val of_int_llvalue : Llvm.llvalue -> t
  val to_int_llvalue : t -> Llvm.llvalue
  val to_opaque : t -> Llvm.llvalue Codegen.t
  val of_opaque : Llvm.llvalue -> t Codegen.t

  val compile_binary_operation
    :  t
    -> Operator.Int.t
    -> t
    -> Llvm.llvalue Codegen.t
end

module Unit : sig
  (* unit carries no information, so is only kept for uniformity *)
  val const_opaque : unit -> Llvm.llvalue Codegen.t
end

module Marker : sig
  (** an llvalue value of type [Types.marker] *)
  type t

  val of_marker_llvalue : Llvm.llvalue -> t
  val to_marker_llvalue : t -> Llvm.llvalue
  val to_opaque : t -> Llvm.llvalue Codegen.t
  val of_opaque : Llvm.llvalue -> t Codegen.t
  val compile_equal : t -> t -> Bool.t Codegen.t
end

module Label : sig
  (** an llvalue value of type [Types.label] *)
  type t

  val of_const_int : int -> t Codegen.t
  val to_label_llvalue : t -> Llvm.llvalue
  val to_opaque : t -> Llvm.llvalue Codegen.t
  val of_opaque : Llvm.llvalue -> t Codegen.t
end
