open! Core
open! Import

module type Arg_S = sig
  type t

  module Field : sig
    type struct_ := t
    type t [@@deriving equal]

    val name : t -> string
    val type_ : t -> Llvm.lltype Codegen.t
    val all : struct_ -> t list
  end
end

module type Struct_S = sig
  module Field : T

  type t

  val type_ : t -> Llvm.lltype Codegen.t

  val populate
    :  t
    -> Llvm.llvalue
    -> f:(Field.t -> Llvm.llvalue)
    -> unit Codegen.t

  val project
    :  ?name:string
    -> t
    -> Llvm.llvalue
    -> Field.t
    -> Llvm.llvalue Codegen.t

  val heap_allocate
    :  t
    -> name:string
    -> runtime:Runtime.t
    -> Llvm.llvalue Codegen.t
end

module type S = sig
  module type Struct_S = Struct_S

  module Make (Arg : Arg_S) :
    Struct_S with module Field := Arg.Field and type t := Arg.t
end
