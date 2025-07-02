open! Core
open! Import

module type Arg_S = sig
  module Field : sig
    type t [@@deriving equal]

    val name : t -> string
    val type_ : t -> Llvm.lltype Codegen.t
    val all : t list
  end
end

module type Struct_S = sig
  module Field : T

  val type_ : Llvm.lltype Codegen.t
  val populate : Llvm.llvalue -> f:(Field.t -> Llvm.llvalue) -> unit Codegen.t

  val project
    :  ?name:string
    -> Llvm.llvalue
    -> Field.t
    -> Llvm.llvalue Codegen.t

  val heap_allocate : name:string -> runtime:Runtime.t -> Llvm.llvalue Codegen.t
end

module type S = sig
  module type Struct_S = Struct_S

  module Make (Arg : Arg_S) : Struct_S with module Field := Arg.Field
end
