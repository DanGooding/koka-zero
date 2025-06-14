open! Core
open! Import

module Maybe_yield_repr : sig
  (** a flattened representation of the variant:
      [Pure of content | Yield of content] *)
  type t

  val create : is_yield_i1:Llvm.llvalue -> content:Llvm.llvalue -> t
  val get_is_yield_i1 : t -> Llvm.llvalue Codegen.t
  val get_content : t -> Llvm.llvalue Codegen.t
  val compile_construct_pure : Llvm.llvalue -> t Codegen.t

  val compile_construct_yield
    :  marker:Llvm.llvalue
    -> op_clause:Llvm.llvalue
    -> resumption:Llvm.llvalue
    -> runtime:Runtime.t
    -> t Codegen.t
end

type t =
  | Pure of Llvm.llvalue
  | Ctl of Maybe_yield_repr.t

(* fail with 'impossible error' if the expected variant isn't returned *)
val pure : t -> Llvm.llvalue Codegen.t
val pure_exn : t -> Llvm.llvalue
val ctl : t -> Maybe_yield_repr.t Codegen.t
val type_ : t -> Evidence_passing_syntax.Type.t
val phi_builder : t Control_flow.Phi_builder.t
