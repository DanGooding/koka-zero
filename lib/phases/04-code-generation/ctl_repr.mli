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
    :  marker:Immediate_repr.Marker.t
    -> op_clause:Llvm.llvalue
    -> resumption:Llvm.llvalue
    -> runtime:Runtime.t
    -> t Codegen.t
end

(** This type represents either a value which is statically known to be pure [Pure],
    or the result of an effectful computation [Ctl], which may be it's final value,
    or a Yield if it performed some operation while being evaluated. *)
type t =
  | Pure of Value_repr.Lazily_packed.t
  | Ctl of Maybe_yield_repr.t

(* fail with 'impossible error' if the expected variant isn't returned *)
val pure : t -> Value_repr.Lazily_packed.t Codegen.t
val pure_exn : t -> Value_repr.Lazily_packed.t
val ctl : t -> Maybe_yield_repr.t Codegen.t
val type_ : t -> Evidence_passing_syntax.Type.t
val phi_builder : t Control_flow.Phi_builder.t
