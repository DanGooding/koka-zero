open Import

module Parameters : sig
  (** maps the current function's parameters to their [llvalues] *)
  type t = (Variable.t * Llvm.llvalue) list
  (* TODO: does this contain the extraneous `self` parameter for non recursive
     functions? *)

  val find : t -> Variable.t -> Llvm.llvalue option
end

module Closure : sig
  module Shape : sig
    type t =
      | Level of Variable.t list * t
      | Empty
    [@@deriving sexp]
  end

  (** a runtime closure [llvalue] and its statically known contents *)
  type t =
    { closure : Llvm.llvalue (** of type [closure *] *)
    ; shape : Shape.t
    }

  (** cons parameters on the front, producing a child closure *)
  val extend : t -> Parameters.t -> runtime:Runtime.t -> t Codegen.t

  (** generate code to retrieve a varaible from a closure, failing with a
      codegen_eror if it is not present. *)
  val compile_get : t -> Variable.t -> Llvm.llvalue Codegen.t
end

(** maps in-scope names to their [llvalues] *)
type t =
  { parameters : Parameters.t
  ; closure : Closure.t
  }

(** generate code to retrieve an in-scope variable, either directly from
    [parameters], or indirectly in the [closure]. fails with a codegen_error if
    it is not in scope. *)
val compile_get : t -> Variable.t -> Llvm.llvalue Codegen.t
