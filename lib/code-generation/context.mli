open Import

module Parameters : sig
  (** maps the current function's parameters to their [llvalues] *)
  type t = (Variable.t * Llvm.llvalue) list

  val find : t -> Variable.t -> Llvm.llvalue option
end

module Closure : sig
  module Shape : sig
    (** describes the statically known shape and contents of a closure *)
    type t =
      | Level of Variable.t list * t
          (** a list of all the variables added to the closure at one nesting
              depth, and the parent closure's shape *)
      | Toplevel of int Variable.Map.t
          (** a map from names in scope from the current enclosing toplevel
              function, to their indices in the _single_ toplevel closure. This
              is is map rather than a list due to potential shadowing *)
    [@@deriving sexp]
  end

  (** a runtime closure [llvalue] and its statically known contents *)
  type t =
    { closure : Llvm.llvalue (** of type [closure *] *)
    ; shape : Shape.t
    }

  (** produce code to cons parameters on the front, producing a child closure *)
  val compile_extend : t -> Parameters.t -> runtime:Runtime.t -> t Codegen.t

  (** generate code to retrieve a varaible from a closure, failing with a
      codegen_eror if it is not present. *)
  val compile_get : t -> Variable.t -> Llvm.llvalue Codegen.t
end

(** maps in-scope names to their [llvalues] *)
type t =
  { parameters : Parameters.t
  ; closure : Closure.t
  }

(** generate code to extend the closure with the current parameters *)
val compile_make_closure : t -> runtime:Runtime.t -> Closure.t Codegen.t

(** generate code to retrieve an in-scope variable, either directly from
    [parameters], or indirectly in the [closure]. fails with a codegen_error if
    it is not in scope. *)
val compile_get : t -> Variable.t -> Llvm.llvalue Codegen.t
