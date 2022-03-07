open Import

module Locals : sig
  (** maps the current function's locals and parameters to their [llvalues]. The
      first name is the innermost, so must search in forward order *)
  type t = (Variable.t * Llvm.llvalue) list

  val find : t -> Variable.t -> Llvm.llvalue option
  val add : t -> name:Variable.t -> value:Llvm.llvalue -> t
end

module Closure : sig
  module Shape : sig
    (** describes the statically known shape and contents of a closure *)
    type t =
      | Level of Variable.t list * t
          (** a list of all the variables added to the closure at one nesting
              depth, and the parent closure's shape *)
      | Toplevel of Variable.t list
          (** names in scope from the current enclosing toplevel function, in
              the _single_ toplevel closure. These are unique, so searching in
              either order is guaranteed to work *)
    [@@deriving sexp]
  end

  (** a runtime closure [llvalue] and its statically known contents *)
  type t =
    { closure : Llvm.llvalue (** of type [closure *] *)
    ; shape : Shape.t
    }

  (** produce code to cons locals on the front, producing a child closure *)
  val compile_extend : t -> Locals.t -> runtime:Runtime.t -> t Codegen.t

  (** produce code to construct a toplevel closure, given its contents *)
  val compile_make_toplevel
    :  (Variable.t * Llvm.llvalue) list
    -> runtime:Runtime.t
    -> t Codegen.t

  (** generate code to retrieve a varaible from a closure, failing with a
      codegen_eror if it is not present. *)
  val compile_get : t -> Variable.t -> Llvm.llvalue Codegen.t
end

(** maps in-scope names to their [llvalues] *)
type t =
  (* TODO: this, or [option]al parameters *)
  | Local of
      { locals : Locals.t
      ; closure : Closure.t
      }
  | Toplevel of Closure.t

(** generate code to extend the closure with the current parameters *)
val compile_capture : t -> runtime:Runtime.t -> Closure.t Codegen.t

(** generate code to retrieve an in-scope variable, either directly from
    [parameters], or indirectly in the [closure]. fails with a codegen_error if
    it is not in scope. *)
val compile_get : t -> Variable.t -> Llvm.llvalue Codegen.t

(** add another local binding to the context, failing if at [Toplevel] *)
val add_local_exn : t -> name:Variable.t -> value:Llvm.llvalue -> t
