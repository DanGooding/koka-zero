open! Core
open Import

module Locals : sig
  (** maps the current function's locals and parameters to their [llvalues]. The
      first name is the innermost, so must search in forward order *)
  type t = (Variable.t * Ctl_repr.t) list
end

module Return_value_pointer : sig
  type t =
    | Pure
    | Ctl of { is_yield_i1_pointer : Llvm.llvalue }

  val compile_return : t -> Ctl_repr.t -> unit Codegen.t
end

module Closure : sig
  module Shape : sig
    (** describes the statically known shape and contents of a closure.
        the outer list stores the closure, then it's parent, then grandparent etc.
        the inner list stores the list of variables at one level, in order *)
    type t = Variable.t list list [@@deriving sexp_of]

    val empty : t
  end

  (** a runtime closure [llvalue] and its statically known contents *)
  type t =
    { closure : Llvm.llvalue (** of type [ptr], potentially [null] *)
    ; shape : Shape.t
    }

  val empty : t Codegen.t
  val is_empty : t -> bool

  (** generate code to retrieve a varaible from a closure, failing with a
      codegen_eror if it is not present. *)
  val compile_get : t -> Variable.t -> Llvm.llvalue Codegen.t
end

module Toplevel : sig
  (** the code pointer for each toplevel function *)
  type t

  val of_ordered_alist : (Variable.t * Function_repr.Callable.t) list -> t
end

(** maps in-scope names to their [llvalues] *)
type t =
  { locals : Locals.t
  ; return_value_pointer : Return_value_pointer.t
  ; closure : Closure.t
  ; toplevel : Toplevel.t
  }
(* lookups are done in order: locals, closure, toplevel *)

val create_toplevel : Toplevel.t -> t Codegen.t

(** generate code to create or extend the closure with the subset of locals which are free
    (escaping variables). may return a null closure if all free are in toplevel.

    note this does not check that non-local free variables are actually present in the closure.
*)
val compile_capture
  :  t
  -> free:Variable.Set.t
  -> runtime:Runtime.t
  -> Closure.t Codegen.t

(** generate code to retrieve an in-scope variable, either directly from
    [parameters], or indirectly in the [closure]. fails with a codegen_error if
    it is not in scope. *)
val compile_get : t -> Variable.t -> Ctl_repr.t Codegen.t

(** add another local binding to the context *)
val add_local_exn : t -> name:Variable.t -> value:Ctl_repr.t -> t
