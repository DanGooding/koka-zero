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
    (** describes the statically known contents of a closure.
        must be non-empty. *)
    type t [@@deriving sexp_of]
  end

  (** a runtime closure [llvalue] and its statically known contents *)
  type t =
    { closure : Llvm.llvalue (** of type [ptr], potentially [null] *)
    ; shape : Shape.t
    }

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
  ; closure : Closure.t option
  ; toplevel : Toplevel.t
  }
(* lookups are done in order: locals, closure, toplevel *)

val create_toplevel : Toplevel.t -> t
val get_captured : t -> free:Variable.Set.t -> Closure.Shape.t option

(** generate code to create a closure with the subset of locals which are free
    (escaping variables). may return a null closure if all free are in toplevel.
*)
val compile_capture
  :  t
  -> captured_shape:Closure.Shape.t
  -> code_address:Llvm.llvalue
  -> runtime:Runtime.t
  -> Closure.t Codegen.t

(** generate code to retrieve an in-scope variable, either directly from
    [parameters], or indirectly in the [closure]. fails with a codegen_error if
    it is not in scope. *)
val compile_get : t -> Variable.t -> Ctl_repr.t Codegen.t

(** add another local binding to the context *)
val add_local_exn : t -> name:Variable.t -> value:Ctl_repr.t -> t
