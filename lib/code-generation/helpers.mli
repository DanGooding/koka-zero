open Import

(** convert a [Variable.t] to a string usable as a register name *)
val register_name_of_variable : Variable.t -> string

val const_int : int -> Llvm.llvalue Codegen.t
val const_false : Llvm.llvalue Codegen.t
val const_true : Llvm.llvalue Codegen.t
val const_bool : bool -> Llvm.llvalue Codegen.t

(* unit carries no information, so is only kept for uniformity *)
val const_unit : Llvm.llvalue Codegen.t

(** [const_tag i] generates the tag constant for branch [i] of a variant type *)
val const_tag : int -> Llvm.llvalue Codegen.t

val const_ctl_pure_tag : Llvm.llvalue Codegen.t
val const_ctl_yield_tag : Llvm.llvalue Codegen.t
val const_op_normal_tag : Llvm.llvalue Codegen.t
val const_op_tail_tag : Llvm.llvalue Codegen.t
val const_label : int -> Llvm.llvalue Codegen.t

(** converts a valid [Types.bool] (either [const_true]/[const_false]) into an
    [i1] usable in a branch. *)
val i1_of_bool : Llvm.llvalue -> Llvm.llvalue Codegen.t

(** converts an [i1] into a valid [Types.bool] **)
val bool_of_i1 : Llvm.llvalue -> Llvm.llvalue Codegen.t

(** builds a cast from [Types.opaque_pointer] to a [Types.int] *)
val cast_unboxed_int_of_opaque
  :  Llvm.llvalue
  -> string
  -> Llvm.llvalue Codegen.t

(** builds a cast from [Types.opaque_pointer] to a [Types.bool] *)
val cast_unboxed_bool_of_opaque
  :  Llvm.llvalue
  -> string
  -> Llvm.llvalue Codegen.t

(** builds an int to ptr cast from e.g. [Types.int] or [Types.bool] to
    [Types.opaque_pointer] *)
val cast_opaque_of_unboxed : Llvm.llvalue -> Llvm.llvalue Codegen.t

(** a constant [Types.opaque_pointer] from a constant e.g. [Types.int] or
    [Types.bool]*)
val const_cast_opaque_of_unboxed : Llvm.llvalue -> Llvm.llvalue Codegen.t

(** [heap_allocate t name ~runtime] generates code which allocates space on the
    heap for a [t], and returns a typed pointer to it (a [t*]). [name] is used
    as a stem for register names *)
val heap_allocate
  :  Llvm.lltype
  -> string
  -> runtime:Runtime.t
  -> Llvm.llvalue Codegen.t

(** [heap_store t v name ~runtime] generates code which: allocates heap memory
    for a [t], stores [v] there, and returns the pointer as a
    [Types.opaque_pointer] *)
val heap_store
  :  Llvm.lltype
  -> Llvm.llvalue
  -> string
  -> runtime:Runtime.t
  -> Llvm.llvalue Codegen.t

val heap_store_marker
  :  Llvm.llvalue
  -> runtime:Runtime.t
  -> Llvm.llvalue Codegen.t

val heap_store_label
  :  Llvm.llvalue
  -> runtime:Runtime.t
  -> Llvm.llvalue Codegen.t

(** [dereference v t name] dereferences an [opaque_pointer] [v] as a [t]. [name]
    is used as a stem for the intermediate and final value *)
val dereference
  :  Llvm.llvalue
  -> Llvm.lltype
  -> string
  -> Llvm.llvalue Codegen.t

val dereference_marker : Llvm.llvalue -> Llvm.llvalue Codegen.t
val dereference_label : Llvm.llvalue -> Llvm.llvalue Codegen.t

(** [compile_populate_struct p members] generates code to populate the struct
    pointed to by [p], with the field values (and tmp names) [members] *)
val compile_populate_struct
  :  Llvm.llvalue
  -> (Llvm.llvalue * string) list
  -> unit Codegen.t

(** [compile_populate_array p elements] generates code to populate the array
    pointed to by [p] (which must be an array pointer!), with the elements (and
    tmp names) [elements] *)
val compile_populate_array
  :  Llvm.llvalue
  -> (Llvm.llvalue * string) list
  -> unit Codegen.t

(** [compile_access_field p index name] generates code to load field [index]
    from the struct pointed to by [p] *)
val compile_access_field
  :  Llvm.llvalue
  -> int
  -> string
  -> Llvm.llvalue Codegen.t
