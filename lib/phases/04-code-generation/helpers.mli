open! Core
open! Import

(** convert a [Variable.t] to a string usable as a register name *)
val register_name_of_variable : Variable.t -> string

(** [const_tag i] generates the tag constant for branch [i] of a variant type *)
val const_tag : int -> Llvm.llvalue Codegen.t

val const_op_normal_tag : Llvm.llvalue Codegen.t
val const_op_tail_tag : Llvm.llvalue Codegen.t
val const_label : int -> Llvm.llvalue Codegen.t

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
  :  struct_type:Llvm.lltype
  -> Llvm.llvalue
  -> (Llvm.llvalue * string) list
  -> unit Codegen.t

(** [compile_populate_array p elements] generates code to populate the array
    pointed to by [p] (which must be an array pointer!), with the elements (and
    tmp names) [elements] *)
val compile_populate_array
  :  array_type:Llvm.lltype
  -> Llvm.llvalue
  -> (Llvm.llvalue * string) list
  -> unit Codegen.t

(** [compile_access_field p index name] generates code to load field [index]
    from the struct pointed to by [p] *)
val compile_access_field
  :  Llvm.llvalue
  -> struct_type:Llvm.lltype
  -> i:int
  -> string
  -> Llvm.llvalue Codegen.t
