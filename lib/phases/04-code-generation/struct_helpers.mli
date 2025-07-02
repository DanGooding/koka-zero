open! Core
open! Import

(** [heap_allocate t name ~runtime] generates code which allocates space on the
    heap for a [t], and returns a typed pointer to it (a [t*]). [name] is used
    as a stem for register names *)
val heap_allocate
  :  Llvm.lltype
  -> string
  -> runtime:Runtime.t
  -> Llvm.llvalue Codegen.t

(** [compile_populate_struct p members] generates code to populate the struct
    pointed to by [p], with the field values (and tmp names) [members] *)
val compile_populate_struct
  :  struct_type:Llvm.lltype
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
