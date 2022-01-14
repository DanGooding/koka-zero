(** pointer to an arbitrary runtime value. Not related to in-development LLVM
    opaque pointers. *)
val opaque_pointer : Llvm.lltype Codegen.t

val bool : Llvm.lltype Codegen.t
val int : Llvm.lltype Codegen.t
val unit : Llvm.lltype Codegen.t
