(** pointer to an arbitrary runtime value *)
val value_pointer : Llvm.lltype Codegen.t

val bool : Llvm.lltype Codegen.t
val int : Llvm.lltype Codegen.t
val unit : Llvm.lltype Codegen.t
