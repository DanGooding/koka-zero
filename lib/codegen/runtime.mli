(** declarations of runtime's functions *)
type t = { malloc : Llvm.llvalue }

(** build declarations for all functions *)
val declare : t Codegen.t
