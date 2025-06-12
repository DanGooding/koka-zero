open! Core
open! Import

val bool : Llvm.lltype Codegen.t
val int : Llvm.lltype Codegen.t
val pointer : Llvm.lltype Codegen.t

(** prompt marker *)
val marker : Llvm.lltype Codegen.t

(** effect label *)
val label : Llvm.lltype Codegen.t

(** type of the tag used to distinguish variants *)
val variant_tag : Llvm.lltype Codegen.t

(** control monad 'yield' contents:
    [{ opaque_pointer marker; op_clause; resumption }] *)
val ctl_yield : Llvm.lltype Codegen.t

(** operation clause variant, but merged into one types as `normal` and `tail`
    have identical fields: [{ tag; opaque_pointer clause }]*)
val op : Llvm.lltype Codegen.t

(** a closure holds free variables in a chain starting from the innermost
    function. The structure is as follows:
    [{ i64 num_vars; opaque_pointer *vars; opaque_pointer parent }]. The
    nil/empty closure is represented as a null pointer. *)
val closure : Llvm.lltype Codegen.t

(** A function object holds a code address, a flag indicating whether it is
    recursive, and a closure:
    [{ opaque_pointer code; closure *closure; i1 is_recursive }] *)
val function_object : Llvm.lltype Codegen.t

(** type of the binary's entry point: [i32 main()] *)
val main_function : Llvm.lltype Codegen.t
