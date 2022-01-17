(** pointer to an arbitrary runtime value. Not related to in-development LLVM
    opaque pointers. *)
val opaque_pointer : Llvm.lltype Codegen.t

val bool : Llvm.lltype Codegen.t
val int : Llvm.lltype Codegen.t
val unit : Llvm.lltype Codegen.t

(** type of the tag used to distinguish variants *)
val variant_tag : Llvm.lltype Codegen.t

(** control monad variant, with unknown contents. This can be cast to [ctl_pure]
    or [ctl_yield] depending on tag. Only pointers to this type should be
    allocated - A [ctl_pure] is smaller so may get aligned wrongly to be cast to
    a [ctl] *)
val ctl : Llvm.lltype Codegen.t

(** control monad 'pure' variant: [Pure { tag; value }] *)
val ctl_pure : Llvm.lltype Codegen.t

(** control monad 'yield' variant:
    [Yield { tag; marker; op_clause; resumption }] *)
val ctl_yield : Llvm.lltype Codegen.t

(** prompt marker *)
val marker : Llvm.lltype Codegen.t

(** effect label *)
val label : Llvm.lltype Codegen.t
