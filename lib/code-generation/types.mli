val bool : Llvm.lltype Codegen.t
val int : Llvm.lltype Codegen.t
val unit : Llvm.lltype Codegen.t
val pointer : Llvm.lltype Codegen.t

(** prompt marker *)
val marker : Llvm.lltype Codegen.t

(** effect label *)
val label : Llvm.lltype Codegen.t

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
    [Yield { tag; opaque_pointer marker; op_clause; resumption }] *)
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

(** an llvm function type, representing the generated function for a lambda with
    [n] arguments. This will be an llvm function of [n + 2] arguments, it also
    requires a pointer to the function object itself for recursion, and a
    closure. [null] should be passed for the former if it is not a recursive
    function. The type is:
    [ptr (
        function_object *f_self,
        closure *closure,
        ptr arg_1, ... ptr arg_n)
    ] *)
val function_code : int -> Llvm.lltype Codegen.t

(** type of the binary's entry point: [i32 main()] *)
val main_function : Llvm.lltype Codegen.t
