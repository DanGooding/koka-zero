open Core
module EPS = Koka_zero_evidence_translation.Evidence_passing_syntax

(* TODO: need a type system for telling when llvalues are
   pointers(opaque/typed)/values/*)

(* TODO: these are not heap allocated - make that obvious *)
let const_int : int -> Llvm.llvalue Codegen.t =
 fun i ->
  let open Codegen.Let_syntax in
  let%map int_type = Types.int in
  Llvm.const_int int_type i
;;

let const_false : Llvm.llvalue Codegen.t =
  let open Codegen.Let_syntax in
  let%map bool_type = Types.bool in
  (* all zeros *)
  Llvm.const_null bool_type
;;

let const_true : Llvm.llvalue Codegen.t =
  let open Codegen.Let_syntax in
  let%map bool_type = Types.bool in
  Llvm.const_int bool_type 1
;;

let const_bool : bool -> Llvm.llvalue Codegen.t =
 fun b -> if b then const_true else const_false
;;

(* unit carries no information, so is only kept for uniformity *)
let const_unit : Llvm.llvalue Codegen.t =
  let open Codegen.Let_syntax in
  let%map unit_type = Types.unit in
  Llvm.const_null unit_type
;;

(** [const_tag i] generates the tag constant for branch [i] of a variant type *)
let const_tag : int -> Llvm.llvalue Codegen.t =
 fun i ->
  let open Codegen.Let_syntax in
  let%map tag_type = Types.variant_tag in
  Llvm.const_int tag_type i
;;

let const_ctl_pure_tag : Llvm.llvalue Codegen.t = const_tag 0
let const_ctl_yield_tag : Llvm.llvalue Codegen.t = const_tag 1

(** [heap_allocate t ~runtime] generates code which allocates space on the heap
    for a [t], and returns a typed pointer to it (a [t*]) *)
let heap_allocate : Llvm.lltype -> runtime:Runtime.t -> Llvm.llvalue Codegen.t =
 fun t ~runtime ->
  let open Codegen.Let_syntax in
  let size = Llvm.size_of t in
  let { Runtime.malloc; _ } = runtime in
  let%bind heap_ptr =
    Codegen.use_builder
      (Llvm.build_call malloc (Array.of_list [ size ]) "heap_ptr")
  in
  let t_ptr_type = Llvm.pointer_type t in
  Codegen.use_builder (Llvm.build_bitcast heap_ptr t_ptr_type "typed_ptr")
;;

(** [heap_store t v ~runtime] generates code which: allocates heap memory for a
    [t], stores [v] there, and returns the pointer as a [Types.opaque_pointer] *)
let heap_store
    : Llvm.lltype -> Llvm.llvalue -> runtime:Runtime.t -> Llvm.llvalue Codegen.t
  =
 fun t v ~runtime ->
  let open Codegen.Let_syntax in
  let%bind t_ptr = heap_allocate t ~runtime in
  let%bind _store = Codegen.use_builder (Llvm.build_store v t_ptr) in
  let%bind opaque_pointer = Types.opaque_pointer in
  Codegen.use_builder (Llvm.build_bitcast t_ptr opaque_pointer "heap_ptr")
;;

(** [heap_store_int i ~runtime] allocates memory to hold a [Types.int] and
    stores [i] there. It returns the address as a [Types.opaque_pointer] *)
let heap_store_int : Llvm.llvalue -> runtime:Runtime.t -> Llvm.llvalue Codegen.t
  =
 fun i ~runtime ->
  let open Codegen.Let_syntax in
  let%bind type_int = Types.int in
  heap_store type_int i ~runtime
;;

(* TODO: could just allocate [true] and [false] at program start, then reuse
   those *)
let heap_store_bool
    : Llvm.llvalue -> runtime:Runtime.t -> Llvm.llvalue Codegen.t
  =
 fun b ~runtime ->
  let open Codegen.Let_syntax in
  let%bind type_bool = Types.bool in
  heap_store type_bool b ~runtime
;;

let heap_store_unit : runtime:Runtime.t -> Llvm.llvalue Codegen.t =
 fun ~runtime ->
  let open Codegen.Let_syntax in
  let%bind u = const_unit in
  let%bind type_unit = Types.unit in
  heap_store type_unit u ~runtime
;;

(** [dereference v t name] dereferences an [opaque_pointer] [v] as a [t]. [name]
    is used as a stem for the intermediate and final value *)
let dereference
    : Llvm.llvalue -> Llvm.lltype -> string -> Llvm.llvalue Codegen.t
  =
 fun ptr t name ->
  let open Codegen.Let_syntax in
  let t_ptr_type = Llvm.pointer_type t in
  let%bind t_ptr =
    Codegen.use_builder (Llvm.build_bitcast ptr t_ptr_type (name ^ "ptr"))
  in
  Codegen.use_builder (Llvm.build_load t_ptr name)
;;

(* TODO: factor out this duplication *)
let dereference_int : Llvm.llvalue -> Llvm.llvalue Codegen.t =
 fun ptr ->
  let open Codegen.Let_syntax in
  let%bind type_int = Types.int in
  dereference ptr type_int "int"
;;

let dereference_bool : Llvm.llvalue -> Llvm.llvalue Codegen.t =
 fun ptr ->
  let open Codegen.Let_syntax in
  let%bind type_bool = Types.bool in
  dereference ptr type_bool "bool"
;;

(** compile a literal into code which returns a pointer to a new heap allocation
    (as a [Types.opaque_pointer]) containing that literal *)
let compile_literal
    : EPS.Literal.t -> runtime:Runtime.t -> Llvm.llvalue Codegen.t
  =
 fun lit ~runtime ->
  let open Codegen.Let_syntax in
  match lit with
  | EPS.Literal.Int i ->
    let%bind v = const_int i in
    heap_store_int v ~runtime
  | EPS.Literal.Bool b ->
    let%bind v = const_bool b in
    heap_store_bool v ~runtime
  | EPS.Literal.Unit -> heap_store_unit ~runtime
;;

(** takes values which are [Types.opaque_pointer]s to the evaluated operands,
    and generates code to evaluate the operator and store the result on the
    heap. *)
let compile_binary_operator
    :  left:Llvm.llvalue -> EPS.Operator.t -> right:Llvm.llvalue
    -> runtime:Runtime.t -> Llvm.llvalue Codegen.t
  =
 fun ~left op ~right ~runtime ->
  let open Codegen.Let_syntax in
  match op with
  | EPS.Operator.Bool bool_op ->
    let%bind x = dereference_bool left in
    let%bind y = dereference_bool right in
    let%bind z =
      match bool_op with
      | EPS.Operator.Bool.And ->
        Codegen.use_builder (Llvm.build_and x y "bool_and")
      | EPS.Operator.Bool.Or ->
        Codegen.use_builder (Llvm.build_or x y "bool_or")
    in
    heap_store_bool z ~runtime
  | EPS.Operator.Int int_op ->
    let%bind x = dereference_int left in
    let%bind y = dereference_int right in
    let operation =
      match int_op with
      | EPS.Operator.Int.Plus -> `Int Llvm.build_add
      | EPS.Operator.Int.Minus -> `Int Llvm.build_sub
      | EPS.Operator.Int.Times -> `Int Llvm.build_mul
      (* TODO: these both have undefined behaviour for division/modulo by zero!
         would prefer to exit or raise a koka exn *)
      | EPS.Operator.Int.Divide -> `Int Llvm.build_sdiv
      (* TODO: good behaviour for modulo of a negative *)
      | EPS.Operator.Int.Modulo -> `Int Llvm.build_srem
      | EPS.Operator.Int.Equals -> `Bool Llvm.Icmp.Eq
      | EPS.Operator.Int.Not_equal -> `Bool Llvm.Icmp.Ne
      | EPS.Operator.Int.Less_than -> `Bool Llvm.Icmp.Slt
      | EPS.Operator.Int.Less_equal -> `Bool Llvm.Icmp.Sle
      | EPS.Operator.Int.Greater_equal -> `Bool Llvm.Icmp.Sge
      | EPS.Operator.Int.Greater_than -> `Bool Llvm.Icmp.Sgt
    in
    (match operation with
    | `Int build ->
      let%bind z = Codegen.use_builder (build x y "int_math") in
      heap_store_int z ~runtime
    | `Bool icmp ->
      let%bind z = Codegen.use_builder (Llvm.build_icmp icmp x y "int_cmp") in
      heap_store_bool z ~runtime)
;;

let compile_unary_operator
    :  Llvm.llvalue -> EPS.Operator.Unary.t -> runtime:Runtime.t
    -> Llvm.llvalue Codegen.t
  =
 fun arg op ~runtime ->
  let open Codegen.Let_syntax in
  match op with
  | EPS.Operator.Unary.Bool EPS.Operator.Bool.Unary.Not ->
    let%bind b = dereference_bool arg in
    let%bind true_ = const_true in
    let%bind not_b = Codegen.use_builder (Llvm.build_xor b true_ "not") in
    heap_store_bool not_b ~runtime
;;

(** [compile_construct_pure x ~runtime] produces code which heap allocates and
    populates a [Types.ctl_pure] struct, returning a [Types.opaque_pointer] to
    it *)
let compile_construct_pure
    : Llvm.llvalue -> runtime:Runtime.t -> Llvm.llvalue Codegen.t
  =
 fun x ~runtime ->
  let open Codegen.Let_syntax in
  let%bind ctl_pure_type = Types.ctl_pure in
  let%bind ctl_pure_ptr = heap_allocate ctl_pure_type ~runtime in
  let%bind tag = const_ctl_pure_tag in
  let%bind tag_ptr =
    Codegen.use_builder (Llvm.build_struct_gep ctl_pure_ptr 0 "tag_p")
  in
  let%bind _store_tag = Codegen.use_builder (Llvm.build_store tag tag_ptr) in
  let%bind value_ptr =
    Codegen.use_builder (Llvm.build_struct_gep ctl_pure_ptr 1 "value_p")
  in
  let%bind _store_value = Codegen.use_builder (Llvm.build_store x value_ptr) in
  let%bind opaque_ptr = Types.opaque_pointer in
  Codegen.use_builder (Llvm.build_bitcast ctl_pure_ptr opaque_ptr "ptr")
;;

(** [compile_construct_yield ~marker ~op_clause ~resumption ~runtime] produces
    code which heap allocates and populates a [Types.ctl_yield] struct,
    returning a [Types.opaque_pointer] to it *)
let compile_construct_yield
    :  marker:Llvm.llvalue -> op_clause:Llvm.llvalue -> resumption:Llvm.llvalue
    -> runtime:Runtime.t -> Llvm.llvalue Codegen.t
  =
 fun ~marker ~op_clause ~resumption ~runtime ->
  let open Codegen.Let_syntax in
  let%bind ctl_yield_type = Types.ctl_yield in
  let%bind ctl_yield_ptr = heap_allocate ctl_yield_type ~runtime in
  let%bind tag = const_ctl_yield_tag in
  let%bind tag_ptr =
    Codegen.use_builder (Llvm.build_struct_gep ctl_yield_ptr 0 "tag_p")
  in
  let%bind _store_tag = Codegen.use_builder (Llvm.build_store tag tag_ptr) in
  let%bind marker_ptr =
    Codegen.use_builder (Llvm.build_struct_gep ctl_yield_ptr 1 "marker_p")
  in
  let%bind _store_marker =
    Codegen.use_builder (Llvm.build_store marker marker_ptr)
  in
  let%bind op_clause_ptr =
    Codegen.use_builder (Llvm.build_struct_gep ctl_yield_ptr 2 "op_clause_p")
  in
  let%bind _store_op_clause =
    Codegen.use_builder (Llvm.build_store op_clause op_clause_ptr)
  in
  let%bind resumption_ptr =
    Codegen.use_builder (Llvm.build_struct_gep ctl_yield_ptr 3 "resumption_p")
  in
  let%bind _store_resumption =
    Codegen.use_builder (Llvm.build_store resumption resumption_ptr)
  in
  let%bind opaque_ptr = Types.opaque_pointer in
  Codegen.use_builder (Llvm.build_bitcast ctl_yield_ptr opaque_ptr "ptr")
;;

(** produces code to evaluate the given expression and store its value to the
    heap. The returned [llvalue] is the [Types.opaque_pointer] to this value. *)
let rec compile_expr : EPS.Expr.t -> runtime:Runtime.t -> Llvm.llvalue Codegen.t
  =
 fun e ~runtime ->
  let open Codegen.Let_syntax in
  match e with
  | EPS.Expr.Literal lit -> compile_literal lit ~runtime
  | EPS.Expr.Unary_operator (op, e) ->
    let%bind arg = compile_expr e ~runtime in
    compile_unary_operator arg op ~runtime
  | EPS.Expr.Operator (e_left, op, e_right) ->
    let%bind left = compile_expr e_left ~runtime in
    let%bind right = compile_expr e_right ~runtime in
    compile_binary_operator ~left op ~right ~runtime
  | EPS.Expr.Construct_pure e ->
    let%bind x = compile_expr e ~runtime in
    compile_construct_pure x ~runtime
  | EPS.Expr.Construct_yield
      { marker = e_marker; op_clause = e_op_clause; resumption = e_resumption }
    ->
    let%bind marker = compile_expr e_marker ~runtime in
    let%bind op_clause = compile_expr e_op_clause ~runtime in
    let%bind resumption = compile_expr e_resumption ~runtime in
    compile_construct_yield ~marker ~op_clause ~resumption ~runtime
  | _ -> failwith "not implemented"
 (* disable fragile-match *)
 [@@warning "-4"]
;;
