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

let heap_allocate
    : Llvm.lltype -> Llvm.llvalue -> runtime:Runtime.t -> Llvm.llvalue Codegen.t
  =
 fun t v ~runtime ->
  let open Codegen.Let_syntax in
  let size = Llvm.size_of t in
  let { Runtime.malloc; _ } = runtime in
  let%bind heap_ptr =
    Codegen.use_builder
      (Llvm.build_call malloc (Array.of_list [ size ]) "heap_ptr")
  in
  let t_ptr_type = Llvm.pointer_type t in
  let%bind t_ptr =
    Codegen.use_builder (Llvm.build_bitcast heap_ptr t_ptr_type "typed_ptr")
  in
  let%map _store = Codegen.use_builder (Llvm.build_store v t_ptr) in
  heap_ptr
;;

(** [heap_allocate_int i] allocates memory to hold a [Types.int] and stores [i]
    there. It returns the address as a [Types.opaque_pointer] *)
let heap_allocate_int
    : Llvm.llvalue -> runtime:Runtime.t -> Llvm.llvalue Codegen.t
  =
 fun i ~runtime ->
  let open Codegen.Let_syntax in
  let%bind type_int = Types.int in
  heap_allocate type_int i ~runtime
;;

(* TODO: could just allocate [true] and [false] at program start, then reuse
   those *)
let heap_allocate_bool
    : Llvm.llvalue -> runtime:Runtime.t -> Llvm.llvalue Codegen.t
  =
 fun b ~runtime ->
  let open Codegen.Let_syntax in
  let%bind type_bool = Types.bool in
  heap_allocate type_bool b ~runtime
;;

let heap_allocate_unit : runtime:Runtime.t -> Llvm.llvalue Codegen.t =
 fun ~runtime ->
  let open Codegen.Let_syntax in
  let%bind u = const_unit in
  let%bind type_unit = Types.unit in
  heap_allocate type_unit u ~runtime
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
    heap_allocate_int v ~runtime
  | EPS.Literal.Bool b ->
    let%bind v = const_bool b in
    heap_allocate_bool v ~runtime
  | EPS.Literal.Unit -> heap_allocate_unit ~runtime
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
    heap_allocate_bool z ~runtime
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
      heap_allocate_int z ~runtime
    | `Bool icmp ->
      let%bind z = Codegen.use_builder (Llvm.build_icmp icmp x y "int_cmp") in
      heap_allocate_bool z ~runtime)
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
    heap_allocate_bool not_b ~runtime
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
  | _ -> failwith "not implemented"
 (* disable fragile-match *)
 [@@warning "-4"]
;;
