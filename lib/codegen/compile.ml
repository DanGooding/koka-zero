open Core
module EPS = Koka_zero_evidence_translation.Evidence_passing_syntax

(* TODO: these are not heap allocated - make that obvious *)
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
    there. It returns the address as a [Types.value_pointer] *)
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

(* TODO: need a type system for telling when llvalues are
   pointers(opaque/typed)/values/*)
let dereference_bool : Llvm.llvalue -> Llvm.llvalue Codegen.t =
 fun value_ptr ->
  let open Codegen.Let_syntax in
  let%bind bool_type = Types.bool in
  let bool_ptr_type = Llvm.pointer_type bool_type in
  let%bind bool_ptr =
    Codegen.use_builder (Llvm.build_bitcast value_ptr bool_ptr_type "boolptr")
  in
  Codegen.use_builder (Llvm.build_load bool_ptr "bool")
;;

(** produces code to evaluate the given expression and store its value to the
    heap. The returned [llvalue] is the [Types.value_pointer] to this value. *)
let rec compile_expr : EPS.Expr.t -> runtime:Runtime.t -> Llvm.llvalue Codegen.t
  =
 fun e ~runtime ->
  let open Codegen.Let_syntax in
  match e with
  | EPS.Expr.Unary_operator (op, e) ->
    let%bind v_ptr = compile_expr e ~runtime in
    (match op with
    | EPS.Operator.Unary.Bool EPS.Operator.Bool.Unary.Not ->
      let%bind b = dereference_bool v_ptr in
      let%bind true_ = const_true in
      let%bind not_b = Codegen.use_builder (Llvm.build_xor b true_ "not") in
      heap_allocate_bool not_b ~runtime)
  | _ -> failwith "not implemented"
 [@@warning "-4"]
;;
(* disable fragile-match *)
