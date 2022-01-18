open Core
open Import
module EPS = Koka_zero_evidence_translation.Evidence_passing_syntax

(* TODO: need a type system for telling when llvalues are
   pointers(opaque/typed)/values/*)

(** get an effect's representation, or fail with a codegen impossible_error if
    not found *)
let lookup_effect_repr
    :  Effect_repr.t Effect_label.Map.t -> Effect_label.t
    -> Effect_repr.t Codegen.t
  =
 fun reprs label ->
  let open Codegen.Let_syntax in
  match Map.find reprs label with
  | Some repr -> return repr
  | None ->
    let message =
      sprintf "unbound effect label %s" (Effect_label.to_string label)
    in
    Codegen.impossible_error message
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
    let%bind v = Helpers.const_int i in
    Helpers.heap_store_int v ~runtime
  | EPS.Literal.Bool b ->
    let%bind v = Helpers.const_bool b in
    Helpers.heap_store_bool v ~runtime
  | EPS.Literal.Unit -> Helpers.heap_store_unit ~runtime
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
    let%bind x = Helpers.dereference_bool left in
    let%bind y = Helpers.dereference_bool right in
    let%bind z =
      (* [and]/[or] work directly on bools, don't need to convert to [i1] and
         back *)
      match bool_op with
      | EPS.Operator.Bool.And ->
        Codegen.use_builder (Llvm.build_and x y "bool_and")
      | EPS.Operator.Bool.Or ->
        Codegen.use_builder (Llvm.build_or x y "bool_or")
    in
    Helpers.heap_store_bool z ~runtime
  | EPS.Operator.Int int_op ->
    let%bind x = Helpers.dereference_int left in
    let%bind y = Helpers.dereference_int right in
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
      Helpers.heap_store_int z ~runtime
    | `Bool icmp ->
      let%bind z = Codegen.use_builder (Llvm.build_icmp icmp x y "int_cmp") in
      let%bind b = Helpers.bool_of_i1 z in
      Helpers.heap_store_bool b ~runtime)
;;

let compile_unary_operator
    :  Llvm.llvalue -> EPS.Operator.Unary.t -> runtime:Runtime.t
    -> Llvm.llvalue Codegen.t
  =
 fun arg op ~runtime ->
  let open Codegen.Let_syntax in
  match op with
  | EPS.Operator.Unary.Bool EPS.Operator.Bool.Unary.Not ->
    let%bind b = Helpers.dereference_bool arg in
    let%bind true_ = Helpers.const_true in
    let%bind not_b = Codegen.use_builder (Llvm.build_xor b true_ "not") in
    Helpers.heap_store_bool not_b ~runtime
;;

(** [compile_construct_pure x ~runtime] produces code which heap allocates and
    populates a [Types.ctl_pure] struct, returning a [Types.opaque_pointer] to
    it *)
let compile_construct_pure
    : Llvm.llvalue -> runtime:Runtime.t -> Llvm.llvalue Codegen.t
  =
 fun x ~runtime ->
  let open Codegen.Let_syntax in
  let%bind ctl_type = Types.ctl in
  (* always allocate a [ctl], then cast, to be sure of correct alignment *)
  let%bind ctl_ptr = Helpers.heap_allocate ctl_type "ctl" ~runtime in
  let%bind ctl_pure_type = Types.ctl_pure in
  let ctl_pure_ptr_type = Llvm.pointer_type ctl_pure_type in
  let%bind ctl_pure_ptr =
    Codegen.use_builder
      (Llvm.build_bitcast ctl_ptr ctl_pure_ptr_type "ctl_pure_ptr")
  in
  let%bind tag = Helpers.const_ctl_pure_tag in
  let%bind () =
    Helpers.compile_populate_struct ctl_pure_ptr [ tag, "tag"; x, "value" ]
  in
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
  let%bind ctl_type = Types.ctl in
  let%bind ctl_ptr = Helpers.heap_allocate ctl_type "ctl" ~runtime in
  let%bind ctl_yield_type = Types.ctl_yield in
  let ctl_yield_ptr_type = Llvm.pointer_type ctl_yield_type in
  let%bind ctl_yield_ptr =
    Codegen.use_builder
      (Llvm.build_bitcast ctl_ptr ctl_yield_ptr_type "ctl_yield_ptr")
  in
  let%bind tag = Helpers.const_ctl_yield_tag in
  let%bind () =
    Helpers.compile_populate_struct
      ctl_yield_ptr
      [ tag, "tag"
      ; marker, "marker"
      ; op_clause, "op_clause"
      ; resumption, "resumption"
      ]
  in
  let%bind opaque_ptr = Types.opaque_pointer in
  Codegen.use_builder (Llvm.build_bitcast ctl_yield_ptr opaque_ptr "ptr")
;;

(** [compile_select_operation label ~op_name v ~effect_reprs] generates code
    which selects the operation clause for [op_name] from [v] (a handler for the
    effect [label] of type [Types.unique_pointer]) *)
let compile_select_operation
    :  Effect_label.t -> op_name:Variable.t -> Llvm.llvalue
    -> effect_reprs:Effect_repr.t Effect_label.Map.t -> Llvm.llvalue Codegen.t
  =
 fun label ~op_name handler_ptr ~effect_reprs ->
  let open Codegen.Let_syntax in
  let%bind repr = lookup_effect_repr effect_reprs label in
  let { Effect_repr.hnd_type; operations; _ } = repr in
  let hnd_ptr_type = Llvm.pointer_type hnd_type in
  let%bind handler =
    Codegen.use_builder (Llvm.build_bitcast handler_ptr hnd_ptr_type "hnd_ptr")
  in
  let%bind op_index =
    match
      List.findi operations ~f:(fun _i op_name' ->
          Variable.(op_name = op_name'))
    with
    | Some (op_index, _op_name) -> return op_index
    | None ->
      let message =
        sprintf
          "unbound operation %s for effect %s"
          (Variable.to_string_user op_name)
          (Effect_label.to_string label)
      in
      Codegen.impossible_error message
  in
  (* pointer into the struct - of type [op_clause**] *)
  let%bind op_clause_field_ptr =
    Codegen.use_builder (Llvm.build_struct_gep handler op_index "op_clause_ptr")
  in
  let%bind opaque_pointer = Types.opaque_pointer in
  Helpers.dereference op_clause_field_ptr opaque_pointer "op_clause"
;;

(** produces code to evaluate the given expression and store its value to the
    heap. The returned [llvalue] is the [Types.opaque_pointer] to this value. *)
let rec compile_expr
    :  EPS.Expr.t -> runtime:Runtime.t
    -> effect_reprs:Effect_repr.t Effect_label.Map.t -> Llvm.llvalue Codegen.t
  =
 fun e ~runtime ~effect_reprs ->
  let open Codegen.Let_syntax in
  match e with
  | EPS.Expr.Variable v ->
    (* lookup in environment to find access path *)
    (* generate code to either traverse the closure or use a parameter directly *)
    failwith "not implemented"
  | EPS.Expr.Lambda lambda -> compile_lambda lambda ~runtime ~effect_reprs
  | EPS.Expr.Fix_lambda fix_lambda ->
    compile_fix_lambda fix_lambda ~runtime ~effect_reprs
  | EPS.Expr.Application (e_f, e_args) ->
    compile_application e_f e_args ~runtime ~effect_reprs
  | EPS.Expr.Literal lit -> compile_literal lit ~runtime
  | EPS.Expr.Unary_operator (op, e) ->
    let%bind arg = compile_expr e ~runtime ~effect_reprs in
    compile_unary_operator arg op ~runtime
  | EPS.Expr.Operator (e_left, op, e_right) ->
    let%bind left = compile_expr e_left ~runtime ~effect_reprs in
    let%bind right = compile_expr e_right ~runtime ~effect_reprs in
    compile_binary_operator ~left op ~right ~runtime
  | EPS.Expr.Construct_pure e ->
    let%bind x = compile_expr e ~runtime ~effect_reprs in
    compile_construct_pure x ~runtime
  | EPS.Expr.Construct_yield
      { marker = e_marker; op_clause = e_op_clause; resumption = e_resumption }
    ->
    let%bind marker = compile_expr e_marker ~runtime ~effect_reprs in
    let%bind op_clause = compile_expr e_op_clause ~runtime ~effect_reprs in
    let%bind resumption = compile_expr e_resumption ~runtime ~effect_reprs in
    compile_construct_yield ~marker ~op_clause ~resumption ~runtime
  | EPS.Expr.Fresh_marker ->
    let { Runtime.fresh_marker; _ } = runtime in
    let%bind m =
      Codegen.use_builder
        (Llvm.build_call fresh_marker (Array.of_list []) "fresh_marker")
    in
    Helpers.heap_store_marker m ~runtime
  | EPS.Expr.Markers_equal (e1, e2) ->
    let%bind marker1_ptr = compile_expr e1 ~runtime ~effect_reprs in
    let%bind marker2_ptr = compile_expr e2 ~runtime ~effect_reprs in
    let%bind marker1 = Helpers.dereference_marker marker1_ptr in
    let%bind marker2 = Helpers.dereference_marker marker2_ptr in
    let { Runtime.markers_equal; _ } = runtime in
    let%bind eq =
      Codegen.use_builder
        (Llvm.build_call
           markers_equal
           (Array.of_list [ marker1; marker2 ])
           "markers_equal")
    in
    Helpers.heap_store_bool eq ~runtime
  | EPS.Expr.Effect_label label ->
    let%bind repr = lookup_effect_repr effect_reprs label in
    let { Effect_repr.id; _ } = repr in
    let%bind label = Helpers.const_label id in
    Helpers.heap_store_label label ~runtime
  | EPS.Expr.Construct_handler
      { handled_effect
      ; operation_clauses = operation_clause_exprs
      ; return_clause
      } ->
    compile_construct_handler
      handled_effect
      operation_clause_exprs
      return_clause
      ~runtime
      ~effect_reprs
  | EPS.Expr.Select_operation (label, op_name, e_handler) ->
    let%bind handler_ptr = compile_expr e_handler ~runtime ~effect_reprs in
    compile_select_operation label ~op_name handler_ptr ~effect_reprs
  | EPS.Expr.Nil_evidence_vector ->
    let { Runtime.nil_evidence_vector; _ } = runtime in
    Codegen.use_builder
      (Llvm.build_call nil_evidence_vector (Array.of_list []) "nil_vector")
  | EPS.Expr.Cons_evidence_vector
      { label = e_label
      ; marker = e_marker
      ; handler = e_handler
      ; vector_tail = e_vector_tail
      } ->
    let%bind label_ptr = compile_expr e_label ~runtime ~effect_reprs in
    let%bind label = Helpers.dereference_label label_ptr in
    let%bind marker_ptr = compile_expr e_marker ~runtime ~effect_reprs in
    let%bind marker = Helpers.dereference_marker marker_ptr in
    let%bind handler = compile_expr e_handler ~runtime ~effect_reprs in
    let%bind vector_tail = compile_expr e_vector_tail ~runtime ~effect_reprs in
    let { Runtime.cons_evidence_vector; _ } = runtime in
    let args = Array.of_list [ label; marker; handler; vector_tail ] in
    Codegen.use_builder
      (Llvm.build_call cons_evidence_vector args "extended_vector")
  | EPS.Expr.Lookup_evidence { label = e_label; vector = e_vector } ->
    let%bind label_ptr = compile_expr e_label ~runtime ~effect_reprs in
    let%bind label = Helpers.dereference_label label_ptr in
    let%bind vector = compile_expr e_vector ~runtime ~effect_reprs in
    let { Runtime.evidence_vector_lookup; _ } = runtime in
    let args = Array.of_list [ vector; label ] in
    Codegen.use_builder (Llvm.build_call evidence_vector_lookup args "evidence")
  | EPS.Expr.Get_evidence_marker e ->
    let%bind evidence = compile_expr e ~runtime ~effect_reprs in
    let { Runtime.get_evidence_marker; _ } = runtime in
    let%bind marker =
      Codegen.use_builder
        (Llvm.build_call
           get_evidence_marker
           (Array.of_list [ evidence ])
           "marker")
    in
    Helpers.heap_store_marker marker ~runtime
  | EPS.Expr.Get_evidence_handler e ->
    let%bind evidence = compile_expr e ~runtime ~effect_reprs in
    let { Runtime.get_evidence_handler; _ } = runtime in
    Codegen.use_builder
      (Llvm.build_call
         get_evidence_handler
         (Array.of_list [ evidence ])
         "handler")
  | EPS.Expr.Impure_built_in impure ->
    compile_impure_built_in impure ~runtime ~effect_reprs
  | _ -> failwith "not implemented"
 (* disable fragile-match *)
 [@@warning "-4"]

and compile_lambda
    :  EPS.Expr.lambda -> runtime:Runtime.t
    -> effect_reprs:Effect_repr.t Effect_label.Map.t -> Llvm.llvalue Codegen.t
  =
 fun (xs, e_body) ->
  let open Codegen.Let_syntax in
  (* save the current builder insertion location *)
  (* new llvm function *)
  (* with args [xs] + closure TODO: avoid closure shadowing any *)
  (* function's first statement is to build closure capturing its own args *)
  (* compile [e_body] in extended environment + that closure *)
  (* verify that function*)
  (* go back to where we were inserting *)
  (* make this function object: - code address - non recursive - current closure
     (not that function's new closure) *)
  failwith "not implemented"

and compile_fix_lambda
    :  EPS.Expr.fix_lambda -> runtime:Runtime.t
    -> effect_reprs:Effect_repr.t Effect_label.Map.t -> Llvm.llvalue Codegen.t
  =
 fun (f, (xs, e_body)) ~runtime ~effect_reprs ->
  let open Codegen.Let_syntax in
  (* likely not compositional! (doesn't call compile-lambda) *)
  failwith "not implemented"

and compile_application
    :  EPS.Expr.t -> EPS.Expr.t list -> runtime:Runtime.t
    -> effect_reprs:Effect_repr.t Effect_label.Map.t -> Llvm.llvalue Codegen.t
  =
 fun e_f e_args ~runtime ~effect_reprs ->
  let open Codegen.Let_syntax in
  let%bind f_opaque_ptr = compile_expr e_f ~runtime ~effect_reprs in
  let%bind arg_ptrs =
    List.map e_args ~f:(compile_expr ~runtime ~effect_reprs) |> Codegen.all
  in
  (* cast v_f to function type *)
  let%bind function_object_type = Types.function_object in
  let function_object_ptr_type = Llvm.pointer_type function_object_type in
  let%bind f_ptr =
    Codegen.use_builder
      (Llvm.build_bitcast f_opaque_ptr function_object_ptr_type "function_ptr")
  in
  (* extract fields of f *)
  let%bind code_address_ptr =
    Codegen.use_builder (Llvm.build_struct_gep f_ptr 0 "code_address_ptr")
  in
  let%bind code_address_opaque =
    Codegen.use_builder (Llvm.build_load code_address_ptr "code_address")
  in
  let%bind closure_field_ptr =
    Codegen.use_builder (Llvm.build_struct_gep f_ptr 1 "closure_field_ptr")
  in
  let%bind closure_ptr =
    Codegen.use_builder (Llvm.build_load closure_field_ptr "closure_ptr")
  in
  let%bind is_recursive_ptr =
    Codegen.use_builder (Llvm.build_struct_gep f_ptr 2 "is_recursive_ptr")
  in
  let%bind is_recursive =
    Codegen.use_builder (Llvm.build_load is_recursive_ptr "is_recursive")
  in
  (* the number of arguments passed in the evidence passing representation *)
  let num_eps_args = List.length arg_ptrs in
  let%bind generated_function_type = Types.function_code num_eps_args in
  let generated_function_ptr_type = Llvm.pointer_type generated_function_type in
  (* pass either [f_ptr] or [null] depending on whether the function is
     recursive *)
  let null_function = Llvm.const_pointer_null generated_function_ptr_type in
  let%bind f_self =
    Codegen.use_builder
      (Llvm.build_select is_recursive f_ptr null_function "f_self")
  in
  let args = Array.of_list (f_self :: closure_ptr :: arg_ptrs) in
  let%bind typed_code_address =
    Codegen.use_builder
      (Llvm.build_bitcast
         code_address_opaque
         generated_function_ptr_type
         "typed_code_address")
  in
  Codegen.use_builder (Llvm.build_call typed_code_address args "result")

and compile_construct_handler
    :  Effect_label.t -> EPS.Expr.t Variable.Map.t -> EPS.Expr.t option
    -> runtime:Runtime.t -> effect_reprs:Effect_repr.t Effect_label.Map.t
    -> Llvm.llvalue Codegen.t
  =
 fun handled_effect operation_clause_exprs return_clause ~runtime ~effect_reprs ->
  let open Codegen.Let_syntax in
  let%bind repr = lookup_effect_repr effect_reprs handled_effect in
  let { Effect_repr.hnd_type; operations; _ } = repr in
  let%bind (operation_clauses_and_names : (Llvm.llvalue * string) list) =
    List.map operations ~f:(fun op_name ->
        let%bind e_clause =
          match Map.find operation_clause_exprs op_name with
          | Some e_clause -> return e_clause
          | None ->
            let message =
              sprintf
                "missing operation %s in handler for effect %s"
                (Variable.to_string_user op_name)
                (Effect_label.to_string handled_effect)
            in
            Codegen.impossible_error message
        in
        let%map clause = compile_expr e_clause ~runtime ~effect_reprs in
        clause, Helpers.register_name_of_variable op_name)
    |> Codegen.all
  in
  let%bind () =
    match return_clause with
    | None -> return ()
    | Some _ -> Codegen.unsupported_feature_error "return clause in handler"
  in
  let%bind handler_ptr = Helpers.heap_allocate hnd_type "hnd" ~runtime in
  let%bind () =
    Helpers.compile_populate_struct handler_ptr operation_clauses_and_names
  in
  let%bind opaque_pointer = Types.opaque_pointer in
  Codegen.use_builder (Llvm.build_bitcast handler_ptr opaque_pointer "hnd_ptr")

and compile_impure_built_in
    :  EPS.Expr.impure_built_in -> runtime:Runtime.t
    -> effect_reprs:Effect_repr.t Effect_label.Map.t -> Llvm.llvalue Codegen.t
  =
 fun impure ~runtime ~effect_reprs ->
  let open Codegen.Let_syntax in
  match impure with
  | EPS.Expr.Impure_print_int e ->
    let%bind v = compile_expr e ~runtime ~effect_reprs in
    let%bind i = Helpers.dereference_int v in
    let { Runtime.print_int; _ } = runtime in
    let%bind _void =
      Codegen.use_builder
        (Llvm.build_call print_int (Array.of_list [ i ]) "void")
    in
    Helpers.heap_store_unit ~runtime
  | EPS.Expr.Impure_read_int ->
    let { Runtime.read_int; _ } = runtime in
    let%bind i =
      Codegen.use_builder (Llvm.build_call read_int (Array.of_list []) "i")
    in
    Helpers.heap_store_int i ~runtime
;;

let compile_effect_decl
    : EPS.Program.Effect_decl.t -> id:int -> Effect_repr.t Codegen.t
  =
 fun { EPS.Program.Effect_decl.operations; name = _ } ~id ->
  let open Codegen.Let_syntax in
  (* result may be sorted anyway, but this makes intent obvious and is less
     brittle *)
  let operations =
    Set.to_list operations |> List.sort ~compare:Variable.compare
  in
  let%bind opaque_pointer = Types.opaque_pointer in
  let fields = Array.of_list_map operations ~f:(fun _op -> opaque_pointer) in
  let%map hnd_type =
    Codegen.use_context (fun context -> Llvm.struct_type context fields)
  in
  { Effect_repr.id; hnd_type; operations }
;;

let compile_effect_decls
    :  EPS.Program.Effect_decl.t list
    -> Effect_repr.t Effect_label.Map.t Codegen.t
  =
 fun decls ->
  let open Codegen.Let_syntax in
  let initial = 0, Effect_label.Map.empty in
  let%map _next_label_id, effect_reprs =
    Codegen.list_fold
      decls
      ~init:initial
      ~f:(fun (next_label_id, effect_reprs) decl ->
        let { EPS.Program.Effect_decl.name; _ } = decl in
        let%map repr = compile_effect_decl decl ~id:next_label_id in
        let next_label_id = next_label_id + 1 in
        let effect_reprs = Map.add_exn effect_reprs ~key:name ~data:repr in
        next_label_id, effect_reprs)
  in
  effect_reprs
;;
