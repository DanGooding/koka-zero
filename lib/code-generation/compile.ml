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
    effect [label] of type [Types.opaque_pointer]) *)
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
    Codegen.use_builder
      (Llvm.build_struct_gep handler op_index "op_clause_field_ptr")
  in
  Codegen.use_builder (Llvm.build_load op_clause_field_ptr "op_clause")
;;

(** [compile_construct_function_object code_address ~is_recursive ~captured_closure ...]
    generates code to heap allocate and populate a [Types.function_object], with
    code pointed to by the (typed or opaque) [code_address], keeping
    [captured_closure] as it's closure. *)
let compile_construct_function_object
    :  Llvm.llvalue -> is_recursive:bool -> captured_closure:Llvm.llvalue
    -> runtime:Runtime.t -> Llvm.llvalue Codegen.t
  =
 fun code_address ~is_recursive ~captured_closure ~runtime ->
  let open Codegen.Let_syntax in
  let%bind function_object_type = Types.function_object in
  let%bind function_ptr =
    Helpers.heap_allocate function_object_type "function" ~runtime
  in
  let%bind opaque_pointer = Types.opaque_pointer in
  let%bind function_code_opaque_ptr =
    Codegen.use_builder
      (Llvm.build_bitcast code_address opaque_pointer "code_address_opaque_ptr")
  in
  let%bind i1 = Codegen.use_context Llvm.i1_type in
  let is_recursive = Llvm.const_int i1 (if is_recursive then 1 else 0) in
  let fields =
    [ function_code_opaque_ptr, "code"
    ; captured_closure, "closure"
    ; is_recursive, "is_recursive"
    ]
  in
  let%bind () = Helpers.compile_populate_struct function_ptr fields in
  Codegen.use_builder
    (Llvm.build_bitcast function_ptr opaque_pointer "function_opaque")
;;

(** [compile_conditional cond ~compile_true ~compile_false] generates a branch
    based on the value of [i1] [cond], to either the code of [compile_true] or
    [compile_false], then generates a phi node to combine their values *)
let compile_conditional
    :  Llvm.llvalue -> compile_true:(unit -> Llvm.llvalue Codegen.t)
    -> compile_false:(unit -> Llvm.llvalue Codegen.t) -> Llvm.llvalue Codegen.t
  =
 fun cond ~compile_true ~compile_false ->
  let open Codegen.Let_syntax in
  let%bind if_start_block = Codegen.insertion_block_exn in
  let current_function = Llvm.block_parent if_start_block in
  let%bind true_start_block =
    Codegen.use_context (fun context ->
        Llvm.append_block context "if_true" current_function)
  in
  let%bind false_start_block =
    Codegen.use_context (fun context ->
        Llvm.append_block context "if_false" current_function)
  in
  let%bind if_end_block =
    Codegen.use_context (fun context ->
        Llvm.append_block context "post_if" current_function)
  in
  let%bind _branch =
    Codegen.use_builder
      (Llvm.build_cond_br cond true_start_block false_start_block)
  in
  (* compile yes branch *)
  let%bind () = Codegen.use_builder (Llvm.position_at_end true_start_block) in
  let%bind true_branch_result = compile_true () in
  let%bind _br_end = Codegen.use_builder (Llvm.build_br if_end_block) in
  let%bind true_end_block = Codegen.insertion_block_exn in
  (* compile false branch *)
  let%bind () = Codegen.use_builder (Llvm.position_at_end false_start_block) in
  let%bind false_branch_result = compile_false () in
  let%bind _br_end = Codegen.use_builder (Llvm.build_br if_end_block) in
  let%bind false_end_block = Codegen.insertion_block_exn in
  (* connect back together *)
  let%bind () = Codegen.use_builder (Llvm.position_at_end if_end_block) in
  let incoming =
    [ true_branch_result, true_end_block; false_branch_result, false_end_block ]
  in
  Codegen.use_builder (Llvm.build_phi incoming "if_result")
;;

(** produces code to evaluate the given expression and store its value to the
    heap. The returned [llvalue] is the [Types.opaque_pointer] to this value. *)
let rec compile_expr
    :  EPS.Expr.t -> env:Context.t -> runtime:Runtime.t
    -> effect_reprs:Effect_repr.t Effect_label.Map.t
    -> outer_symbol:Symbol_name.t -> Llvm.llvalue Codegen.t
  =
 fun e ~env ~runtime ~effect_reprs ~outer_symbol ->
  let open Codegen.Let_syntax in
  match e with
  | EPS.Expr.Variable v ->
    (* TODO: note this will be duplicated for each access, although common
       subexpression elimination should easily remove it *)
    Context.compile_get env v
  | EPS.Expr.Lambda lambda ->
    compile_lambda lambda ~env ~runtime ~effect_reprs ~outer_symbol
  | EPS.Expr.Fix_lambda fix_lambda ->
    compile_fix_lambda fix_lambda ~env ~runtime ~effect_reprs ~outer_symbol
  | EPS.Expr.Application (e_f, e_args) ->
    let%bind f = compile_expr e_f ~env ~runtime ~effect_reprs ~outer_symbol in
    let%bind args =
      List.map
        e_args
        ~f:(compile_expr ~env ~runtime ~effect_reprs ~outer_symbol)
      |> Codegen.all
    in
    compile_application f args
  | EPS.Expr.Literal lit -> compile_literal lit ~runtime
  | EPS.Expr.If_then_else (e_cond, e_yes, e_no) ->
    let%bind cond_ptr =
      compile_expr e_cond ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind cond = Helpers.dereference_bool cond_ptr in
    compile_if_then_else
      cond
      ~e_yes
      ~e_no
      ~env
      ~runtime
      ~effect_reprs
      ~outer_symbol
  | EPS.Expr.Unary_operator (op, e) ->
    let%bind arg = compile_expr e ~env ~runtime ~effect_reprs ~outer_symbol in
    compile_unary_operator arg op ~runtime
  | EPS.Expr.Operator (e_left, op, e_right) ->
    let%bind left =
      compile_expr e_left ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind right =
      compile_expr e_right ~env ~runtime ~effect_reprs ~outer_symbol
    in
    compile_binary_operator ~left op ~right ~runtime
  | EPS.Expr.Construct_pure e ->
    let%bind x = compile_expr e ~env ~runtime ~effect_reprs ~outer_symbol in
    compile_construct_pure x ~runtime
  | EPS.Expr.Construct_yield
      { marker = e_marker; op_clause = e_op_clause; resumption = e_resumption }
    ->
    let%bind marker =
      compile_expr e_marker ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind op_clause =
      compile_expr e_op_clause ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind resumption =
      compile_expr e_resumption ~env ~runtime ~effect_reprs ~outer_symbol
    in
    compile_construct_yield ~marker ~op_clause ~resumption ~runtime
  | EPS.Expr.Match_ctl { subject = e_subject; pure_branch; yield_branch } ->
    let%bind subject =
      compile_expr e_subject ~env ~runtime ~effect_reprs ~outer_symbol
    in
    compile_match_ctl
      subject
      ~pure_branch
      ~yield_branch
      ~env
      ~runtime
      ~effect_reprs
      ~outer_symbol
  | EPS.Expr.Fresh_marker ->
    let { Runtime.fresh_marker; _ } = runtime in
    let%bind m =
      Codegen.use_builder
        (Llvm.build_call fresh_marker (Array.of_list []) "fresh_marker")
    in
    Helpers.heap_store_marker m ~runtime
  | EPS.Expr.Markers_equal (e1, e2) ->
    let%bind marker1_ptr =
      compile_expr e1 ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind marker2_ptr =
      compile_expr e2 ~env ~runtime ~effect_reprs ~outer_symbol
    in
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
      { handled_effect; operation_clauses = operation_clause_exprs } ->
    compile_construct_handler
      handled_effect
      operation_clause_exprs
      ~env
      ~runtime
      ~effect_reprs
      ~outer_symbol
  | EPS.Expr.Select_operation (label, op_name, e_handler) ->
    let%bind handler_ptr =
      compile_expr e_handler ~env ~runtime ~effect_reprs ~outer_symbol
    in
    compile_select_operation label ~op_name handler_ptr ~effect_reprs
  | EPS.Expr.Nil_evidence_vector ->
    let { Runtime.nil_evidence_vector; _ } = runtime in
    Codegen.use_builder
      (Llvm.build_call nil_evidence_vector (Array.of_list []) "nil_vector")
  | EPS.Expr.Cons_evidence_vector
      { label = e_label
      ; marker = e_marker
      ; handler = e_handler
      ; handler_site_vector = e_handler_site_vector
      ; vector_tail = e_vector_tail
      } ->
    let%bind label_ptr =
      compile_expr e_label ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind label = Helpers.dereference_label label_ptr in
    let%bind marker_ptr =
      compile_expr e_marker ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind marker = Helpers.dereference_marker marker_ptr in
    let%bind handler =
      compile_expr e_handler ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind handler_site_vector =
      compile_expr
        e_handler_site_vector
        ~env
        ~runtime
        ~effect_reprs
        ~outer_symbol
    in
    let%bind vector_tail =
      compile_expr e_vector_tail ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let { Runtime.cons_evidence_vector; _ } = runtime in
    let args =
      Array.of_list [ label; marker; handler; handler_site_vector; vector_tail ]
    in
    Codegen.use_builder
      (Llvm.build_call cons_evidence_vector args "extended_vector")
  | EPS.Expr.Lookup_evidence { label = e_label; vector = e_vector } ->
    let%bind label_ptr =
      compile_expr e_label ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind label = Helpers.dereference_label label_ptr in
    let%bind vector =
      compile_expr e_vector ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let { Runtime.evidence_vector_lookup; _ } = runtime in
    let args = Array.of_list [ vector; label ] in
    Codegen.use_builder (Llvm.build_call evidence_vector_lookup args "evidence")
  | EPS.Expr.Get_evidence_marker e ->
    let%bind evidence =
      compile_expr e ~env ~runtime ~effect_reprs ~outer_symbol
    in
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
    let%bind evidence =
      compile_expr e ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let { Runtime.get_evidence_handler; _ } = runtime in
    Codegen.use_builder
      (Llvm.build_call
         get_evidence_handler
         (Array.of_list [ evidence ])
         "handler")
  | EPS.Expr.Get_evidence_handler_site_vector e ->
    let%bind evidence =
      compile_expr e ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let { Runtime.get_evidence_handler_site_vector; _ } = runtime in
    Codegen.use_builder
      (Llvm.build_call
         get_evidence_handler_site_vector
         (Array.of_list [ evidence ])
         "handler_site_vector")
  | EPS.Expr.Impure_built_in impure ->
    compile_impure_built_in impure ~env ~runtime ~effect_reprs ~outer_symbol

(** [compile_match_ctl subject ~pure_branch ~yield_branch ...] generates code to
    branch on the ctl varaint poitned to by [Types.opaque_pointer]:[subject],
    calling either [pure_branch] or [yield_branch] with its fields. (These are
    also compiled) *)
and compile_match_ctl
    :  Llvm.llvalue -> pure_branch:EPS.Expr.lambda
    -> yield_branch:EPS.Expr.lambda -> env:Context.t -> runtime:Runtime.t
    -> effect_reprs:Effect_repr.t Effect_label.Map.t
    -> outer_symbol:Symbol_name.t -> Llvm.llvalue Codegen.t
  =
 fun subject
     ~pure_branch
     ~yield_branch
     ~env
     ~runtime
     ~effect_reprs
     ~outer_symbol ->
  let open Codegen.Let_syntax in
  let%bind pure_function =
    compile_lambda pure_branch ~env ~runtime ~effect_reprs ~outer_symbol
  in
  let%bind yield_function =
    compile_lambda yield_branch ~env ~runtime ~effect_reprs ~outer_symbol
  in
  let%bind ctl_type = Types.ctl in
  let ctl_ptr_type = Llvm.pointer_type ctl_type in
  let%bind ctl_ptr =
    Codegen.use_builder (Llvm.build_bitcast subject ctl_ptr_type "ctl_ptr")
  in
  let%bind tag = Helpers.compile_access_field ctl_ptr 0 "tag" in
  let%bind pure_tag = Helpers.const_tag 0 in
  let%bind is_pure =
    Codegen.use_builder (Llvm.build_icmp Llvm.Icmp.Eq tag pure_tag "is_pure")
  in
  compile_conditional
    is_pure
    ~compile_true:(fun () ->
      let%bind ctl_pure_type = Types.ctl_pure in
      let ctl_pure_ptr_type = Llvm.pointer_type ctl_pure_type in
      let%bind ctl_pure_ptr =
        Codegen.use_builder
          (Llvm.build_bitcast subject ctl_pure_ptr_type "ctl_pure_ptr")
      in
      let%bind value = Helpers.compile_access_field ctl_pure_ptr 1 "value" in
      compile_application pure_function [ value ])
    ~compile_false:(fun () ->
      let%bind ctl_yield_type = Types.ctl_yield in
      let ctl_yield_ptr_type = Llvm.pointer_type ctl_yield_type in
      let%bind ctl_yield_ptr =
        Codegen.use_builder
          (Llvm.build_bitcast subject ctl_yield_ptr_type "ctl_yield_ptr")
      in
      let%bind marker = Helpers.compile_access_field ctl_yield_ptr 1 "marker" in
      let%bind op_clause =
        Helpers.compile_access_field ctl_yield_ptr 2 "op_clause"
      in
      let%bind resumption =
        Helpers.compile_access_field ctl_yield_ptr 3 "resumption"
      in
      compile_application yield_function [ marker; op_clause; resumption ])

(** [compile_if_then_else b ~e_yes ~e_no ~env ~runtime ~effect_reprs ~outer_symbol]
    generates code to branch on the value of the [Types.bool] [b], and evaluate
    to the value of either [e_yes] or [e_no] *)
and compile_if_then_else
    :  Llvm.llvalue -> e_yes:EPS.Expr.t -> e_no:EPS.Expr.t -> env:Context.t
    -> runtime:Runtime.t -> effect_reprs:Effect_repr.t Effect_label.Map.t
    -> outer_symbol:Symbol_name.t -> Llvm.llvalue Codegen.t
  =
 fun cond ~e_yes ~e_no ~env ~runtime ~effect_reprs ~outer_symbol ->
  let open Codegen.Let_syntax in
  let%bind cond_i1 = Helpers.i1_of_bool cond in
  compile_conditional
    cond_i1
    ~compile_true:(fun () ->
      compile_expr e_yes ~env ~runtime ~effect_reprs ~outer_symbol)
    ~compile_false:(fun () ->
      compile_expr e_no ~env ~runtime ~effect_reprs ~outer_symbol)

(** [compile_function ~symbol_name rec_name ps e_body ~captured_shape ~outer_symbol ...]
    generates a function with the given arguments and body, and within the scope
    given by [captured_shape]. It returns this function's [llvalue]. The
    [llbuilder]'s insertion point is saved and restored. *)
and compile_function
    :  symbol_name:Symbol_name.t -> rec_name:Variable.t option
    -> EPS.Parameter.t list -> EPS.Expr.t
    -> captured_shape:Context.Closure.Shape.t -> outer_symbol:Symbol_name.t
    -> runtime:Runtime.t -> effect_reprs:Effect_repr.t Effect_label.Map.t
    -> Llvm.llvalue Codegen.t
  =
 fun ~symbol_name
     ~rec_name
     ps
     e_body
     ~captured_shape
     ~outer_symbol
     ~runtime
     ~effect_reprs ->
  let open Codegen.Let_syntax in
  (* new llvm function *)
  let%bind type_ = Types.function_code (List.length ps) in
  let%bind function_ =
    Codegen.use_module
      (Llvm.define_function (Symbol_name.to_string symbol_name) type_)
  in
  let function_start_block = Llvm.entry_block function_ in
  let%map () =
    Codegen.within_block function_start_block ~f:(fun () ->
        (* name parameters: *)
        let params = Llvm.params function_ |> Array.to_list in
        let f_self_param, closure_param, params =
          match params with
          | f_self_param :: closure_param :: params ->
            f_self_param, closure_param, params
          | _ ->
            (* this is a programmer error not a data error *)
            raise_s
              [%message "function type has unexpected number of parameters"]
        in
        let%bind opaque_pointer = Types.opaque_pointer in
        let%bind (parameters : (EPS.Parameter.t * Llvm.llvalue) list) =
          match rec_name with
          | Some rec_name ->
            let%map f_self_param =
              Codegen.use_builder
                (Llvm.build_bitcast
                   f_self_param
                   opaque_pointer
                   (Helpers.register_name_of_variable rec_name))
            in
            let rec_p = EPS.Parameter.Variable rec_name in
            List.zip_exn (rec_p :: ps) (f_self_param :: params)
          | None ->
            Llvm.set_value_name "null" f_self_param;
            ignore (f_self_param : Llvm.llvalue);
            List.zip_exn ps params |> return
        in
        (* parameters available via variables in the the body*)
        let (env_parameters : Context.Parameters.t) =
          List.filter_map parameters ~f:(fun (p, value) ->
              match p with
              | EPS.Parameter.Variable name ->
                Llvm.set_value_name
                  (Helpers.register_name_of_variable name)
                  value;
                Some (name, value)
              | EPS.Parameter.Wildcard ->
                Llvm.set_value_name "_" value;
                None)
        in
        List.iter env_parameters ~f:(fun (name, value) ->
            Llvm.set_value_name (Helpers.register_name_of_variable name) value);
        Llvm.set_value_name "closure" closure_param;
        let captured =
          { Context.Closure.shape = captured_shape; closure = closure_param }
        in
        (* compile [e_body] in extended environment (capture variables which
           escaped from the containing function) *)
        let env' =
          Context.With_parameters
            { closure = captured; parameters = env_parameters }
        in
        let%bind result =
          compile_expr e_body ~env:env' ~runtime ~effect_reprs ~outer_symbol
        in
        let%map _return = Codegen.use_builder (Llvm.build_ret result) in
        ())
  in
  function_

(** a special case of [compile_function], where the function's symbol name is
    based on the containing function's name ([outer_symbol]) *)
and compile_local_function
    :  rec_name:Variable.t option -> EPS.Parameter.t list -> EPS.Expr.t
    -> captured_shape:Context.Closure.Shape.t -> outer_symbol:Symbol_name.t
    -> runtime:Runtime.t -> effect_reprs:Effect_repr.t Effect_label.Map.t
    -> Llvm.llvalue Codegen.t
  =
 fun ~rec_name ps e_body ~captured_shape ~outer_symbol ~runtime ~effect_reprs ->
  let open Codegen.Let_syntax in
  let%bind symbol_name, outer_symbol =
    match rec_name with
    | Some name ->
      let%map symbol_name =
        Codegen.symbol_of_local_name ~containing:outer_symbol name
      in
      symbol_name, symbol_name
    | None ->
      let%map symbol_name =
        Codegen.symbol_of_local_anonymous ~containing:outer_symbol
      in
      symbol_name, outer_symbol
  in
  compile_function
    ~symbol_name
    ~rec_name
    ps
    e_body
    ~captured_shape
    ~outer_symbol
    ~runtime
    ~effect_reprs

(** [compile_lambda lambda ...] compiles lambda as a function, and generates
    code to construct a function object of it in the given [env]. The result is
    a [Types.opaque_pointer] to it. *)
and compile_lambda
    :  EPS.Expr.lambda -> env:Context.t -> outer_symbol:Symbol_name.t
    -> runtime:Runtime.t -> effect_reprs:Effect_repr.t Effect_label.Map.t
    -> Llvm.llvalue Codegen.t
  =
 fun (ps, e_body) ~env ~outer_symbol ~runtime ~effect_reprs ->
  let open Codegen.Let_syntax in
  (* TODO: this rebuilds the escaping closure on every capture - cheaper to
     build it once at function entry (keep it around in the context?) *)
  (* ecaping closure contains everything in [env]: parameters and closure *)
  let%bind escaping = Context.compile_capture env ~runtime in
  let { Context.Closure.shape = escaping_shape; closure = escaping_closure } =
    escaping
  in
  let%bind function_code =
    compile_local_function
      ~rec_name:None
      ps
      e_body
      ~captured_shape:escaping_shape
      ~outer_symbol
      ~runtime
      ~effect_reprs
  in
  compile_construct_function_object
    function_code
    ~is_recursive:false
    ~captured_closure:escaping_closure
    ~runtime

(** [compile_fix_lambda lambda ...] compiles a recursive lambda as a function,
    and generates code to construct a function object of it in the given [env].
    The result is a [Types.opaque_pointer] to it. *)
and compile_fix_lambda
    :  EPS.Expr.fix_lambda -> env:Context.t -> outer_symbol:Symbol_name.t
    -> runtime:Runtime.t -> effect_reprs:Effect_repr.t Effect_label.Map.t
    -> Llvm.llvalue Codegen.t
  =
 fun (f, (ps, e_body)) ~env ~outer_symbol ~runtime ~effect_reprs ->
  let open Codegen.Let_syntax in
  let%bind escaping = Context.compile_capture env ~runtime in
  let { Context.Closure.shape = escaping_shape; closure = escaping_closure } =
    escaping
  in
  let%bind function_code =
    compile_local_function
      ~rec_name:(Some f)
      ps
      e_body
      ~captured_shape:escaping_shape
      ~outer_symbol
      ~runtime
      ~effect_reprs
  in
  compile_construct_function_object
    function_code
    ~is_recursive:true
    ~captured_closure:escaping_closure
    ~runtime

(** [compile_application f args] generates code to 'call' the function object
    pointed to by [Types.opaque_pointer]:[f], with the given arguments. *)
and compile_application
    : Llvm.llvalue -> Llvm.llvalue list -> Llvm.llvalue Codegen.t
  =
 fun f_ptr arg_ptrs ->
  let open Codegen.Let_syntax in
  (* cast v_f to function type *)
  let%bind function_object_type = Types.function_object in
  let function_object_ptr_type = Llvm.pointer_type function_object_type in
  let%bind f_ptr =
    Codegen.use_builder
      (Llvm.build_bitcast f_ptr function_object_ptr_type "function_ptr")
  in
  (* extract fields of f *)
  let%bind code_address_opaque =
    Helpers.compile_access_field f_ptr 0 "code_address"
  in
  let%bind closure_ptr = Helpers.compile_access_field f_ptr 1 "closure" in
  let%bind is_recursive = Helpers.compile_access_field f_ptr 2 "is_recursive" in
  (* the number of arguments passed in the evidence passing representation *)
  let num_eps_args = List.length arg_ptrs in
  let%bind generated_function_type = Types.function_code num_eps_args in
  let generated_function_ptr_type = Llvm.pointer_type generated_function_type in
  (* pass either [f_ptr] or [null] depending on whether the function is
     recursive *)
  let null_function = Llvm.const_pointer_null function_object_ptr_type in
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
    :  Effect_label.t -> EPS.Expr.t Variable.Map.t -> env:Context.t
    -> runtime:Runtime.t -> effect_reprs:Effect_repr.t Effect_label.Map.t
    -> outer_symbol:Symbol_name.t -> Llvm.llvalue Codegen.t
  =
 fun handled_effect
     operation_clause_exprs
     ~env
     ~runtime
     ~effect_reprs
     ~outer_symbol ->
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
        let%map clause =
          compile_expr e_clause ~env ~runtime ~effect_reprs ~outer_symbol
        in
        clause, Helpers.register_name_of_variable op_name)
    |> Codegen.all
  in
  let%bind handler_ptr = Helpers.heap_allocate hnd_type "hnd" ~runtime in
  let%bind () =
    Helpers.compile_populate_struct handler_ptr operation_clauses_and_names
  in
  let%bind opaque_pointer = Types.opaque_pointer in
  Codegen.use_builder (Llvm.build_bitcast handler_ptr opaque_pointer "hnd_ptr")

and compile_impure_built_in
    :  EPS.Expr.impure_built_in -> env:Context.t -> runtime:Runtime.t
    -> effect_reprs:Effect_repr.t Effect_label.Map.t
    -> outer_symbol:Symbol_name.t -> Llvm.llvalue Codegen.t
  =
 fun impure ~env ~runtime ~effect_reprs ~outer_symbol ->
  let open Codegen.Let_syntax in
  match impure with
  | EPS.Expr.Impure_print_int e ->
    let%bind v = compile_expr e ~env ~runtime ~effect_reprs ~outer_symbol in
    let%bind i = Helpers.dereference_int v in
    let { Runtime.print_int; _ } = runtime in
    let%bind _void =
      Codegen.use_builder (Llvm.build_call print_int (Array.of_list [ i ]) "")
    in
    Helpers.heap_store_unit ~runtime
  | EPS.Expr.Impure_read_int ->
    let { Runtime.read_int; _ } = runtime in
    let%bind i =
      Codegen.use_builder (Llvm.build_call read_int (Array.of_list []) "i")
    in
    Helpers.heap_store_int i ~runtime
;;

(** creates the representation of a declared effect. This builds the type of
    handlers for that effect to be a struct with as many [Types.opaque_pointer]
    fields as there are operations *)
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

(** [compile_fun_decl decl ~toplevel_names ...] compiles a toplevel function
    declaration into an llvm function, and returns that function's [llvalue] and
    name *)
let compile_fun_decl
    :  EPS.Program.Fun_decl.t -> toplevel_names:Variable.t list
    -> runtime:Runtime.t -> effect_reprs:Effect_repr.t Effect_label.Map.t
    -> (Variable.t * Llvm.llvalue) Codegen.t
  =
 fun decl ~toplevel_names ~runtime ~effect_reprs ->
  let open Codegen.Let_syntax in
  let f_name, (ps, e_body) = decl in
  let captured_shape = Context.Closure.Shape.Toplevel toplevel_names in
  let symbol_name = Symbol_name.of_toplevel f_name in
  let%map code =
    compile_function
      ~symbol_name
      ~rec_name:(Some f_name)
      ps
      e_body
      ~captured_shape
      ~outer_symbol:symbol_name
      ~runtime
      ~effect_reprs
  in
  f_name, code
;;

(** compile the given functions in order, allowing each to access the ones
    before it through its context. Returns a associative list from names to
    functions' [llvalues] (i.e. code addresses), in the order they should appear
    in the toplevel closure *)
let compile_fun_decls
    :  EPS.Program.Fun_decl.t list -> runtime:Runtime.t
    -> effect_reprs:Effect_repr.t Effect_label.Map.t
    -> (Variable.t * Llvm.llvalue) list Codegen.t
  =
 fun decls ~runtime ~effect_reprs ->
  let open Codegen.Let_syntax in
  Codegen.list_fold decls ~init:[] ~f:(fun defined decl ->
      let toplevel_names = List.map defined ~f:(fun (name, _code) -> name) in
      let%map name, code =
        compile_fun_decl decl ~toplevel_names ~runtime ~effect_reprs
      in
      defined @ [ name, code ])
;;

let compile_program : EPS.Program.t -> unit Codegen.t =
 fun { EPS.Program.effect_declarations; fun_declarations } ->
  let open Codegen.Let_syntax in
  let%bind effect_reprs = compile_effect_decls effect_declarations in
  let%bind runtime = Runtime.declare in
  (* compile toplevel functions, returning a list of their llvalues *)
  let%bind named_code_addresses =
    compile_fun_decls fun_declarations ~runtime ~effect_reprs
  in
  (* make `main` *)
  let%bind main_type = Types.main_function in
  let%bind main_function =
    Codegen.use_module
      (Llvm.define_function (Symbol_name.to_string Symbol_name.main) main_type)
  in
  let main_start_block = Llvm.entry_block main_function in
  let%bind () = Codegen.use_builder (Llvm.position_at_end main_start_block) in
  let { Runtime.init; _ } = runtime in
  let%bind _void =
    Codegen.use_builder (Llvm.build_call init (Array.of_list []) "")
  in
  (* build a function object for each toplevel function *)
  (* putting [null] for the parent for now *)
  let%bind closure_type = Types.closure in
  let closure_ptr_type = Llvm.pointer_type closure_type in
  let null_closure = Llvm.const_pointer_null closure_ptr_type in
  let%bind named_function_objects =
    List.map named_code_addresses ~f:(fun (name, code) ->
        let%map f =
          compile_construct_function_object
            code
            ~is_recursive:true
            ~captured_closure:null_closure
            ~runtime
        in
        name, f)
    |> Codegen.all
  in
  (* build the toplevel closure using the defined toplevel functions' function
     objects' addresses *)
  let%bind (toplevel_closure : Context.Closure.t) =
    Context.Closure.compile_make_toplevel named_function_objects ~runtime
  in
  (* now update each function object to point back to the closure *)
  let%bind () =
    List.map named_function_objects ~f:(fun (_name, f_opaque) ->
        let%bind function_object_type = Types.function_object in
        let function_object_ptr_type = Llvm.pointer_type function_object_type in
        let%bind f =
          Codegen.use_builder
            (Llvm.build_bitcast
               f_opaque
               function_object_ptr_type
               "function_object_ptr")
        in
        (* currently pointing at null - update it *)
        let%bind closure_field_ptr =
          Codegen.use_builder (Llvm.build_struct_gep f 1 "closure_field_ptr")
        in
        let { Context.Closure.closure; _ } = toplevel_closure in
        let%map _store =
          Codegen.use_builder (Llvm.build_store closure closure_field_ptr)
        in
        ())
    |> Codegen.all_unit
  in
  let env = Context.Toplevel toplevel_closure in
  (* compile a call to `entry_point` - or should this be kept as an expression
     rather than a function? *)
  (* [entry_point()(runtime.nil_evidence_vector)] *)
  let invocation =
    EPS.Expr.Application
      ( EPS.Expr.Application (EPS.Expr.Variable EPS.Keyword.entry_point, [])
      , [ EPS.Expr.Nil_evidence_vector ] )
  in
  let%bind _unit =
    compile_expr
      invocation
      ~env
      ~outer_symbol:Symbol_name.main
      ~runtime
      ~effect_reprs
  in
  (* return 0 *)
  let%bind i32 = Codegen.use_context Llvm.i32_type in
  let return_code = Llvm.const_int i32 0 in
  let%bind _return = Codegen.use_builder (Llvm.build_ret return_code) in
  Codegen.check_module_valid
;;

let module_name_or_default = Option.value ~default:"anonymous"

let compile_then_write_program ?module_name ~filename program =
  let module_id = module_name_or_default module_name in
  Codegen.run_then_write_module ~module_id ~filename (compile_program program)
;;

let compile_then_dump_program ?module_name program =
  let module_id = module_name_or_default module_name in
  Codegen.run_then_dump_module ~module_id (compile_program program)
;;
