open Core
open Import
module EPS = Evidence_passing_syntax

(* TODO: need a type system for telling when llvalues are
   pointers(opaque/typed)/values/*)

(** get an effect's representation, or fail with a codegen impossible_error if
    not found *)
let lookup_effect_repr
  :  Effect_repr.t Effect_label.Map.t
  -> Effect_label.t
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
let compile_literal (lit : Literal.t) =
  let open Codegen.Let_syntax in
  match lit with
  | Literal.Int i ->
    let%bind v = Immediate_repr.Int.const i in
    Immediate_repr.Int.to_opaque v
  | Literal.Bool b ->
    let%bind v = Immediate_repr.Bool.const_bool b in
    Immediate_repr.Bool.to_opaque v
  | Literal.Unit -> Immediate_repr.Unit.const_opaque ()
;;

(** takes values which are [Types.opaque_pointer]s to the evaluated operands,
    and generates code to evaluate the operator and store the result on the
    heap. *)
let compile_binary_operator =
  fun ~left (op : Operator.t) ~right ->
  let open Codegen.Let_syntax in
  match op with
  | Operator.Bool bool_op ->
    let%bind left = Immediate_repr.Bool.of_opaque left in
    let%bind right = Immediate_repr.Bool.of_opaque right in
    Immediate_repr.Bool.compile_binary_operation left bool_op right
  | Operator.Int int_op ->
    let%bind left = Immediate_repr.Int.of_opaque left in
    let%bind right = Immediate_repr.Int.of_opaque right in
    Immediate_repr.Int.compile_binary_operation left int_op right
;;

let compile_unary_operator arg op =
  let open Codegen.Let_syntax in
  match (op : Operator.Unary.t) with
  | Bool op ->
    let%bind arg = Immediate_repr.Bool.of_opaque arg in
    Immediate_repr.Bool.compile_unary_operation op arg
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
  let%bind ctl_pure_type = Types.ctl_pure in
  (* always allocate a [ctl], then cast, to be sure of correct alignment *)
  let%bind ctl_ptr = Helpers.heap_allocate ctl_type "ctl" ~runtime in
  let%bind tag = Helpers.const_ctl_pure_tag in
  let%map () =
    Helpers.compile_populate_struct
      ctl_ptr
      ~struct_type:ctl_pure_type
      [ tag, "tag"; x, "value" ]
  in
  ctl_ptr
;;

(** [compile_construct_yield ~marker ~op_clause ~resumption ~runtime] produces
    code which heap allocates and populates a [Types.ctl_yield] struct,
    returning a [Types.opaque_pointer] to it *)
let compile_construct_yield
  :  marker:Llvm.llvalue
  -> op_clause:Llvm.llvalue
  -> resumption:Llvm.llvalue
  -> runtime:Runtime.t
  -> Llvm.llvalue Codegen.t
  =
  fun ~marker ~op_clause ~resumption ~runtime ->
  let open Codegen.Let_syntax in
  let%bind ctl_type = Types.ctl in
  let%bind ctl_yield_type = Types.ctl_yield in
  let%bind ctl_ptr = Helpers.heap_allocate ctl_type "ctl" ~runtime in
  let%bind tag = Helpers.const_ctl_yield_tag in
  let%map () =
    Helpers.compile_populate_struct
      ctl_ptr
      ~struct_type:ctl_yield_type
      [ tag, "tag"
      ; marker, "marker"
      ; op_clause, "op_clause"
      ; resumption, "resumption"
      ]
  in
  ctl_ptr
;;

(** [compile_construct_op variant clause ...] generates code to allocate a
    [Types.op] and populate it with the given [variant] and operation [clause]
*)
let compile_construct_op
  :  [ `Normal | `Tail ]
  -> Llvm.llvalue
  -> runtime:Runtime.t
  -> Llvm.llvalue Codegen.t
  =
  fun tag clause ~runtime ->
  let open Codegen.Let_syntax in
  let%bind op_type = Types.op in
  let%bind op_ptr = Helpers.heap_allocate op_type "op" ~runtime in
  let%bind tag =
    match tag with
    | `Normal -> Helpers.const_op_normal_tag
    | `Tail -> Helpers.const_op_tail_tag
  in
  let%map () =
    Helpers.compile_populate_struct
      op_ptr
      ~struct_type:op_type
      [ tag, "tag"; clause, "clause" ]
  in
  op_ptr
;;

(** [compile_select_operation label ~op_name v ~effect_reprs] generates code
    which selects the operation clause for [op_name] from [v] (a handler for the
    effect [label]) *)
let compile_select_operation
  :  Effect_label.t
  -> op_name:Variable.t
  -> Llvm.llvalue
  -> effect_reprs:Effect_repr.t Effect_label.Map.t
  -> Llvm.llvalue Codegen.t
  =
  fun label ~op_name handler ~effect_reprs ->
  let open Codegen.Let_syntax in
  let%bind repr = lookup_effect_repr effect_reprs label in
  let { Effect_repr.hnd_type; operations; _ } = repr in
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
      (Llvm.build_struct_gep hnd_type handler op_index "op_clause_field_ptr")
  in
  let%bind pointer_type = Types.pointer in
  Codegen.use_builder
    (Llvm.build_load pointer_type op_clause_field_ptr "op_clause")
;;

(** [compile_construct_function_object code_address ~is_recursive ~captured_closure ...]
    generates code to heap allocate and populate a [Types.function_object], with
    code pointed to by the (typed or opaque) [code_address], keeping
    [captured_closure] as it's closure. *)
let compile_construct_function_object
  :  Llvm.llvalue
  -> is_recursive:bool
  -> captured_closure:Llvm.llvalue
  -> runtime:Runtime.t
  -> Llvm.llvalue Codegen.t
  =
  fun code_address ~is_recursive ~captured_closure ~runtime ->
  let open Codegen.Let_syntax in
  let%bind function_object_type = Types.function_object in
  let%bind function_ptr =
    Helpers.heap_allocate function_object_type "function" ~runtime
  in
  let%bind i1 = Codegen.use_context Llvm.i1_type in
  let is_recursive = Llvm.const_int i1 (if is_recursive then 1 else 0) in
  let fields =
    [ code_address, "code"
    ; captured_closure, "closure"
    ; is_recursive, "is_recursive"
    ]
  in
  let%map () =
    Helpers.compile_populate_struct
      function_ptr
      ~struct_type:function_object_type
      fields
  in
  function_ptr
;;

(** [compile_match_corrupted_tag ...] generates a default branch for a match
    statement, executed when a variants tag has an unexpected value. This exits
    the program, and results in a null [Types.opaque_pointer] to satisfy llvm's
    type system *)
let compile_match_corrupted_tag ~runtime () =
  let open Codegen.Let_syntax in
  let { Runtime.exit; _ } = runtime in
  let%bind _void =
    Runtime.Function.build_call exit ~args:(Array.of_list []) ""
  in
  let%map pointer_type = Types.pointer in
  Llvm.const_null pointer_type
;;

(** produces code to evaluate the given expression and store its value to the
    heap. The returned [llvalue] is the [Types.opaque_pointer] to this value. *)
let rec compile_expr
  :  EPS.Expr.t
  -> env:Context.t
  -> runtime:Runtime.t
  -> effect_reprs:Effect_repr.t Effect_label.Map.t
  -> outer_symbol:Symbol_name.t
  -> Llvm.llvalue Codegen.t
  =
  fun e ~env ~runtime ~effect_reprs ~outer_symbol ->
  let open Codegen.Let_syntax in
  match e with
  | EPS.Expr.Variable v ->
    (* TODO: note this will be duplicated for each access, although common
       subexpression elimination should easily remove it *)
    Context.compile_get env v
  | EPS.Expr.Let (p, e_subject, e_body) ->
    let%bind subject =
      compile_expr e_subject ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let env' =
      match p with
      | Parameter.Wildcard -> env
      | Parameter.Variable v -> Context.add_local_exn env ~name:v ~value:subject
    in
    compile_expr e_body ~env:env' ~runtime ~effect_reprs ~outer_symbol
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
  | EPS.Expr.Literal lit -> compile_literal lit
  | EPS.Expr.If_then_else (e_cond, e_yes, e_no) ->
    let%bind cond_ptr =
      compile_expr e_cond ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind cond = Immediate_repr.Bool.of_opaque cond_ptr in
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
    compile_unary_operator arg op
  | EPS.Expr.Operator (e_left, op, e_right) ->
    let%bind left =
      compile_expr e_left ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind right =
      compile_expr e_right ~env ~runtime ~effect_reprs ~outer_symbol
    in
    compile_binary_operator ~left op ~right
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
      Runtime.Function.build_call
        fresh_marker
        ~args:(Array.of_list [])
        "fresh_marker"
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
      Runtime.Function.build_call
        markers_equal
        ~args:(Array.of_list [ marker1; marker2 ])
        "markers_equal"
    in
    let eq = Immediate_repr.Bool.of_bool_llvalue eq in
    Immediate_repr.Bool.to_opaque eq
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
  | EPS.Expr.Construct_op_normal e_clause ->
    let%bind clause =
      compile_expr e_clause ~env ~runtime ~effect_reprs ~outer_symbol
    in
    compile_construct_op `Normal clause ~runtime
  | EPS.Expr.Construct_op_tail e_clause ->
    let%bind clause =
      compile_expr e_clause ~env ~runtime ~effect_reprs ~outer_symbol
    in
    compile_construct_op `Tail clause ~runtime
  | EPS.Expr.Match_op { subject = e_subject; normal_branch; tail_branch } ->
    let%bind subject =
      compile_expr e_subject ~env ~runtime ~effect_reprs ~outer_symbol
    in
    compile_match_op
      subject
      ~normal_branch
      ~tail_branch
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
    Runtime.Function.build_call
      nil_evidence_vector
      ~args:(Array.of_list [])
      "nil_vector"
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
    Runtime.Function.build_call cons_evidence_vector ~args "extended_vector"
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
    Runtime.Function.build_call evidence_vector_lookup ~args "evidence"
  | EPS.Expr.Get_evidence_marker e ->
    let%bind evidence =
      compile_expr e ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let { Runtime.get_evidence_marker; _ } = runtime in
    let%bind marker =
      Runtime.Function.build_call
        get_evidence_marker
        ~args:(Array.of_list [ evidence ])
        "marker"
    in
    Helpers.heap_store_marker marker ~runtime
  | EPS.Expr.Get_evidence_handler e ->
    let%bind evidence =
      compile_expr e ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let { Runtime.get_evidence_handler; _ } = runtime in
    Runtime.Function.build_call
      get_evidence_handler
      ~args:(Array.of_list [ evidence ])
      "handler"
  | EPS.Expr.Get_evidence_handler_site_vector e ->
    let%bind evidence =
      compile_expr e ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let { Runtime.get_evidence_handler_site_vector; _ } = runtime in
    Runtime.Function.build_call
      get_evidence_handler_site_vector
      ~args:(Array.of_list [ evidence ])
      "handler_site_vector"
  | EPS.Expr.Impure_built_in impure ->
    compile_impure_built_in impure ~env ~runtime ~effect_reprs ~outer_symbol

(** [compile_match_ctl subject ~pure_branch ~yield_branch ...] generates code to
    branch on the ctl varaint poitned to by [Types.opaque_pointer]:[subject],
    calling either [pure_branch] or [yield_branch] with its fields. (These are
    also compiled) *)
and compile_match_ctl
  :  Llvm.llvalue
  -> pure_branch:Variable.t * EPS.Expr.t
  -> yield_branch:Variable.t * Variable.t * Variable.t * EPS.Expr.t
  -> env:Context.t
  -> runtime:Runtime.t
  -> effect_reprs:Effect_repr.t Effect_label.Map.t
  -> outer_symbol:Symbol_name.t
  -> Llvm.llvalue Codegen.t
  =
  fun subject
    ~pure_branch
    ~yield_branch
    ~env
    ~runtime
    ~effect_reprs
    ~outer_symbol ->
  let open Codegen.Let_syntax in
  let%bind ctl_type = Types.ctl in
  let%bind tag =
    Helpers.compile_access_field subject ~struct_type:ctl_type ~i:0 "tag"
  in
  let%bind pure_tag = Helpers.const_ctl_pure_tag in
  let%bind yield_tag = Helpers.const_ctl_yield_tag in
  let compile_pure () =
    let%bind ctl_pure_type = Types.ctl_pure in
    let x_value, body = pure_branch in
    let%bind value =
      Helpers.compile_access_field
        subject
        ~struct_type:ctl_pure_type
        ~i:1
        (Helpers.register_name_of_variable x_value)
    in
    let env' = Context.add_local_exn env ~name:x_value ~value in
    compile_expr body ~env:env' ~runtime ~effect_reprs ~outer_symbol
  in
  let compile_yield () =
    let%bind ctl_yield_type = Types.ctl_yield in
    let x_marker, x_op_clause, x_resumption, body = yield_branch in
    let%bind marker =
      Helpers.compile_access_field
        subject
        ~struct_type:ctl_yield_type
        ~i:1
        (Helpers.register_name_of_variable x_marker)
    in
    let%bind op_clause =
      Helpers.compile_access_field
        subject
        ~struct_type:ctl_yield_type
        ~i:2
        (Helpers.register_name_of_variable x_op_clause)
    in
    let%bind resumption =
      Helpers.compile_access_field
        subject
        ~struct_type:ctl_yield_type
        ~i:3
        (Helpers.register_name_of_variable x_resumption)
    in
    let env' =
      env
      |> Context.add_local_exn ~name:x_marker ~value:marker
      |> Context.add_local_exn ~name:x_op_clause ~value:op_clause
      |> Context.add_local_exn ~name:x_resumption ~value:resumption
    in
    compile_expr body ~env:env' ~runtime ~effect_reprs ~outer_symbol
  in
  Helpers.compile_switch
    tag
    ~table:
      [ pure_tag, "ctl_pure", compile_pure
      ; yield_tag, "ctl_yield", compile_yield
      ]
    ~compile_default:(compile_match_corrupted_tag ~runtime)

(** [compile_match_op subject ~normal_branch ~tail_branch ...] compiles a match
    statement on a [Types.op] pointed to by [subject], calling either
    normal/tail with the op's clause *)
and compile_match_op
  :  Llvm.llvalue
  -> normal_branch:Variable.t * EPS.Expr.t
  -> tail_branch:Variable.t * EPS.Expr.t
  -> env:Context.t
  -> runtime:Runtime.t
  -> effect_reprs:Effect_repr.t Effect_label.Map.t
  -> outer_symbol:Symbol_name.t
  -> Llvm.llvalue Codegen.t
  =
  fun subject
    ~normal_branch
    ~tail_branch
    ~env
    ~runtime
    ~effect_reprs
    ~outer_symbol ->
  let open Codegen.Let_syntax in
  let%bind op_type = Types.op in
  let%bind tag =
    Helpers.compile_access_field subject ~struct_type:op_type ~i:0 "tag"
  in
  let%bind normal_tag = Helpers.const_op_normal_tag in
  let%bind tail_tag = Helpers.const_op_tail_tag in
  let make_compile_branch branch () =
    let x, body = branch in
    let%bind clause =
      Helpers.compile_access_field
        subject
        ~struct_type:op_type
        ~i:1
        (Helpers.register_name_of_variable x)
    in
    let env' = Context.add_local_exn env ~name:x ~value:clause in
    compile_expr body ~env:env' ~runtime ~effect_reprs ~outer_symbol
  in
  Helpers.compile_switch
    tag
    ~table:
      [ normal_tag, "op_normal", make_compile_branch normal_branch
      ; tail_tag, "op_tail", make_compile_branch tail_branch
      ]
    ~compile_default:(compile_match_corrupted_tag ~runtime)

(** [compile_if_then_else b ~e_yes ~e_no ~env ~runtime ~effect_reprs ~outer_symbol]
    generates code to branch on the value of the [Types.bool] [b], and evaluate
    to the value of either [e_yes] or [e_no] *)
and compile_if_then_else =
  fun (cond : Immediate_repr.Bool.t)
    ~(e_yes : EPS.Expr.t)
    ~(e_no : EPS.Expr.t)
    ~env
    ~runtime
    ~effect_reprs
    ~outer_symbol ->
  let open Codegen.Let_syntax in
  let%bind cond_i1 = Immediate_repr.Bool.to_i1 cond in
  Helpers.compile_conditional
    ~cond_i1
    ~compile_true:(fun () ->
      compile_expr e_yes ~env ~runtime ~effect_reprs ~outer_symbol)
    ~compile_false:(fun () ->
      compile_expr e_no ~env ~runtime ~effect_reprs ~outer_symbol)

(** [compile_function ~symbol_name rec_name ps e_body ~captured_shape ~outer_symbol ...]
    generates a function with the given arguments and body, and within the scope
    given by [captured_shape]. It returns this function's [llvalue]. The
    [llbuilder]'s insertion point is saved and restored. *)
and compile_function
  :  symbol_name:Symbol_name.t
  -> rec_name:Variable.t option
  -> Parameter.t list
  -> EPS.Expr.t
  -> captured_shape:Context.Closure.Shape.t
  -> toplevel:Context.Toplevel.t
  -> outer_symbol:Symbol_name.t
  -> runtime:Runtime.t
  -> effect_reprs:Effect_repr.t Effect_label.Map.t
  -> Llvm.llvalue Codegen.t
  =
  fun ~symbol_name
    ~rec_name
    ps
    e_body
    ~captured_shape
    ~toplevel
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
          raise_s [%message "function type has unexpected number of parameters"]
      in
      let (parameters : (Parameter.t * Llvm.llvalue) list) =
        match rec_name with
        | Some rec_name ->
          let rec_p = Parameter.Variable rec_name in
          List.zip_exn (rec_p :: ps) (f_self_param :: params)
        | None ->
          Llvm.set_value_name "null" f_self_param;
          ignore (f_self_param : Llvm.llvalue);
          List.zip_exn ps params
      in
      (* parameters available via variables in the the body*)
      let (env_locals : Context.Locals.t) =
        List.filter_map parameters ~f:(fun (p, value) ->
          match p with
          | Parameter.Variable name ->
            Llvm.set_value_name (Helpers.register_name_of_variable name) value;
            Some (name, value)
          | Parameter.Wildcard ->
            Llvm.set_value_name "_" value;
            None)
      in
      List.iter env_locals ~f:(fun (name, value) ->
        Llvm.set_value_name (Helpers.register_name_of_variable name) value);
      Llvm.set_value_name "closure" closure_param;
      let captured =
        { Context.Closure.shape = captured_shape; closure = closure_param }
      in
      (* compile [e_body] in extended environment (capture variables which
         escaped from the containing function) *)
      let env' =
        { Context.toplevel; closure = captured; locals = env_locals }
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
  :  rec_name:Variable.t option
  -> Parameter.t list
  -> EPS.Expr.t
  -> captured_shape:Context.Closure.Shape.t
  -> toplevel:Context.Toplevel.t
  -> outer_symbol:Symbol_name.t
  -> runtime:Runtime.t
  -> effect_reprs:Effect_repr.t Effect_label.Map.t
  -> Llvm.llvalue Codegen.t
  =
  fun ~rec_name
    ps
    e_body
    ~captured_shape
    ~toplevel
    ~outer_symbol
    ~runtime
    ~effect_reprs ->
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
    ~toplevel
    ~outer_symbol
    ~runtime
    ~effect_reprs

(** a helper function used by [compile_lambda] and [compile_fix_lambda] *)
and compile_lambda_like
  :  [ `Fix_lambda of EPS.Expr.fix_lambda | `Lambda of EPS.Expr.lambda ]
  -> env:Context.t
  -> outer_symbol:Symbol_name.t
  -> runtime:Runtime.t
  -> effect_reprs:Effect_repr.t Effect_label.Map.t
  -> Llvm.llvalue Codegen.t
  =
  fun lambda_like ~env ~outer_symbol ~runtime ~effect_reprs ->
  let open Codegen.Let_syntax in
  let free =
    match lambda_like with
    | `Fix_lambda fix_lambda -> Free_variables.free_in_fix_lambda fix_lambda
    | `Lambda lambda -> Free_variables.free_in_lambda lambda
  in
  (* capture all free varaibles which are local (non-local ones are already in
     the closure, so just chain)*)
  let%bind escaping = Context.compile_capture env ~free ~runtime in
  let { Context.Closure.shape = escaping_shape; closure = escaping_closure } =
    escaping
  in
  let rec_name, is_recursive, (params, e_body) =
    match lambda_like with
    | `Fix_lambda (rec_name, lambda) -> Some rec_name, true, lambda
    | `Lambda lambda -> None, false, lambda
  in
  let%bind function_code =
    compile_local_function
      ~rec_name
      params
      e_body
      ~captured_shape:escaping_shape
      ~toplevel:env.toplevel
      ~outer_symbol
      ~runtime
      ~effect_reprs
  in
  match Context.Closure.is_empty escaping with
  | true ->
    (* functions with no free variables are simply code pointers,
       with no heap allocation required *)
    Function_repr.compile_wrap_callable (Code_pointer function_code)
  | false ->
    (* closures are allocated on the heap *)
    compile_construct_function_object
      function_code
      ~is_recursive
      ~captured_closure:escaping_closure
      ~runtime

(** [compile_lambda lambda ...] compiles a lambda as a function, and generates
    code to construct a function object of it in the given [env]. The result is
    a [Types.opaque_pointer] to it. *)
and compile_lambda
  :  EPS.Expr.lambda
  -> env:Context.t
  -> outer_symbol:Symbol_name.t
  -> runtime:Runtime.t
  -> effect_reprs:Effect_repr.t Effect_label.Map.t
  -> Llvm.llvalue Codegen.t
  =
  fun lambda ~env ~outer_symbol ~runtime ~effect_reprs ->
  compile_lambda_like (`Lambda lambda) ~env ~outer_symbol ~runtime ~effect_reprs

(** [compile_fix_lambda fix_lambda ...] compiles a recursive lambda as a
    function, and generates code to construct a function object of it in the
    given [env]. The result is a [Types.opaque_pointer] to it. *)
and compile_fix_lambda
  :  EPS.Expr.fix_lambda
  -> env:Context.t
  -> outer_symbol:Symbol_name.t
  -> runtime:Runtime.t
  -> effect_reprs:Effect_repr.t Effect_label.Map.t
  -> Llvm.llvalue Codegen.t
  =
  fun fix_lambda ~env ~outer_symbol ~runtime ~effect_reprs ->
  compile_lambda_like
    (`Fix_lambda fix_lambda)
    ~env
    ~outer_symbol
    ~runtime
    ~effect_reprs

(** [compile_application f args] generates code to 'call' the function object
    pointed to by [f], with the given arguments. *)
and compile_application
  : Llvm.llvalue -> Llvm.llvalue list -> Llvm.llvalue Codegen.t
  =
  fun f_ptr arg_ptrs ->
  let open Codegen.Let_syntax in
  let compile_call ~code_address ~f_self ~closure_ptr =
    (* the number of arguments passed in the evidence passing representation *)
    let num_eps_args = List.length arg_ptrs in
    let%bind generated_function_type = Types.function_code num_eps_args in
    let args = Array.of_list (f_self :: closure_ptr :: arg_ptrs) in
    Codegen.use_builder
      (Llvm.build_call generated_function_type code_address args "result")
  in
  Function_repr.compile_use_callable
    (Maybe_tagged f_ptr)
    ~compile_use_code_pointer:(fun code_address ->
      (* TODO: stop doing this is_recursive nonsense *)
      let%bind { closure; _ } = Context.Closure.empty in
      compile_call ~code_address ~f_self:f_ptr ~closure_ptr:closure)
    ~compile_use_function_object_pointer:(fun f_ptr ->
      (* cast v_f to function type *)
      let%bind function_object_type = Types.function_object in
      (* extract fields of f *)
      let%bind code_address =
        Helpers.compile_access_field
          f_ptr
          ~struct_type:function_object_type
          ~i:0
          "code_address"
      in
      let%bind closure_ptr =
        Helpers.compile_access_field
          f_ptr
          ~struct_type:function_object_type
          ~i:1
          "closure"
      in
      let%bind is_recursive =
        Helpers.compile_access_field
          f_ptr
          ~struct_type:function_object_type
          ~i:2
          "is_recursive"
      in
      let%bind pointer_type = Types.pointer in
      (* pass either [f_ptr] or [null] depending on whether the function is
         recursive *)
      let null_function = Llvm.const_pointer_null pointer_type in
      let%bind f_self =
        Codegen.use_builder
          (Llvm.build_select is_recursive f_ptr null_function "f_self")
      in
      compile_call ~code_address ~f_self ~closure_ptr)

and compile_construct_handler
  :  Effect_label.t
  -> EPS.Expr.t Variable.Map.t
  -> env:Context.t
  -> runtime:Runtime.t
  -> effect_reprs:Effect_repr.t Effect_label.Map.t
  -> outer_symbol:Symbol_name.t
  -> Llvm.llvalue Codegen.t
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
  let%map () =
    Helpers.compile_populate_struct
      ~struct_type:hnd_type
      handler_ptr
      operation_clauses_and_names
  in
  handler_ptr

and compile_impure_built_in
  :  EPS.Expr.impure_built_in
  -> env:Context.t
  -> runtime:Runtime.t
  -> effect_reprs:Effect_repr.t Effect_label.Map.t
  -> outer_symbol:Symbol_name.t
  -> Llvm.llvalue Codegen.t
  =
  fun impure ~env ~runtime ~effect_reprs ~outer_symbol ->
  let open Codegen.Let_syntax in
  match impure with
  | EPS.Expr.Impure_println ->
    let { Runtime.println; _ } = runtime in
    let%bind _void =
      Runtime.Function.build_call println ~args:(Array.of_list []) ""
    in
    Immediate_repr.Unit.const_opaque ()
  | EPS.Expr.Impure_print_int { value = e; newline } ->
    let%bind v = compile_expr e ~env ~runtime ~effect_reprs ~outer_symbol in
    let%bind i = Immediate_repr.Int.of_opaque v in
    let i = Immediate_repr.Int.to_int_llvalue i in
    let%bind i8_type = Codegen.use_context Llvm.i8_type in
    let newline = Llvm.const_int i8_type (Bool.to_int newline) in
    let { Runtime.print_int; _ } = runtime in
    let%bind _void =
      Runtime.Function.build_call
        print_int
        ~args:(Array.of_list [ i; newline ])
        ""
    in
    Immediate_repr.Unit.const_opaque ()
  | EPS.Expr.Impure_read_int ->
    let { Runtime.read_int; _ } = runtime in
    let%bind i =
      Runtime.Function.build_call read_int ~args:(Array.of_list []) "i"
    in
    let i = Immediate_repr.Int.of_int_llvalue i in
    Immediate_repr.Int.to_opaque i
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
  let%bind pointer_type = Types.pointer in
  let fields = Array.of_list_map operations ~f:(fun _op -> pointer_type) in
  let%map hnd_type =
    Codegen.use_context (fun context -> Llvm.struct_type context fields)
  in
  { Effect_repr.id; hnd_type; operations }
;;

let compile_effect_decls
  : EPS.Program.Effect_decl.t list -> Effect_repr.t Effect_label.Map.t Codegen.t
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
  :  EPS.Program.Fun_decl.t
  -> toplevel:Context.Toplevel.t
  -> runtime:Runtime.t
  -> effect_reprs:Effect_repr.t Effect_label.Map.t
  -> (Variable.t * Llvm.llvalue) Codegen.t
  =
  fun decl ~toplevel ~runtime ~effect_reprs ->
  let open Codegen.Let_syntax in
  let f_name, (ps, e_body) = decl in
  let symbol_name = Symbol_name.of_toplevel f_name in
  let%map code =
    compile_function
      ~symbol_name
      ~rec_name:(Some f_name)
      ps
      e_body
      ~captured_shape:Context.Closure.Shape.empty
      ~toplevel
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
  :  EPS.Program.Fun_decl.t list
  -> runtime:Runtime.t
  -> effect_reprs:Effect_repr.t Effect_label.Map.t
  -> (Variable.t * Function_repr.Callable.t) list Codegen.t
  =
  fun decls ~runtime ~effect_reprs ->
  let open Codegen.Let_syntax in
  Codegen.list_fold decls ~init:[] ~f:(fun defined decl ->
    (* use the latest definition if there is shadowing *)
    let toplevel = Context.Toplevel.of_ordered_alist defined in
    let%map name, code =
      compile_fun_decl decl ~toplevel ~runtime ~effect_reprs
    in
    defined @ [ name, Function_repr.Callable.Code_pointer code ])
;;

let compile_program : EPS.Program.t -> unit Codegen.t =
  fun { EPS.Program.effect_declarations; fun_declarations; entry_expr } ->
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
    Runtime.Function.build_call init ~args:(Array.of_list []) ""
  in
  let toplevel = Context.Toplevel.of_ordered_alist named_code_addresses in
  let%bind env = Context.create_toplevel toplevel in
  let%bind _unit =
    compile_expr
      entry_expr
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
