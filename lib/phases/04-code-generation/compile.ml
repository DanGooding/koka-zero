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

(** [compile_construct_op variant clause ...] generates code to allocate a
    [Types.op] and populate it with the given [variant] and operation [clause]
*)
let compile_construct_op =
  fun tag (clause : Llvm.llvalue) ~runtime : Llvm.llvalue Codegen.t ->
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

(** [compile_match_corrupted_tag ...] generates a default branch for a match
    statement, executed when a variants tag has an unexpected value. This exits
    the program, and results in a null [Types.opaque_pointer] to satisfy llvm's
    type system *)
let compile_match_corrupted_tag
      (type result)
      ~runtime
      ~type_
      ~(is_tail_position : result Is_tail_position.t)
      ()
  : result Codegen.t
  =
  let open Codegen.Let_syntax in
  let { Runtime.exit; _ } = runtime in
  let%bind _void =
    Runtime.Function.build_call exit ~args:(Array.of_list []) ""
  in
  match is_tail_position with
  | Tail_position ->
    (* exit never returns, so this is always unreachable.
       TODO: compile_switch should take [default_never_returns]
       which would exclude the [default] branch from recombination
    *)
    (let%map _unreachable = Codegen.use_builder Llvm.build_unreachable in
     ()
     : result Codegen.t)
  | Non_tail_position ->
    (match (type_ : EPS.Type.t) with
     | Pure ->
       let%map pointer_type = Types.pointer in
       let content = Llvm.const_null pointer_type in
       Ctl_repr.Pure (Packed content)
     | Ctl ->
       let%bind pointer_type = Types.pointer in
       let%map i1_type = Codegen.use_context Llvm.i1_type in
       let content = Llvm.const_null pointer_type in
       let is_yield_i1 = Llvm.const_null i1_type in
       Ctl_repr.Ctl (Ctl_repr.Maybe_yield_repr.create ~is_yield_i1 ~content))
;;

(** produces code to evaluate the given expression *)
let rec compile_expr
          (e : EPS.Expr.t)
          ~(env : Context.t)
          ~runtime
          ~effect_reprs
          ~(outer_symbol : Symbol_name.t)
  : Ctl_repr.t Codegen.t
  =
  let open Codegen.Let_syntax in
  match e with
  | EPS.Expr.Variable v ->
    (* TODO: note this will be duplicated for each access, although common
       subexpression elimination should easily remove it *)
    Context.compile_get env v
  | EPS.Expr.Let (param, type_, subject, body) ->
    compile_let
      ~param
      ~type_
      ~subject
      ~body
      ~is_tail_position:Is_tail_position.Non_tail_position
      ~env
      ~runtime
      ~effect_reprs
      ~outer_symbol
  | EPS.Expr.Lambda lambda ->
    let%map lambda =
      compile_lambda lambda ~env ~runtime ~effect_reprs ~outer_symbol
    in
    Ctl_repr.Pure (Value_repr.Lazily_packed.Function lambda)
  | EPS.Expr.Fix_lambda fix_lambda ->
    let%map lambda =
      compile_fix_lambda fix_lambda ~env ~runtime ~effect_reprs ~outer_symbol
    in
    Ctl_repr.Pure (Value_repr.Lazily_packed.Function lambda)
  | EPS.Expr.Application (e_f, e_args, return_type) ->
    let%bind f =
      compile_expr_pure e_f ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind args =
      List.map e_args ~f:(fun (e_arg, _type) ->
        compile_expr e_arg ~env ~runtime ~effect_reprs ~outer_symbol)
      |> Codegen.all
    in
    compile_application
      f
      args
      return_type
      ~is_tail_position:Is_tail_position.Non_tail_position
      ~env
  | EPS.Expr.Literal lit ->
    let%map lit = compile_literal lit in
    Ctl_repr.Pure (Packed lit)
  | EPS.Expr.If_then_else (e_cond, e_yes, e_no) ->
    let%bind cond_ptr =
      compile_expr_pure_packed e_cond ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind cond = Immediate_repr.Bool.of_opaque cond_ptr in
    compile_if_then_else
      cond
      ~e_yes
      ~e_no
      ~is_tail_position:Is_tail_position.Non_tail_position
      ~env
      ~runtime
      ~effect_reprs
      ~outer_symbol
  | EPS.Expr.Unary_operator (op, e) ->
    let%bind arg =
      compile_expr_pure_packed e ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%map result = compile_unary_operator arg op in
    Ctl_repr.Pure (Packed result)
  | EPS.Expr.Operator (e_left, op, e_right) ->
    let%bind left =
      compile_expr_pure_packed e_left ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind right =
      compile_expr_pure_packed e_right ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%map result = compile_binary_operator ~left op ~right in
    Ctl_repr.Pure (Packed result)
  | EPS.Expr.Construct_pure e ->
    let%bind x =
      compile_expr_pure_packed e ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%map pure = Ctl_repr.Maybe_yield_repr.compile_construct_pure x in
    Ctl_repr.Ctl pure
  | EPS.Expr.Construct_yield
      { marker = e_marker; op_clause = e_op_clause; resumption = e_resumption }
    ->
    let%bind marker =
      compile_expr_pure_packed
        e_marker
        ~env
        ~runtime
        ~effect_reprs
        ~outer_symbol
    in
    let%bind op_clause =
      compile_expr_pure_packed
        e_op_clause
        ~env
        ~runtime
        ~effect_reprs
        ~outer_symbol
    in
    let%bind resumption =
      compile_expr_pure_packed
        e_resumption
        ~env
        ~runtime
        ~effect_reprs
        ~outer_symbol
    in
    let%map yield =
      Ctl_repr.Maybe_yield_repr.compile_construct_yield
        ~marker
        ~op_clause
        ~resumption
        ~runtime
    in
    Ctl_repr.Ctl yield
  | EPS.Expr.Match_ctl { subject = e_subject; pure_branch; yield_branch } ->
    let%bind subject =
      compile_expr_ctl e_subject ~env ~runtime ~effect_reprs ~outer_symbol
    in
    compile_match_ctl
      subject
      ~pure_branch
      ~yield_branch
      ~is_tail_position:Is_tail_position.Non_tail_position
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
    let%map marker = Helpers.heap_store_marker m ~runtime in
    Ctl_repr.Pure (Packed marker)
  | EPS.Expr.Markers_equal (e1, e2) ->
    let%bind marker1_ptr =
      compile_expr_pure_packed e1 ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind marker2_ptr =
      compile_expr_pure_packed e2 ~env ~runtime ~effect_reprs ~outer_symbol
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
    let%map eq = Immediate_repr.Bool.to_opaque eq in
    Ctl_repr.Pure (Packed eq)
  | EPS.Expr.Effect_label label ->
    let%bind repr = lookup_effect_repr effect_reprs label in
    let { Effect_repr.id; _ } = repr in
    let%bind label = Helpers.const_label id in
    let%map label = Helpers.heap_store_label label ~runtime in
    Ctl_repr.Pure (Packed label)
  | EPS.Expr.Construct_handler
      { handled_effect; operation_clauses = operation_clause_exprs } ->
    let%map handler =
      compile_construct_handler
        handled_effect
        operation_clause_exprs
        ~env
        ~runtime
        ~effect_reprs
        ~outer_symbol
    in
    Ctl_repr.Pure (Packed handler)
  | EPS.Expr.Construct_op_normal e_clause ->
    let%bind clause =
      compile_expr_pure_packed
        e_clause
        ~env
        ~runtime
        ~effect_reprs
        ~outer_symbol
    in
    let%map op = compile_construct_op `Normal clause ~runtime in
    Ctl_repr.Pure (Packed op)
  | EPS.Expr.Construct_op_tail e_clause ->
    let%bind clause =
      compile_expr_pure_packed
        e_clause
        ~env
        ~runtime
        ~effect_reprs
        ~outer_symbol
    in
    let%map op = compile_construct_op `Tail clause ~runtime in
    Ctl_repr.Pure (Packed op)
  | EPS.Expr.Match_op { subject = e_subject; normal_branch; tail_branch } ->
    let%bind subject =
      compile_expr_pure_packed
        e_subject
        ~env
        ~runtime
        ~effect_reprs
        ~outer_symbol
    in
    compile_match_op
      subject
      ~normal_branch
      ~tail_branch
      ~is_tail_position:Is_tail_position.Non_tail_position
      ~env
      ~runtime
      ~effect_reprs
      ~outer_symbol
  | EPS.Expr.Select_operation (label, op_name, e_handler) ->
    let%bind handler_ptr =
      compile_expr_pure_packed
        e_handler
        ~env
        ~runtime
        ~effect_reprs
        ~outer_symbol
    in
    let%map op =
      compile_select_operation label ~op_name handler_ptr ~effect_reprs
    in
    Ctl_repr.Pure (Packed op)
  | EPS.Expr.Nil_evidence_vector ->
    let { Runtime.nil_evidence_vector; _ } = runtime in
    let%map vector =
      Runtime.Function.build_call
        nil_evidence_vector
        ~args:(Array.of_list [])
        "nil_vector"
    in
    Ctl_repr.Pure (Packed vector)
  | EPS.Expr.Cons_evidence_vector
      { label = e_label
      ; marker = e_marker
      ; handler = e_handler
      ; handler_site_vector = e_handler_site_vector
      ; vector_tail = e_vector_tail
      } ->
    let%bind label_ptr =
      compile_expr_pure_packed e_label ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind label = Helpers.dereference_label label_ptr in
    let%bind marker_ptr =
      compile_expr_pure_packed
        e_marker
        ~env
        ~runtime
        ~effect_reprs
        ~outer_symbol
    in
    let%bind marker = Helpers.dereference_marker marker_ptr in
    let%bind handler =
      compile_expr_pure_packed
        e_handler
        ~env
        ~runtime
        ~effect_reprs
        ~outer_symbol
    in
    let%bind handler_site_vector =
      compile_expr_pure_packed
        e_handler_site_vector
        ~env
        ~runtime
        ~effect_reprs
        ~outer_symbol
    in
    let%bind vector_tail =
      compile_expr_pure_packed
        e_vector_tail
        ~env
        ~runtime
        ~effect_reprs
        ~outer_symbol
    in
    let { Runtime.cons_evidence_vector; _ } = runtime in
    let args =
      Array.of_list [ label; marker; handler; handler_site_vector; vector_tail ]
    in
    let%map extended_vector =
      Runtime.Function.build_call cons_evidence_vector ~args "extended_vector"
    in
    Ctl_repr.Pure (Packed extended_vector)
  | EPS.Expr.Lookup_evidence { label = e_label; vector = e_vector } ->
    let%bind label_ptr =
      compile_expr_pure_packed e_label ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind label = Helpers.dereference_label label_ptr in
    let%bind vector =
      compile_expr_pure_packed
        e_vector
        ~env
        ~runtime
        ~effect_reprs
        ~outer_symbol
    in
    let { Runtime.evidence_vector_lookup; _ } = runtime in
    let args = Array.of_list [ vector; label ] in
    let%map evidence =
      Runtime.Function.build_call evidence_vector_lookup ~args "evidence"
    in
    Ctl_repr.Pure (Packed evidence)
  | EPS.Expr.Get_evidence_marker e ->
    let%bind evidence =
      compile_expr_pure_packed e ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let { Runtime.get_evidence_marker; _ } = runtime in
    let%bind marker =
      Runtime.Function.build_call
        get_evidence_marker
        ~args:(Array.of_list [ evidence ])
        "marker"
    in
    let%map marker = Helpers.heap_store_marker marker ~runtime in
    Ctl_repr.Pure (Packed marker)
  | EPS.Expr.Get_evidence_handler e ->
    let%bind evidence =
      compile_expr_pure_packed e ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let { Runtime.get_evidence_handler; _ } = runtime in
    let%map handler =
      Runtime.Function.build_call
        get_evidence_handler
        ~args:(Array.of_list [ evidence ])
        "handler"
    in
    Ctl_repr.Pure (Packed handler)
  | EPS.Expr.Get_evidence_handler_site_vector e ->
    let%bind evidence =
      compile_expr_pure_packed e ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let { Runtime.get_evidence_handler_site_vector; _ } = runtime in
    let%map vector =
      Runtime.Function.build_call
        get_evidence_handler_site_vector
        ~args:(Array.of_list [ evidence ])
        "handler_site_vector"
    in
    Ctl_repr.Pure (Packed vector)
  | EPS.Expr.Impure_built_in impure ->
    let%map result =
      compile_impure_built_in impure ~env ~runtime ~effect_reprs ~outer_symbol
    in
    Ctl_repr.Pure (Packed result)

and compile_tail_position_expr
      (e : EPS.Expr.t)
      ~(env : Context.t)
      ~runtime
      ~effect_reprs
      ~(outer_symbol : Symbol_name.t)
  : unit Codegen.t
  =
  let open Codegen.Let_syntax in
  match e with
  | Application (e_f, e_args, return_type) ->
    (* compile a tail-call *)
    let%bind f =
      compile_expr_pure e_f ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind args =
      List.map e_args ~f:(fun (e_arg, _type) ->
        compile_expr e_arg ~env ~runtime ~effect_reprs ~outer_symbol)
      |> Codegen.all
    in
    compile_application
      f
      args
      return_type
      ~is_tail_position:Is_tail_position.Tail_position
      ~env
  | Let (param, type_, subject, body) ->
    compile_let
      ~param
      ~type_
      ~subject
      ~body
      ~is_tail_position:Is_tail_position.Tail_position
      ~env
      ~runtime
      ~effect_reprs
      ~outer_symbol
  | If_then_else (e_cond, e_yes, e_no) ->
    let%bind cond_ptr =
      compile_expr_pure_packed e_cond ~env ~runtime ~effect_reprs ~outer_symbol
    in
    let%bind cond = Immediate_repr.Bool.of_opaque cond_ptr in
    compile_if_then_else
      cond
      ~e_yes
      ~e_no
      ~is_tail_position:Is_tail_position.Tail_position
      ~env
      ~runtime
      ~effect_reprs
      ~outer_symbol
  | Match_ctl { subject; pure_branch; yield_branch } ->
    let%bind subject =
      compile_expr_ctl subject ~env ~runtime ~effect_reprs ~outer_symbol
    in
    compile_match_ctl
      subject
      ~pure_branch
      ~yield_branch
      ~is_tail_position:Is_tail_position.Tail_position
      ~env
      ~runtime
      ~effect_reprs
      ~outer_symbol
  | Match_op { subject; normal_branch; tail_branch } ->
    let%bind subject =
      compile_expr_pure_packed subject ~env ~runtime ~effect_reprs ~outer_symbol
    in
    compile_match_op
      subject
      ~normal_branch
      ~tail_branch
      ~is_tail_position:Is_tail_position.Tail_position
      ~env
      ~runtime
      ~effect_reprs
      ~outer_symbol
  | Variable _
  | Lambda _
  | Fix_lambda _
  | Literal _
  | Operator _
  | Unary_operator _
  | Construct_pure _
  | Construct_yield _
  | Fresh_marker
  | Markers_equal _
  | Effect_label _
  | Construct_op_normal _
  | Construct_op_tail _
  | Construct_handler _
  | Select_operation _
  | Nil_evidence_vector
  | Cons_evidence_vector _
  | Lookup_evidence _
  | Get_evidence_marker _
  | Get_evidence_handler _
  | Get_evidence_handler_site_vector _
  | Impure_built_in _ ->
    let%bind result =
      compile_expr e ~env ~runtime ~effect_reprs ~outer_symbol
    in
    Context.Return_value_pointer.compile_return env.return_value_pointer result

and compile_maybe_tail_position_expr
  : type result.
    EPS.Expr.t
    -> is_tail_position:result Is_tail_position.t
    -> env:Context.t
    -> runtime:Runtime.t
    -> effect_reprs:Effect_repr.t Effect_label.Map.t
    -> outer_symbol:Symbol_name.t
    -> result Codegen.t
  =
  fun e ~is_tail_position ~env ~runtime ~effect_reprs ~outer_symbol ->
  match is_tail_position with
  | Non_tail_position ->
    compile_expr e ~env ~runtime ~effect_reprs ~outer_symbol
  | Tail_position ->
    compile_tail_position_expr e ~env ~runtime ~effect_reprs ~outer_symbol

(* branching - eval subject/match as normal, then recurse on each branch *)
(* all others - delegate to  *)

and compile_expr_pure e ~env ~runtime ~effect_reprs ~outer_symbol
  : Value_repr.Lazily_packed.t Codegen.t
  =
  let open Codegen.Let_syntax in
  let%bind e = compile_expr e ~env ~runtime ~effect_reprs ~outer_symbol in
  Ctl_repr.pure e

and compile_expr_pure_packed e ~env ~runtime ~effect_reprs ~outer_symbol
  : Llvm.llvalue Codegen.t
  =
  let open Codegen.Let_syntax in
  let%bind v = compile_expr_pure e ~env ~runtime ~effect_reprs ~outer_symbol in
  Value_repr.Lazily_packed.pack v

and compile_expr_ctl e ~env ~runtime ~effect_reprs ~outer_symbol
  : Ctl_repr.Maybe_yield_repr.t Codegen.t
  =
  let open Codegen.Let_syntax in
  let%bind e = compile_expr e ~env ~runtime ~effect_reprs ~outer_symbol in
  Ctl_repr.ctl e

and compile_let
  : type result.
    param:Parameter.t
    -> type_:Evidence_passing_syntax.Type.t
    -> subject:EPS.Expr.t
    -> body:EPS.Expr.t
    -> is_tail_position:result Is_tail_position.t
    -> env:Context.t
    -> runtime:Runtime.t
    -> effect_reprs:Effect_repr.t Effect_label.Map.t
    -> outer_symbol:Symbol_name.t
    -> result Codegen.t
  =
  fun ~param
    ~type_:_
    ~subject
    ~body
    ~(is_tail_position : result Is_tail_position.t)
    ~env
    ~runtime
    ~effect_reprs
    ~outer_symbol ->
  let open Codegen.Let_syntax in
  let%bind subject =
    compile_expr subject ~env ~runtime ~effect_reprs ~outer_symbol
  in
  let env' =
    match param with
    | Parameter.Wildcard -> env
    | Parameter.Variable v -> Context.add_local_exn env ~name:v ~value:subject
  in
  compile_maybe_tail_position_expr
    body
    ~is_tail_position
    ~env:env'
    ~runtime
    ~effect_reprs
    ~outer_symbol

(** [compile_match_ctl subject ~pure_branch ~yield_branch ...] generates code to
    branch on the ctl varaint poitned to by [Types.opaque_pointer]:[subject],
    calling either [pure_branch] or [yield_branch] with its fields. (These are
    also compiled) *)
and compile_match_ctl
  : type result.
    Ctl_repr.Maybe_yield_repr.t
    -> pure_branch:Variable.t * EPS.Expr.t
    -> yield_branch:Variable.t * Variable.t * Variable.t * EPS.Expr.t
    -> is_tail_position:result Is_tail_position.t
    -> env:Context.t
    -> runtime:Runtime.t
    -> effect_reprs:Effect_repr.t Effect_label.Map.t
    -> outer_symbol:Symbol_name.t
    -> result Codegen.t
  =
  fun subject
    ~pure_branch
    ~yield_branch
    ~is_tail_position
    ~env
    ~runtime
    ~effect_reprs
    ~outer_symbol ->
  let open Codegen.Let_syntax in
  let compile_pure () =
    let x_value, body = pure_branch in
    let%bind content = Ctl_repr.Maybe_yield_repr.get_content subject in
    let env' =
      Context.add_local_exn env ~name:x_value ~value:(Pure (Packed content))
    in
    compile_maybe_tail_position_expr
      body
      ~is_tail_position
      ~env:env'
      ~runtime
      ~effect_reprs
      ~outer_symbol
  in
  let compile_yield () =
    let%bind ctl_yield_type = Types.ctl_yield in
    let x_marker, x_op_clause, x_resumption, body = yield_branch in
    let%bind content = Ctl_repr.Maybe_yield_repr.get_content subject in
    let%bind marker =
      Helpers.compile_access_field
        content
        ~struct_type:ctl_yield_type
        ~i:0
        (Helpers.register_name_of_variable x_marker)
    in
    let%bind op_clause =
      Helpers.compile_access_field
        content
        ~struct_type:ctl_yield_type
        ~i:1
        (Helpers.register_name_of_variable x_op_clause)
    in
    let%bind resumption =
      Helpers.compile_access_field
        content
        ~struct_type:ctl_yield_type
        ~i:2
        (Helpers.register_name_of_variable x_resumption)
    in
    let env' =
      env
      |> Context.add_local_exn ~name:x_marker ~value:(Pure (Packed marker))
      |> Context.add_local_exn
           ~name:x_op_clause
           ~value:(Pure (Packed op_clause))
      |> Context.add_local_exn
           ~name:x_resumption
           ~value:(Pure (Packed resumption))
    in
    compile_maybe_tail_position_expr
      body
      ~is_tail_position
      ~env:env'
      ~runtime
      ~effect_reprs
      ~outer_symbol
  in
  let%bind is_yield_i1 = Ctl_repr.Maybe_yield_repr.get_is_yield_i1 subject in
  Is_tail_position.compile_conditional
    is_tail_position
    ~cond_i1:is_yield_i1
    ~compile_true:compile_yield
    ~compile_false:compile_pure

(** [compile_match_op subject ~normal_branch ~tail_branch ...] compiles a match
    statement on a [Types.op] pointed to by [subject], calling either
    normal/tail with the op's clause *)
and compile_match_op
  : type result.
    Llvm.llvalue
    -> normal_branch:Variable.t * EPS.Expr.t
    -> tail_branch:Variable.t * EPS.Expr.t
    -> is_tail_position:result Is_tail_position.t
    -> env:Context.t
    -> runtime:Runtime.t
    -> effect_reprs:Effect_repr.t Effect_label.Map.t
    -> outer_symbol:Symbol_name.t
    -> result Codegen.t
  =
  fun (subject : Llvm.llvalue)
    ~(normal_branch : Variable.t * EPS.Expr.t)
    ~(tail_branch : Variable.t * EPS.Expr.t)
    ~(is_tail_position : result Is_tail_position.t)
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
  let make_compile_branch (x, body) () : result Codegen.t =
    let%bind clause =
      Helpers.compile_access_field
        subject
        ~struct_type:op_type
        ~i:1
        (Helpers.register_name_of_variable x)
    in
    let env' =
      Context.add_local_exn env ~name:x ~value:(Pure (Packed clause))
    in
    compile_maybe_tail_position_expr
      body
      ~is_tail_position
      ~env:env'
      ~runtime
      ~effect_reprs
      ~outer_symbol
  in
  Is_tail_position.compile_switch
    is_tail_position
    tag
    ~table:
      [ normal_tag, "op_normal", make_compile_branch normal_branch
      ; tail_tag, "op_tail", make_compile_branch tail_branch
      ]
    ~compile_default:
      (compile_match_corrupted_tag ~runtime ~type_:Ctl ~is_tail_position)

(** [compile_if_then_else b ~e_yes ~e_no ~env ~runtime ~effect_reprs ~outer_symbol]
    generates code to branch on the value of the [Types.bool] [b], and evaluate
    to the value of either [e_yes] or [e_no] *)
and compile_if_then_else
  : type result.
    Immediate_repr.Bool.t
    -> e_yes:EPS.Expr.t
    -> e_no:EPS.Expr.t
    -> is_tail_position:result Is_tail_position.t
    -> env:Context.t
    -> runtime:Runtime.t
    -> effect_reprs:Effect_repr.t Effect_label.Map.t
    -> outer_symbol:Symbol_name.t
    -> result Codegen.t
  =
  fun (cond : Immediate_repr.Bool.t)
    ~(e_yes : EPS.Expr.t)
    ~(e_no : EPS.Expr.t)
    ~(is_tail_position : result Is_tail_position.t)
    ~env
    ~runtime
    ~effect_reprs
    ~outer_symbol ->
  let open Codegen.Let_syntax in
  let%bind cond_i1 = Immediate_repr.Bool.to_i1 cond in
  let compile_branch e () =
    compile_maybe_tail_position_expr
      e
      ~is_tail_position
      ~env
      ~runtime
      ~effect_reprs
      ~outer_symbol
  in
  Is_tail_position.compile_conditional
    is_tail_position
    ~cond_i1
    ~compile_true:(compile_branch e_yes)
    ~compile_false:(compile_branch e_no)

(** [compile_function ~symbol_name rec_name ps e_body ~return_type ~captured_shape ~outer_symbol ...]
    generates a function with the given arguments and body, and within the scope
    given by [captured_shape]. It returns this function's [llvalue]. The
    [llbuilder]'s insertion point is saved and restored. *)
and compile_function
      ~symbol_name
      ~(rec_name : Variable.t option)
      (params : (Parameter.t * EPS.Type.t) list)
      e_body
      ~(return_type : EPS.Type.t)
      ~(captured_shape : Context.Closure.Shape.t option)
      ~(toplevel : Context.Toplevel.t)
      ~outer_symbol
      ~runtime
      ~effect_reprs
  : Llvm.llvalue Codegen.t
  =
  let open Codegen.Let_syntax in
  (* new llvm function *)
  let%bind function_, context =
    Calling_convention.make_function_and_context
      ~params
      ~symbol_name
      ~self:rec_name
      ~return_type
      ~captured_shape
      ~toplevel
  in
  let function_start_block = Llvm.entry_block function_ in
  let%map () =
    Codegen.within_block function_start_block ~f:(fun () ->
      compile_tail_position_expr
        e_body
        ~env:context
        ~runtime
        ~effect_reprs
        ~outer_symbol)
  in
  function_

(** a special case of [compile_function], where the function's symbol name is
    based on the containing function's name ([outer_symbol]) *)
and compile_local_function
      ~(rec_name : Variable.t option)
      (ps : (Parameter.t * EPS.Type.t) list)
      (e_body : EPS.Expr.t)
      ~(return_type : EPS.Type.t)
      ~(captured_shape : Context.Closure.Shape.t option)
      ~(toplevel : Context.Toplevel.t)
      ~(outer_symbol : Symbol_name.t)
      ~(runtime : Runtime.t)
      ~(effect_reprs : Effect_repr.t Effect_label.Map.t)
  : Llvm.llvalue Codegen.t
  =
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
    ~return_type
    ~captured_shape
    ~toplevel
    ~outer_symbol
    ~runtime
    ~effect_reprs

(** a helper function used by [compile_lambda] and [compile_fix_lambda] *)
and compile_lambda_like
      (lambda_like :
        [ `Fix_lambda of EPS.Expr.fix_lambda | `Lambda of EPS.Expr.lambda ])
      ~env
      ~outer_symbol
      ~runtime
      ~effect_reprs
  : Value_repr.Unpacked.Function.t Codegen.t
  =
  let open Codegen.Let_syntax in
  let free =
    match lambda_like with
    | `Fix_lambda fix_lambda -> Free_variables.free_in_fix_lambda fix_lambda
    | `Lambda lambda -> Free_variables.free_in_lambda lambda
  in
  (* capture all free varaibles which are local (non-local ones are already in
     the closure, so just chain)*)
  let captured_shape = Context.get_captured env ~free in
  let rec_name, (params, return_type, e_body) =
    match lambda_like with
    | `Fix_lambda (rec_name, lambda) -> Some rec_name, lambda
    | `Lambda lambda -> None, lambda
  in
  let%bind code_address =
    compile_local_function
      ~rec_name
      params
      e_body
      ~return_type
      ~captured_shape
      ~toplevel:env.toplevel
      ~outer_symbol
      ~runtime
      ~effect_reprs
  in
  match captured_shape with
  | None ->
    (* functions with no free variables are simply code pointers,
       with no heap allocation required *)
    return (Value_repr.Unpacked.Function.Code_pointer code_address)
  | Some captured_shape ->
    let%map { Context.Closure.closure; shape = _ } =
      Context.compile_capture env ~captured_shape ~code_address ~runtime
    in
    Value_repr.Unpacked.Function.Closure closure

(** [compile_lambda lambda ...] compiles a lambda as a function, and generates
    code to construct a function object of it in the given [env]. The result is
    a [Types.opaque_pointer] to it. *)
and compile_lambda
  :  EPS.Expr.lambda
  -> env:Context.t
  -> outer_symbol:Symbol_name.t
  -> runtime:Runtime.t
  -> effect_reprs:Effect_repr.t Effect_label.Map.t
  -> Value_repr.Unpacked.Function.t Codegen.t
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
  -> Value_repr.Unpacked.Function.t Codegen.t
  =
  fun fix_lambda ~env ~outer_symbol ~runtime ~effect_reprs ->
  compile_lambda_like
    (`Fix_lambda fix_lambda)
    ~env
    ~outer_symbol
    ~runtime
    ~effect_reprs

(** [compile_application f args type_] generates code to 'call' the function object
    pointed to by [f], with the given arguments. *)
and compile_application
  : type result.
    Value_repr.Lazily_packed.t
    -> Ctl_repr.t list
    -> EPS.Type.t
    -> is_tail_position:result Is_tail_position.t
    -> env:Context.t
    -> result Codegen.t
  =
  fun (f_ptr : Value_repr.Lazily_packed.t)
    (args : Ctl_repr.t list)
    (return_type : EPS.Type.t)
    ~(is_tail_position : result Is_tail_position.t)
    ~(env : Context.t) ->
  let open Codegen.Let_syntax in
  let compile_call ~code_pointer ~function_repr ~args ~return_type
    : result Codegen.t
    =
    match is_tail_position with
    | Non_tail_position ->
      Calling_convention.compile_call
        ~code_pointer
        ~function_repr
        ~args
        ~return_type
    | Tail_position ->
      let%bind result =
        Calling_convention.compile_tail_call
          ~code_pointer
          ~function_repr
          ~args
          ~return_type
          ~return_value_pointer:env.return_value_pointer
      in
      let%map _return = Codegen.use_builder (Llvm.build_ret result) in
      ()
  in
  Value_repr.Lazily_packed.unpack_function
    f_ptr
    ~f:(fun function_repr ->
      match (function_repr : Value_repr.Unpacked.Function.t) with
      | Code_pointer code_pointer ->
        compile_call ~code_pointer ~function_repr ~args ~return_type
      | Closure closure ->
        let%bind closure_type =
          (* we don't know the number of variables in the closure struct at the call-site,
             but this should be okay, since we only access the field before them. *)
          Types.closure_struct ~num_captured:1
        in
        (* extract fields of f *)
        let%bind code_pointer =
          Helpers.compile_access_field
            closure
            ~struct_type:closure_type
            ~i:0
            "code_address"
        in
        compile_call ~code_pointer ~function_repr ~args ~return_type)
    ~compile_conditional:(Is_tail_position.compile_conditional is_tail_position)

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
        compile_expr_pure_packed
          e_clause
          ~env
          ~runtime
          ~effect_reprs
          ~outer_symbol
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
    let%bind v =
      compile_expr_pure_packed e ~env ~runtime ~effect_reprs ~outer_symbol
    in
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
  let f_name, (ps, return_type, e_body) = decl in
  let symbol_name = Symbol_name.of_toplevel f_name in
  let%map code =
    compile_function
      ~symbol_name
      ~rec_name:(Some f_name)
      ps
      e_body
      ~return_type
      ~captured_shape:None
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
  -> (Variable.t * Value_repr.Unpacked.Function.t) list Codegen.t
  =
  fun decls ~runtime ~effect_reprs ->
  let open Codegen.Let_syntax in
  Codegen.list_fold decls ~init:[] ~f:(fun defined decl ->
    (* use the latest definition if there is shadowing *)
    let toplevel = Context.Toplevel.of_ordered_alist defined in
    let%map name, code =
      compile_fun_decl decl ~toplevel ~runtime ~effect_reprs
    in
    defined @ [ name, Value_repr.Unpacked.Function.Code_pointer code ])
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
  let env = Context.create_toplevel toplevel in
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
