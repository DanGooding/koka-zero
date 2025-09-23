open Core
open Import
open Evidence_passing_syntax

let is_name : Expr.t -> Variable.t -> bool =
  fun e name ->
  match[@warning "-4"] e with
  | Expr.Variable v -> Variable.(name = v)
  | _ -> false
;;

module Rewrite_result = struct
  (** This is an Applicative, but not a Monad *)
  type 'a t =
    { transformed_fast_path : 'a
    ; transformed_slow_path : 'a
    ; fun_decls : Program.Fun_decl.t list
    }

  let return expr =
    { transformed_fast_path = expr
    ; transformed_slow_path = expr
    ; fun_decls = []
    }
  ;;

  let map t ~f =
    let transformed_fast_path = f t.transformed_fast_path in
    let transformed_slow_path = f t.transformed_slow_path in
    { t with transformed_fast_path; transformed_slow_path }
  ;;

  let combine t t' ~f =
    let transformed_fast_path =
      f t.transformed_fast_path t'.transformed_fast_path
    in
    let transformed_slow_path =
      f t.transformed_slow_path t'.transformed_slow_path
    in
    let fun_decls = t.fun_decls @ t'.fun_decls in
    { transformed_fast_path; transformed_slow_path; fun_decls }
  ;;

  let combine3 t1 t2 t3 ~f =
    combine
      t1
      (combine t2 t3 ~f:(fun a2 a3 -> a2, a3))
      ~f:(fun a1 (a2, a3) -> f a1 a2 a3)
  ;;

  let all ts =
    let transformed_fast_path =
      List.map ts ~f:(fun t -> t.transformed_fast_path)
    in
    let transformed_slow_path =
      List.map ts ~f:(fun t -> t.transformed_slow_path)
    in
    let fun_decls = List.concat_map ts ~f:(fun t -> t.fun_decls) in
    { transformed_fast_path; transformed_slow_path; fun_decls }
  ;;

  let all_map ts =
    let transformed_fast_path =
      Map.map ts ~f:(fun t -> t.transformed_fast_path)
    in
    let transformed_slow_path =
      Map.map ts ~f:(fun t -> t.transformed_slow_path)
    in
    let fun_decls = Map.data ts |> List.concat_map ~f:(fun t -> t.fun_decls) in
    { transformed_fast_path; transformed_slow_path; fun_decls }
  ;;
end

let all_of_type type_ types =
  List.all_equal (type_ :: types) ~equal:[%equal: Type.t] |> Option.is_some
;;

let rec rewrite_aux (expr : Expr.t) ~(toplevel : Variable.Set.t)
  : Expr.t Rewrite_result.t Generation.t
  =
  let open Generation.Let_syntax in
  match expr with
  | Application
      ( bind
      , [ (expr_a, type_expr_a)
        ; (vector_a, type_vector_a)
        ; ( Lambda
              ( [ (param_a, type_param_a)
                ; (param_vector_a, type_param_vector_a)
                ]
              , type_inner_a
              , inner_a )
          , type_bind_expr (* Pure *) )
        ]
      , type_bind_result (* Ctl *) )
    when is_name bind Primitive_names.bind ->
    (* guard against incorrectly typed bind. for some reason the exhaustive match heurstic complains if
       we check this in the above pattern match. *)
    (match
       all_of_type Ctl [ type_expr_a; type_inner_a; type_bind_result ]
       && all_of_type
            Pure
            [ type_vector_a; type_param_a; type_param_vector_a; type_bind_expr ]
     with
     | true -> ()
     | false ->
       raise_s
         [%message
           "badly typed [bind] application"
             (expr : Expr.t)
             ~expected_ctl:
               ([ type_expr_a; type_inner_a; type_bind_expr ] : Type.t list)
             ~expected_pure:
               ([ type_vector_a
                ; type_param_a
                ; type_param_vector_a
                ; type_bind_result
                ]
                : Type.t list)]);
    let inner_a_free = Free_variables.free_in_expr inner_a in
    let inner_a_free = Set.diff inner_a_free toplevel in
    let inner_a_free_excluding_a =
      let exclude =
        List.map [ param_a; param_vector_a ] ~f:Parameter.bound_variables
        |> Variable.Set.union_list
      in
      Set.to_list (Set.diff inner_a_free exclude)
    in
    let%bind (inner_result : Expr.t Rewrite_result.t) =
      rewrite_aux inner_a ~toplevel
    in
    let%bind join_name = Generation.fresh_name in
    let join_args = inner_a_free_excluding_a in
    let join_params =
      List.map join_args ~f:(fun v -> Parameter.Variable v, Type.Pure)
    in
    let join_args =
      List.map join_args ~f:(fun v -> Expr.Variable v, Type.Pure)
    in
    let join_decl : Program.Fun_decl.t =
      (*
         fun (free(Inner_a) - a) -> fun (a, param_vector_a) -> transformed_inner_a_slow_path
      *)
      ( join_name
      , ( join_params
        , Pure
        , Lambda
            ( [ param_a, Pure; param_vector_a, Pure ]
            , Ctl
            , inner_result.transformed_slow_path ) ) )
    in
    let%bind expr_a_result = rewrite_aux expr_a ~toplevel in
    let fun_decls =
      expr_a_result.fun_decls @ inner_result.fun_decls @ [ join_decl ]
    in
    let%bind transformed_fast_path =
      (*
         match expr_a with
         | Yield (m,op,k) -> Yield (m, op, k * join_a(<free(Inner_a) - a)>)
         | Pure a ->
         let param_vector_a = vector_a in
         transformed_inner_a_fast_path
      *)
      let subject = expr_a_result.transformed_fast_path in
      let pure_branch =
        let body =
          Expr.Let
            (param_vector_a, Pure, vector_a, inner_result.transformed_fast_path)
        in
        param_a, body
      in
      let%map yield_branch =
        let%bind marker = Generation.fresh_name in
        let%bind op_clause = Generation.fresh_name in
        let%bind resumption = Generation.fresh_name in
        let%map body =
          let marker = Expr.Variable marker in
          let op_clause = Expr.Variable op_clause in
          let%map resumption =
            Generation.make_lambda_expr_2 Ctl (fun x w ->
              Expr.Application
                ( bind
                , [ ( Application
                        (Variable resumption, [ x, Pure; w, Pure ], Ctl)
                    , Ctl )
                  ; w, Pure
                  ; Application (Variable join_name, join_args, Pure), Pure
                  ]
                , Ctl )
              |> Generation.return)
          in
          Expr.Construct_yield { marker; op_clause; resumption }
        in
        marker, op_clause, resumption, body
      in
      Expr.Match_ctl { subject; pure_branch; yield_branch }
    in
    let transformed_slow_path =
      (*
         (expr_a, vector_a) >>= join_a (<free(Inner_a) - a>)
      *)
      Expr.Application
        ( bind
        , [ expr_a_result.transformed_slow_path, Ctl
          ; vector_a, Pure
          ; Application (Variable join_name, join_args, Pure), Pure
          ]
        , Ctl )
    in
    return
      { Rewrite_result.transformed_fast_path; transformed_slow_path; fun_decls }
  | Lambda lambda ->
    let%map lambda = rewrite_lambda_aux lambda ~toplevel in
    Rewrite_result.map lambda ~f:(fun lambda -> Expr.Lambda lambda)
  | Fix_lambda (name, lambda) ->
    let%map lambda = rewrite_lambda_aux lambda ~toplevel in
    Rewrite_result.map lambda ~f:(fun lambda -> Expr.Fix_lambda (name, lambda))
  (* the implementation simply recurses over the rest of the syntax tree,
     choosing the fast-path for within each new lambda. *)
  | Application (e_fun, e_args, type_) ->
    let%bind fun_result = rewrite_aux e_fun ~toplevel in
    let%map arg_results =
      List.map e_args ~f:(fun (e_arg, type_) ->
        let%map arg_result = rewrite_aux e_arg ~toplevel in
        Rewrite_result.map arg_result ~f:(fun arg -> arg, type_))
      |> Generation.all
    in
    Rewrite_result.combine
      fun_result
      (Rewrite_result.all arg_results)
      ~f:(fun e_fun e_args -> Expr.Application (e_fun, e_args, type_))
  | Variable _ | Literal _ | Fresh_marker | Effect_label _ | Nil_evidence_vector
    -> return (Rewrite_result.return expr)
  | Construction (constructor, args) ->
    let%map args = Generation.list_map args ~f:(rewrite_aux ~toplevel) in
    Rewrite_result.all args
    |> Rewrite_result.map ~f:(fun args -> Expr.Construction (constructor, args))
  | Tuple_construction args ->
    let%map args = Generation.list_map args ~f:(rewrite_aux ~toplevel) in
    Rewrite_result.all args
    |> Rewrite_result.map ~f:(fun args -> Expr.Tuple_construction args)
  | Let (param, type_, e_subject, e_body) ->
    let%bind subject_result = rewrite_aux e_subject ~toplevel in
    let%map body_result = rewrite_aux e_body ~toplevel in
    Rewrite_result.combine subject_result body_result ~f:(fun subject body ->
      Expr.Let (param, type_, subject, body))
  | If_then_else (e_cond, e_true, e_false) ->
    let%bind cond_result = rewrite_aux e_cond ~toplevel in
    let%bind true_result = rewrite_aux e_true ~toplevel in
    let%map false_result = rewrite_aux e_false ~toplevel in
    Rewrite_result.combine3
      cond_result
      true_result
      false_result
      ~f:(fun e_cond e_true e_false ->
        Expr.If_then_else (e_cond, e_true, e_false))
  | Match (e_subject, scrutinee, cases) ->
    let%bind e_subject = rewrite_aux e_subject ~toplevel in
    let%map cases =
      Generation.list_map cases ~f:(fun (pattern, body) ->
        let%map body = rewrite_aux body ~toplevel in
        Rewrite_result.map body ~f:(fun body -> pattern, body))
    in
    let cases = Rewrite_result.all cases in
    Rewrite_result.combine e_subject cases ~f:(fun subject cases ->
      Expr.Match (subject, scrutinee, cases))
  | Operator (e_left, op, e_right) ->
    let%bind left_result = rewrite_aux e_left ~toplevel in
    let%map right_result = rewrite_aux e_right ~toplevel in
    Rewrite_result.combine left_result right_result ~f:(fun e_left e_right ->
      Expr.Operator (e_left, op, e_right))
  | Unary_operator (op, e) ->
    let%map result = rewrite_aux e ~toplevel in
    Rewrite_result.map result ~f:(fun e -> Expr.Unary_operator (op, e))
  | Construct_pure e ->
    let%map result = rewrite_aux e ~toplevel in
    Rewrite_result.map result ~f:(fun e -> Expr.Construct_pure e)
  | Construct_yield { marker; op_clause; resumption } ->
    let%bind marker = rewrite_aux marker ~toplevel in
    let%bind op_clause = rewrite_aux op_clause ~toplevel in
    let%map resumption = rewrite_aux resumption ~toplevel in
    Rewrite_result.combine3
      marker
      op_clause
      resumption
      ~f:(fun marker op_clause resumption ->
        Expr.Construct_yield { marker; op_clause; resumption })
  | Markers_equal (e_left, e_right) ->
    let%bind left_result = rewrite_aux e_left ~toplevel in
    let%map right_result = rewrite_aux e_right ~toplevel in
    Rewrite_result.combine left_result right_result ~f:(fun e_left e_right ->
      Expr.Markers_equal (e_left, e_right))
  | Construct_op_normal e ->
    let%map result = rewrite_aux e ~toplevel in
    Rewrite_result.map result ~f:(fun e -> Expr.Construct_op_normal e)
  | Construct_op_tail e ->
    let%map result = rewrite_aux e ~toplevel in
    Rewrite_result.map result ~f:(fun e -> Expr.Construct_op_tail e)
  | Match_op { subject; normal_branch = x, e_normal; tail_branch = y, e_tail }
    ->
    let%bind subject = rewrite_aux subject ~toplevel in
    let%bind normal = rewrite_aux e_normal ~toplevel in
    let%map tail = rewrite_aux e_tail ~toplevel in
    Rewrite_result.combine3
      subject
      normal
      tail
      ~f:(fun subject e_normal e_tail ->
        Expr.Match_op
          { subject; normal_branch = x, e_normal; tail_branch = y, e_tail })
  | Construct_handler { handled_effect; operation_clauses } ->
    let%map operation_clauses =
      Map.map operation_clauses ~f:(rewrite_aux ~toplevel) |> Generation.all_map
    in
    let operation_clauses = Rewrite_result.all_map operation_clauses in
    Rewrite_result.map operation_clauses ~f:(fun operation_clauses ->
      Expr.Construct_handler { handled_effect; operation_clauses })
  | Select_operation (label, op, e) ->
    let%map result = rewrite_aux e ~toplevel in
    Rewrite_result.map result ~f:(fun e -> Expr.Select_operation (label, op, e))
  | Cons_evidence_vector
      { label; marker; handler; handler_site_vector; vector_tail } ->
    let%bind label = rewrite_aux label ~toplevel in
    let%bind marker = rewrite_aux marker ~toplevel in
    let%bind handler = rewrite_aux handler ~toplevel in
    let%bind handler_site_vector = rewrite_aux handler_site_vector ~toplevel in
    let%map vector_tail = rewrite_aux vector_tail ~toplevel in
    Rewrite_result.combine3
      label
      marker
      (Rewrite_result.combine3
         handler
         handler_site_vector
         vector_tail
         ~f:Tuple3.create)
      ~f:(fun label marker (handler, handler_site_vector, vector_tail) ->
        Expr.Cons_evidence_vector
          { label; marker; handler; handler_site_vector; vector_tail })
  | Lookup_evidence { label; vector } ->
    let%bind label = rewrite_aux label ~toplevel in
    let%map vector = rewrite_aux vector ~toplevel in
    Rewrite_result.combine label vector ~f:(fun label vector ->
      Expr.Lookup_evidence { label; vector })
  | Get_evidence_marker e ->
    let%map result = rewrite_aux e ~toplevel in
    Rewrite_result.map result ~f:(fun e -> Expr.Get_evidence_marker e)
  | Get_evidence_handler e ->
    let%map result = rewrite_aux e ~toplevel in
    Rewrite_result.map result ~f:(fun e -> Expr.Get_evidence_handler e)
  | Get_evidence_handler_site_vector e ->
    let%map result = rewrite_aux e ~toplevel in
    Rewrite_result.map result ~f:(fun e ->
      Expr.Get_evidence_handler_site_vector e)
  | Impure_built_in impure_built_in ->
    let%map result = rewrite_impure_builtin impure_built_in ~toplevel in
    Rewrite_result.map result ~f:(fun impure_built_in ->
      Expr.Impure_built_in impure_built_in)
  | Match_ctl_pure { subject; pure_branch = x, pure_body } ->
    let%bind subject = rewrite_aux subject ~toplevel in
    let%map pure_body = rewrite_aux pure_body ~toplevel in
    Rewrite_result.combine subject pure_body ~f:(fun subject pure_body ->
      Expr.Match_ctl_pure { subject; pure_branch = x, pure_body })
  | Match_ctl _ ->
    (* before this rewriting pass, this appears only within the definition of [bind],
       so there is no need to recurse into this *)
    return (Rewrite_result.return expr)

and rewrite_lambda (params, type_, e_body) ~toplevel =
  let open Generation.Let_syntax in
  let%map (body_result : _ Rewrite_result.t) = rewrite_aux e_body ~toplevel in
  let lambda = params, type_, body_result.transformed_fast_path in
  let fun_decls = body_result.fun_decls in
  lambda, fun_decls

and rewrite_lambda_aux lambda ~toplevel =
  let open Generation.Let_syntax in
  let%map lambda, fun_decls = rewrite_lambda lambda ~toplevel in
  { Rewrite_result.transformed_fast_path = lambda
  ; transformed_slow_path = lambda
  ; fun_decls
  }

and rewrite_impure_builtin (impure_built_in : Expr.impure_built_in) ~toplevel =
  let open Generation.Let_syntax in
  match impure_built_in with
  | Impure_println | Impure_read_int ->
    return (Rewrite_result.return impure_built_in)
  | Impure_print_int { value; newline } ->
    let%map result = rewrite_aux value ~toplevel in
    Rewrite_result.map result ~f:(fun value ->
      Expr.Impure_print_int { value; newline })
;;

let rewrite expr ~toplevel =
  let open Generation.Let_syntax in
  let%map (result : Expr.t Rewrite_result.t) = rewrite_aux expr ~toplevel in
  result.transformed_fast_path, result.fun_decls
;;

let rewrite_program (program : Program.t) =
  let open Generation.Let_syntax in
  let%map fun_declarations =
    Generation.list_fold
      program.fun_declarations
      ~init:[]
      ~f:(fun preceding_fun_declarations fun_decl ->
        let name, lambda = fun_decl in
        let toplevel =
          List.map preceding_fun_declarations ~f:(fun (name, _) -> name)
          |> Variable.Set.of_list
        in
        let%map lambda, new_fun_decls = rewrite_lambda lambda ~toplevel in
        let fun_decl = name, lambda in
        preceding_fun_declarations @ new_fun_decls @ [ fun_decl ])
  in
  { program with fun_declarations }
;;

(*
   This is an inlining transformation on a sequence of binds such as:
   ... bind (A, V_a, lambda (a, w_a) ->
   ... bind (B, V_b, lambda (b, w_b) ->
   ...
   ... bind (Z, V_z, lambda (z, w_z) ->
   Inner_Z
   )..))

   The aim is to inline along a fast-path where none of the expressions Yield,
   but not to inline at all after the first Yield. This avoids allocating callback
   closures in the non-yielding case, while also preventing the exponential blowup
   of naive inlining.
*)
