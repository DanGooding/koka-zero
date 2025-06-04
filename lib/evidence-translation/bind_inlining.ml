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
end

let rec rewrite_aux (expr : Expr.t) ~(toplevel : Variable.Set.t)
  : Expr.t Rewrite_result.t Generation.t
  =
  let open Generation.Let_syntax in
  (* TODO: remove warning suppression *)
  match[@warning "-4"] expr with
  | Application
      (bind, [ expr_a; vector_a; Lambda ([ param_a; param_vector_a ], inner_a) ])
    when is_name bind Primitives.Names.bind ->
    (* TODO: apply to expr_a too *)
    let inner_a_free = Free_variables.free_in_expr inner_a in
    let inner_a_free = Set.diff inner_a_free toplevel in
    let inner_a_free_excluding_a =
      let exclude =
        List.filter_map [ param_a; param_vector_a ] ~f:Parameter.variable_opt
        |> Variable.Set.of_list
      in
      Set.to_list (Set.diff inner_a_free exclude)
    in
    let%bind (inner_result : Expr.t Rewrite_result.t) =
      rewrite_aux inner_a ~toplevel
    in
    let%bind join_name = Generation.fresh_name in
    let join_args = inner_a_free_excluding_a in
    let join_params = List.map join_args ~f:(fun v -> Parameter.Variable v) in
    let join_args = List.map join_args ~f:(fun v -> Expr.Variable v) in
    let join_decl : Program.Fun_decl.t =
      (*
         fun (free(Inner_a) - a) -> fun (a, param_vector_a) -> transformed_inner_a_slow_path
      *)
      ( join_name
      , ( join_params
        , Lambda
            ([ param_a; param_vector_a ], inner_result.transformed_slow_path) )
      )
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
      let%bind pure_branch =
        let%map var_a =
          match (param_a : Parameter.t) with
          | Variable var_a -> return var_a
          | Wildcard -> Generation.fresh_name
        in
        let body =
          Expr.Let (param_vector_a, vector_a, inner_result.transformed_fast_path)
        in
        var_a, body
      in
      let%map yield_branch =
        let%bind marker = Generation.fresh_name in
        let%bind op_clause = Generation.fresh_name in
        let%bind resumption = Generation.fresh_name in
        let%map body =
          let marker = Expr.Variable marker in
          let op_clause = Expr.Variable op_clause in
          let%map resumption =
            Generation.make_lambda_expr_2 (fun x w ->
              Expr.Application
                ( bind
                , [ Application (Variable resumption, [ x; w ])
                  ; w
                  ; Application (Variable join_name, join_args)
                  ] )
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
        , [ expr_a_result.transformed_slow_path
          ; vector_a
          ; Application (Variable join_name, join_args)
          ] )
    in
    return
      { Rewrite_result.transformed_fast_path; transformed_slow_path; fun_decls }
  | Application (e_fun, e_args) ->
    let%bind fun_result = rewrite_aux e_fun ~toplevel in
    let%map arg_results =
      List.map e_args ~f:(rewrite_aux ~toplevel) |> Generation.all
    in
    Rewrite_result.combine
      fun_result
      (Rewrite_result.all arg_results)
      ~f:(fun e_fun e_args -> Expr.Application (e_fun, e_args))
  | Variable _
  | Literal _
  | Fresh_marker
  | Effect_label _
  | Nil_evidence_vector
  | Lambda _
  | Fix_lambda _ ->
    (* TODO: apply under lambdas too *)
    return (Rewrite_result.return expr)
  | Let (param, e_subject, e_body) ->
    let%bind subject_result = rewrite_aux e_subject ~toplevel in
    let%map body_result = rewrite_aux e_body ~toplevel in
    Rewrite_result.combine subject_result body_result ~f:(fun subject body ->
      Expr.Let (param, subject, body))
  | _ ->
    (* here there are two cases
       - the base case - we don't find any more binds
       - an expr containing some binds
    *)
    return (Rewrite_result.return expr)
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
        let name, (params, expr) = fun_decl in
        let toplevel =
          List.map preceding_fun_declarations ~f:(fun (name, _) -> name)
          |> Variable.Set.of_list
        in
        let%map expr, new_fun_decls = rewrite expr ~toplevel in
        let fun_decl = name, (params, expr) in
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
