open Core
open Evidence_passing_syntax
open Import

module Names = struct
  let compose_unary = Variable.of_language_internal "compose_unary"
  let bind = Variable.of_language_internal "bind"
  let pure = Variable.of_language_internal "pure"
  let prompt = Variable.of_language_internal "prompt"
  let handler = Variable.of_language_internal "handler"
  let under = Variable.of_language_internal "under"
  let perform = Variable.of_language_internal "perform"
end

let map_name_lambda ~(name : Variable.t) (lambda : Expr.lambda Generation.t)
    : Expr.fix_lambda Generation.t
  =
  let open Generation.Let_syntax in
  let%map lambda = lambda in
  name, lambda
;;

(** [compose_unary] is the function `\g f -> \x -> g(f(x))` *)
let compose_unary =
  map_name_lambda
    ~name:Names.compose_unary
    (Generation.make_lambda_2 (fun g f ->
         Generation.make_lambda_expr_1 (fun x ->
             Expr.Application (g, [ Expr.Application (f, [ x ]) ])
             |> Generation.return)))
;;

(* TODO: note [bind], [kleisli_compose_unary] are mutually recursive *)
(** bind : ( mon<e,a>, a -> mon<e,b> ) -> mon<e, b> *)
let bind =
  let open Generation.Let_syntax in
  map_name_lambda
    ~name:Names.bind
    (Generation.make_lambda_2 (fun e g ->
         Generation.make_lambda_expr_1 (fun vector ->
             let run_e = Expr.Application (e, [ vector ]) in
             Generation.make_match_ctl
               run_e
               ~pure:(fun x ->
                 Expr.Application (Expr.Application (g, [ x ]), [ vector ])
                 |> Generation.return)
               ~yield:(fun ~marker ~op_clause ~resumption ->
                 (* this should just be [kleisli_compose_unary g resumption] but
                    that would be mutual recursion, so we inline it *)
                 let%map resumption =
                   Generation.make_lambda_expr_1 (fun x ->
                       Expr.Application
                         ( Expr.Variable Names.bind
                         , [ Expr.Application (resumption, [ x ]); g ] )
                       |> Generation.return)
                 in
                 Expr.Construct_yield { marker; op_clause; resumption }))))
;;

(** pure : a -> mon<e,a> *)
let pure =
  map_name_lambda
    ~name:Names.pure
    (Generation.make_lambda_1 (fun v ->
         Generation.make_lambda_expr_1 (fun _vector ->
             Expr.Construct_pure v |> Generation.return)))
;;

(** prompt : (label, marker, hnd<label,e,a>) -> (mon<label|e> a) -> mon<e> a *)
let prompt =
  map_name_lambda
    ~name:Names.prompt
    (Generation.make_lambda_3 (fun label marker handler ->
         Generation.make_lambda_expr_1 (fun e ->
             Generation.make_lambda_expr_1 (fun vector ->
                 let vector' =
                   Expr.Cons_evidence_vector
                     { label
                     ; marker
                     ; handler
                     ; handler_site_vector = vector
                     ; vector_tail = vector
                     }
                 in
                 let run_e_under = Expr.Application (e, [ vector' ]) in
                 Generation.make_match_ctl
                   run_e_under
                   ~pure:(fun x -> Expr.Construct_pure x |> Generation.return)
                   ~yield:(fun ~marker:marker' ~op_clause ~resumption ->
                     let same_prompt =
                       Expr.Application
                         (Expr.Variable Names.prompt, [ label; marker; handler ])
                     in
                     let resume_under =
                       (* resumption run under this exact prompt *)
                       Expr.Application
                         ( Expr.Variable Names.compose_unary
                         , [ same_prompt; resumption ] )
                     in
                     let bubble_further =
                       Expr.Construct_yield
                         { marker = marker'
                         ; op_clause
                         ; resumption = resume_under
                         }
                     in
                     let handle_here =
                       Expr.Application
                         ( Expr.Application (op_clause, [ resume_under ])
                         , [ vector ] )
                     in
                     Expr.If_then_else
                       ( Expr.Markers_equal (marker, marker')
                       , handle_here
                       , bubble_further )
                     |> Generation.return)))))
;;

(** handler : (label, hnd<label,e,a>) -> (action : () -> mon<label|e> a) ->
    mon<e> a *)
let handler =
  map_name_lambda
    ~name:Names.handler
    (Generation.make_lambda_2 (fun label handler ->
         Generation.make_lambda_expr_1 (fun action ->
             Expr.Application
               ( Expr.Application
                   ( Expr.Variable Names.prompt
                   , [ label
                     ; Expr.Fresh_marker
                       (* TODO: this has a side effect - is it safe? *)
                     ; handler
                     ] )
               , [ Expr.Application (action, []) ] )
             |> Generation.return)))
;;

(** under : (label, evv<e'>, mon<e'> a) -> mon<e> a *)
let under =
  let open Generation.Let_syntax in
  map_name_lambda
    ~name:Names.under
    (Generation.make_lambda_3 (fun label handler_site_vector e ->
         Generation.make_lambda_expr_1 (fun _perform_site_vector ->
             let result = Expr.Application (e, [ handler_site_vector ]) in
             Generation.make_match_ctl
               result
               ~pure:(fun x -> Expr.Construct_pure x |> Generation.return)
               ~yield:(fun ~marker ~op_clause ~resumption ->
                 let%map resumption' =
                   (* inlined [underk] *)
                   Generation.make_lambda_expr_1 (fun x ->
                       Generation.make_lambda_expr_1 (fun vector ->
                           let evidence =
                             Expr.Lookup_evidence { label; vector }
                           in
                           let handler_site_vector =
                             Expr.Get_evidence_handler_site_vector evidence
                           in
                           let e_resume =
                             Expr.Application (resumption, [ x ])
                           in
                           Expr.Application
                             ( Expr.Application
                                 ( Expr.Variable Names.under
                                 , [ label; handler_site_vector; e_resume ] )
                             , [ vector ] )
                           |> Generation.return))
                 in
                 Expr.Construct_yield
                   { marker; op_clause; resumption = resumption' }))))
;;

(* TODO: or a label-indexed family? *)
(** perform : (label, select : hnd<e,r> -> op<a,b,e,r>) -> a -> mon<label|e> r *)
let perform =
  let open Generation.Let_syntax in
  map_name_lambda
    ~name:Names.perform
    (Generation.make_lambda_2 (fun label select ->
         Generation.make_lambda_expr_1 (fun arg ->
             Generation.make_lambda_expr_1 (fun vector ->
                 let evidence = Expr.Lookup_evidence { label; vector } in
                 let handler = Expr.Get_evidence_handler evidence in
                 let op = Expr.Application (select, [ handler ]) in
                 Generation.make_match_op
                   op
                   ~normal:(fun op_clause ->
                     let marker = Expr.Get_evidence_marker evidence in
                     (* monadic form of identity is: `\x -> pure x` *)
                     let identity_resumption = Expr.Variable Names.pure in
                     let%map op_clause =
                       Generation.make_lambda_expr_1 (fun resume ->
                           Expr.Application (op_clause, [ arg; resume ])
                           |> Generation.return)
                     in
                     Expr.Construct_yield
                       { marker; op_clause; resumption = identity_resumption })
                   ~tail:(fun op_clause ->
                     let handler_site_vector =
                       Expr.Get_evidence_handler_site_vector evidence
                     in
                     Expr.Application
                       ( Expr.Application
                           ( Expr.Variable Names.under
                           , [ label
                             ; handler_site_vector
                             ; Expr.Application (op_clause, [ arg ])
                             ] )
                       , [ vector ] )
                     |> Generation.return)))))
;;

let prelude =
  let open Generation.Let_syntax in
  let decls = [ compose_unary; bind; pure; prompt; handler; under; perform ] in
  let%map decls_rev =
    Generation.list_fold decls ~init:[] ~f:(fun decls_rev decl ->
        let%map decl = decl in
        decl :: decls_rev)
  in
  List.rev decls_rev
;;
