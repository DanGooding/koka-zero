open Core
open Import
open Evidence_passing_syntax

let map_name_lambda ~(name : Variable.t) (lambda : Expr.lambda Generation.t)
  : Expr.fix_lambda Generation.t
  =
  let open Generation.Let_syntax in
  let%map lambda = lambda in
  name, lambda
;;

(** bind : (ctl<e,a>, evv<e>, (a, evv<e>) -> ctl<e,b>) -> ctl<e,b> *)
let bind =
  let open Generation.Let_syntax in
  map_name_lambda
    ~name:Primitive_names.bind
    (Generation.make_lambda_3 (fun e w g ->
       Generation.make_match_ctl
         e
         ~pure:(fun x -> Expr.Application (g, [ x; w ]) |> Generation.return)
         ~yield:(fun ~marker ~op_clause ~resumption ->
           let%map resumption =
             (* this should just be [kleisli_compose_unary (g, resumption)]
                but that would be mutual recursion, so we inline it *)
             Generation.make_lambda_expr_2 (fun x w ->
               (* \x w. (resume(x w), w) >>= g *)
               Expr.Application
                 ( Expr.Variable Primitive_names.bind
                 , [ Expr.Application (resumption, [ x; w ]); w; g ] )
               |> Generation.return)
           in
           Expr.Construct_yield { marker; op_clause; resumption })))
;;

(** prompt : (label, marker, hnd<label,e,a>, evv<label|e> -> ctl<label|e>,
    evv<e>) -> ct<e,a> *)
let prompt =
  let open Generation.Let_syntax in
  map_name_lambda
    ~name:Primitive_names.prompt
    (Generation.make_lambda_5 (fun label marker handler e vector ->
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
           let%map resumption =
             Generation.make_lambda_expr_2 (fun x vector ->
               let%map resume_with_x =
                 Generation.make_lambda_expr_1 (fun vector' ->
                   Expr.Application (resumption, [ x; vector' ])
                   |> Generation.return)
               in
               Expr.Application
                 ( Expr.Variable Primitive_names.prompt
                 , [ label; marker; handler; resume_with_x; vector ] ))
           in
           let handle_here =
             Expr.Application (op_clause, [ resumption; vector ])
           in
           let bubble_further =
             Expr.Construct_yield { marker = marker'; op_clause; resumption }
           in
           Expr.If_then_else
             (Expr.Markers_equal (marker, marker'), handle_here, bubble_further))))
;;

(** handler : (label, hnd<label,e,a>) -> (evv<label|e> ->) *)
let handler =
  map_name_lambda
    ~name:Primitive_names.handler
    (Generation.make_lambda_2 (fun label handler ->
       Generation.make_lambda_expr_2 (fun action vector ->
         Expr.Application
           ( Expr.Variable Primitive_names.prompt
             (* TODO: this has a side effect - is it correctly ordered? *)
           , [ label; Expr.Fresh_marker; handler; action; vector ] )
         |> Generation.return)))
;;

(** under : (label, evv<e'>, a, (a, evv<e'>) -> ctl<e',b>) -> ctl<e,b> *)
let under =
  let open Generation.Let_syntax in
  map_name_lambda
    ~name:Primitive_names.under
    (Generation.make_lambda_4 (fun label handler_site_vector x g ->
       let run_at_handler = Expr.Application (g, [ x; handler_site_vector ]) in
       Generation.make_match_ctl
         run_at_handler
         ~pure:(fun x -> Expr.Construct_pure x |> Generation.return)
         ~yield:(fun ~marker ~op_clause ~resumption ->
           let%map resumption' =
             (* inlined [underk] *)
             Generation.make_lambda_expr_2 (fun x vector ->
               let evidence = Expr.Lookup_evidence { label; vector } in
               let handler_site_vector =
                 Expr.Get_evidence_handler_site_vector evidence
               in
               Expr.Application
                 ( Expr.Variable Primitive_names.under
                 , [ label; handler_site_vector; x; resumption ] )
               |> Generation.return)
           in
           Expr.Construct_yield { marker; op_clause; resumption = resumption' })))
;;

(** perform : (label, select : hnd<e',r> -> op<a,b,e',r>) -> (a, evv<e>) ->
    ctl<label|e> b *)
let perform =
  let open Generation.Let_syntax in
  map_name_lambda
    ~name:Primitive_names.perform
    (Generation.make_lambda_2 (fun label select ->
       Generation.make_lambda_expr_2 (fun arg vector ->
         let evidence = Expr.Lookup_evidence { label; vector } in
         let handler = Expr.Get_evidence_handler evidence in
         let op = Expr.Application (select, [ handler ]) in
         Generation.make_match_op
           op
           ~normal:(fun op_clause ->
             let marker = Expr.Get_evidence_marker evidence in
             (* monadic form of identity is `Pure` *)
             let%bind identity_resumption =
               Generation.make_lambda_expr_2 (fun x _vector ->
                 Expr.Construct_pure x |> Generation.return)
             in
             let%map op_clause =
               Generation.make_lambda_expr_2 (fun resume vector ->
                 Expr.Application (op_clause, [ arg; resume; vector ])
                 |> Generation.return)
             in
             Expr.Construct_yield
               { marker; op_clause; resumption = identity_resumption })
           ~tail:(fun op_clause ->
             let handler_site_vector =
               Expr.Get_evidence_handler_site_vector evidence
             in
             Expr.Application
               ( Expr.Variable Primitive_names.under
               , [ label; handler_site_vector; arg; op_clause ] )
             |> Generation.return))))
;;

let prelude =
  let open Generation.Let_syntax in
  let decls = [ bind; prompt; handler; under; perform ] in
  let%map decls_rev =
    Generation.list_fold decls ~init:[] ~f:(fun decls_rev decl ->
      let%map decl = decl in
      decl :: decls_rev)
  in
  List.rev decls_rev
;;
