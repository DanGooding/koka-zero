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
    (let%bind e = Generation.fresh_name in
     let%bind w = Generation.fresh_name in
     let%bind g = Generation.fresh_name in
     let params : (Parameter.t * Type.t) list =
       [ Variable e, Ctl; Variable w, Pure; Variable g, Pure ]
     in
     let%map body =
       let e = Expr.Variable e in
       let w = Expr.Variable w in
       let g = Expr.Variable g in
       Generation.make_match_ctl
         e
         ~pure:(fun x ->
           Expr.Application (g, [ x, Pure; w, Pure ], Ctl) |> Generation.return)
         ~yield:(fun ~marker ~op_clause ~resumption ->
           let%map resumption =
             (* this should just be [kleisli_compose_unary (g, resumption)]
                but that would be mutual recursion, so we inline it *)
             Generation.make_lambda_expr_2 Ctl (fun x w ->
               (* \x w. (resume(x w), w) >>= g *)
               Expr.Application
                 ( Expr.Variable Primitive_names.bind
                 , [ ( Expr.Application (resumption, [ x, Pure; w, Pure ], Ctl)
                     , Ctl )
                   ; w, Pure
                   ; g, Pure
                   ]
                 , Ctl )
               |> Generation.return)
           in
           Expr.Construct_yield { marker; op_clause; resumption })
     in
     params, Type.Ctl, body)
;;

(** prompt : (label, marker, hnd<label,e,a>, evv<label|e> -> ctl<label|e>,
    evv<e>) -> ctl<e,a> *)
let prompt =
  let open Generation.Let_syntax in
  map_name_lambda
    ~name:Primitive_names.prompt
    (Generation.make_lambda_5 Ctl (fun label marker handler e vector ->
       let vector' =
         Expr.Cons_evidence_vector
           { label
           ; marker
           ; handler
           ; handler_site_vector = vector
           ; vector_tail = vector
           }
       in
       let run_e_under = Expr.Application (e, [ vector', Pure ], Ctl) in
       Generation.make_match_ctl
         run_e_under
         ~pure:(fun x -> Expr.Construct_pure x |> Generation.return)
         ~yield:(fun ~marker:marker' ~op_clause ~resumption ->
           let%map resumption =
             Generation.make_lambda_expr_2 Ctl (fun x vector ->
               let%map resume_with_x =
                 Generation.make_lambda_expr_1 Ctl (fun vector' ->
                   Expr.Application (resumption, [ x, Pure; vector', Pure ], Ctl)
                   |> Generation.return)
               in
               Expr.Application
                 ( Expr.Variable Primitive_names.prompt
                 , [ label, Pure
                   ; marker, Pure
                   ; handler, Pure
                   ; resume_with_x, Pure
                   ; vector, Pure
                   ]
                 , Ctl ))
           in
           let handle_here =
             Expr.Application
               (op_clause, [ resumption, Pure; vector, Pure ], Ctl)
           in
           let bubble_further =
             Expr.Construct_yield { marker = marker'; op_clause; resumption }
           in
           Expr.If_then_else
             (Expr.Markers_equal (marker, marker'), handle_here, bubble_further))))
;;

(** handler : (label, hnd<label,e,a>) -> (evv<label|e> -> ctl<a,label|e>, evv<label|e>) -> ctl<a,e>
*)
let handler =
  map_name_lambda
    ~name:Primitive_names.handler
    (Generation.make_lambda_2 Pure (fun label handler ->
       Generation.make_lambda_expr_2 Ctl (fun action vector ->
         Expr.Application
           ( Expr.Variable Primitive_names.prompt
           , [ label, Pure
             ; Expr.Fresh_marker, Pure
             ; handler, Pure
             ; action, Pure
             ; vector, Pure
             ]
           , Ctl )
         |> Generation.return)))
;;

(** under : (label, evv<e'>, a, (a, evv<e'>) -> ctl<e',b>) -> ctl<e,b> *)
let under =
  let open Generation.Let_syntax in
  map_name_lambda
    ~name:Primitive_names.under
    (Generation.make_lambda_4 Ctl (fun label handler_site_vector x g ->
       let run_at_handler =
         Expr.Application (g, [ x, Pure; handler_site_vector, Pure ], Ctl)
       in
       Generation.make_match_ctl
         run_at_handler
         ~pure:(fun x -> Expr.Construct_pure x |> Generation.return)
         ~yield:(fun ~marker ~op_clause ~resumption ->
           let%map resumption' =
             (* inlined [underk] *)
             Generation.make_lambda_expr_2 Ctl (fun x vector ->
               let evidence = Expr.Lookup_evidence { label; vector } in
               let handler_site_vector =
                 Expr.Get_evidence_handler_site_vector evidence
               in
               Expr.Application
                 ( Expr.Variable Primitive_names.under
                 , [ label, Pure
                   ; handler_site_vector, Pure
                   ; x, Pure
                   ; resumption, Pure
                   ]
                 , Ctl )
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
    (Generation.make_lambda_2 Pure (fun label select ->
       Generation.make_lambda_expr_2 Ctl (fun arg vector ->
         let evidence = Expr.Lookup_evidence { label; vector } in
         let handler = Expr.Get_evidence_handler evidence in
         let op = Expr.Application (select, [ handler, Pure ], Pure) in
         Generation.make_match_op
           op
           ~normal:(fun op_clause ->
             let marker = Expr.Get_evidence_marker evidence in
             (* monadic form of identity is `Pure` *)
             let%bind identity_resumption =
               Generation.make_lambda_expr_2 Ctl (fun x _vector ->
                 Expr.Construct_pure x |> Generation.return)
             in
             let%map op_clause =
               Generation.make_lambda_expr_2 Ctl (fun resume vector ->
                 Expr.Application
                   (op_clause, [ arg, Pure; resume, Pure; vector, Pure ], Ctl)
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
               , [ label, Pure
                 ; handler_site_vector, Pure
                 ; arg, Pure
                 ; op_clause, Pure
                 ]
               , Ctl )
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
