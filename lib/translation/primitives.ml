open Core
open Monadic_syntax
module E = Expr

(* TODO: these are essentially a prelude *)
(* TODO: names should be global constants somewhere, definitions can be
   generated once separate from usage *)

module Names = struct
  let compose_unary = Variable.of_language_internal "compose_unary"
  let bind = Variable.of_language_internal "bind"
  let pure = Variable.of_language_internal "pure"
  let prompt = Variable.of_language_internal "prompt"
  let handler = Variable.of_language_internal "handler"
  let perform = Variable.of_language_internal "perform"
end

let map_name_lambda ~(name : Variable.t) (lambda : E.lambda t) : E.fix_lambda t =
  let open Generation.Let_syntax in
  let%map lambda = lambda in
  name, lambda
;;

(** [compose_unary g f] returns the function `\x -> g(f(x))` *)
let compose_unary =
  map_name_lambda
    ~name:Names.compose_unary
    ( Names.compose_unary
    , Generation.make_lambda_expr_1 (fun x ->
          E.Applicaton (g, [ E.Application (f, [ x ]) ])) )
;;

(** bind : ( mon<e,a>, a -> mon<e,b> ) -> mon<e, b> *)
let bind =
  map_name_lambda
    ~name:Names.bind
    (Generation.make_lambda_expr_2 (fun e g ->
         Generation.make_lambda_expr_1 (fun vector ->
             let run_e = E.Application (e, [ vector ]) in
             Generation.make_match_ctl
               run_e
               ~pure:(fun x ->
                 E.Application (E.Application (g, [ x ]), [ vector ]))
               ~yield:(fun ~marker ~op_clause ~resumption ->
                 let resumption =
                   E.Application (Names.kleisli_compose_unary, [ g, resumption ])
                 in
                 E.Construct_yield { marker; op_clause; resumption }))))
;;

(** kleisli_compose_unary(g, f) = \x. f x >>= g *)
let kelisli_compose_unary =
  map_name_lambda
    ~name:Names.kleisli_compose_unary
    (Generation.make_lambda_expr_2 (fun g f ->
         Generation.make_lambda_expr_1 (fun x ->
             E.Application (Names.bind, [ E.Application (f, [ x ]), g ]))))
;;

(** pure : a -> mon<e,a> *)
let pure =
  map_name_lambda
    ~name:Names.pure
    (Generation.make_lambda_expr_1 (fun v ->
         Generation.make_lambda_expr_1 (fun _vector -> v)))
;;

(** prompt : (label, marker, hnd<label,e,a>) -> (mon<label|e> a) -> mon<e> a *)
let prompt =
  map_name_lambda
    ~name:Names.prompt
    (Generation.make_lambda_3 (fun label marker handler ->
         Generation.make_lambda_expr_1 (fun e ->
             Generation.make_lambda_expr_1 (fun vector ->
                 let vector' =
                   E.Cons_evidence_vector
                     { label; marker; handler; vector_tail = vector }
                 in
                 let run_e_under = E.Application (e, [ vector' ]) in
                 Generation.make_match_ctl
                   run_e_under
                   ~pure:(fun x -> E.Construct_pure x)
                   ~yield:(fun ~marker:marker' ~op_clause ~resumption ->
                     let same_prompt =
                       E.Application (Names.prompt, [ label; marker; handler ])
                     in
                     let resume_under =
                       (* reumption run under this exact prompt *)
                       E.Application
                         (Names.compose_unary, [ same_prompt; resumption ])
                     in
                     let bubble_further =
                       E.Construct_yield
                         { marker = marker'
                         ; op_clause
                         ; resumption = resume_under
                         }
                     in
                     let handle_here =
                       E.Application
                         ( E.Application (op_clause, [ resume_under ])
                         , [ vector ] )
                     in
                     E.If
                       ( E.Markers_equal (marker, marker')
                       , handle_here
                       , bubble_further ))))))
;;

(** handler : (label, hnd<label,e,a>) -> (action : () -> mon<label|e> a) ->
    mon<e> a *)
let handler =
  map_name_lambda
    ~name:Names.handler
    (Generation.make_lambda_2 (fun label handler ->
         Generate.make_lambda_expr (fun action ->
             E.Application
               ( E.Application (Names.prompt, [ label; Fresh_marker; handler ])
               , [ E.Application (Variable x_action, []) ] ))))
;;

(** perform : (label, select : hnd<e,r> -> op<a,b,e,r>) -> a -> mon<label|e> r *)
let perform =
  map_name_lambda
    ~name:Names.perform
    (Generation.make_lambda_3 (fun label select arg ->
         Generation.make_lambda_expr_1 (fun vector ->
             let evidence = E.Lookup_evidence { label; vector } in
             let handler = E.Get_evidence_handler evidence in
             let op_clause = E.Application (select, [ handler ]) in
             let identity_resumption =
               Generation.make_lambda_1 (fun x ->
                   Generation.make_lambda_1 (fun _vector -> E.Construct_pure x))
             in
             let op_clause =
               Generation.make_lambda_1 (fun resume ->
                   E.Application (op_clause, [ x; resume ]))
             in
             E.Construct_yield
               { marker; op_clause; resumption = identity_resumption })))
;;
