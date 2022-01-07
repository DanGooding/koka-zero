open Core
open Monadic_syntax
module E = Expr

module Names = struct
  let compose_unary = Variable.of_language_internal "compose_unary"
  let kleisli_compose_unary = Variable.of_language_internal "compose_unary"
  let bind = Variable.of_language_internal "bind"
  let pure = Variable.of_language_internal "pure"
  let prompt = Variable.of_language_internal "prompt"
  let handler = Variable.of_language_internal "handler"
  let perform = Variable.of_language_internal "perform"
end

let map_name_lambda ~(name : Variable.t) (lambda : E.lambda Generation.t)
    : E.fix_lambda Generation.t
  =
  let open Generation.Let_syntax in
  let%map lambda = lambda in
  name, lambda
;;

(** [compose_unary] is the function `\f g -> \x -> g(f(x))` *)
let compose_unary =
  map_name_lambda
    ~name:Names.compose_unary
    (Generation.make_lambda_2 (fun f g ->
         Generation.make_lambda_expr_1 (fun x ->
             E.Application (g, [ E.Application (f, [ x ]) ])
             |> Generation.return)))
;;

(** bind : ( mon<e,a>, a -> mon<e,b> ) -> mon<e, b> *)
let bind =
  map_name_lambda
    ~name:Names.bind
    (Generation.make_lambda_2 (fun e g ->
         Generation.make_lambda_expr_1 (fun vector ->
             let run_e = E.Application (e, [ vector ]) in
             Generation.make_match_ctl
               run_e
               ~pure:(fun x ->
                 E.Application (E.Application (g, [ x ]), [ vector ])
                 |> Generation.return)
               ~yield:(fun ~marker ~op_clause ~resumption ->
                 let resumption =
                   E.Application
                     (E.Variable Names.kleisli_compose_unary, [ g; resumption ])
                 in
                 E.Construct_yield { marker; op_clause; resumption }
                 |> Generation.return))))
;;

(** kleisli_compose_unary(g, f) = \x. f x >>= g *)
let kelisli_compose_unary =
  map_name_lambda
    ~name:Names.kleisli_compose_unary
    (Generation.make_lambda_2 (fun g f ->
         Generation.make_lambda_expr_1 (fun x ->
             E.Application
               (E.Variable Names.bind, [ E.Application (f, [ x ]); g ])
             |> Generation.return)))
;;

(** pure : a -> mon<e,a> *)
let pure =
  map_name_lambda
    ~name:Names.pure
    (Generation.make_lambda_1 (fun v ->
         Generation.make_lambda_expr_1 (fun _vector ->
             E.Construct_pure v |> Generation.return)))
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
                   ~pure:(fun x -> E.Construct_pure x |> Generation.return)
                   ~yield:(fun ~marker:marker' ~op_clause ~resumption ->
                     let same_prompt =
                       E.Application
                         (E.Variable Names.prompt, [ label; marker; handler ])
                     in
                     let resume_under =
                       (* resumption run under this exact prompt *)
                       E.Application
                         ( E.Variable Names.compose_unary
                         , [ same_prompt; resumption ] )
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
                     E.If_then_else
                       ( E.Markers_equal (marker, marker')
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
             E.Application
               ( E.Application
                   (E.Variable Names.prompt, [ label; E.Fresh_marker; handler ])
               , [ E.Application (action, []) ] )
             |> Generation.return)))
;;

(** perform : (label, select : hnd<e,r> -> op<a,b,e,r>) -> a -> mon<label|e> r *)
let perform =
  let open Generation.Let_syntax in
  map_name_lambda
    ~name:Names.perform
    (Generation.make_lambda_3 (fun label select arg ->
         Generation.make_lambda_expr_1 (fun vector ->
             let evidence = E.Lookup_evidence { label; vector } in
             let handler = E.Get_evidence_handler evidence in
             let marker = E.Get_evidence_marker evidence in
             let op_clause = E.Application (select, [ handler ]) in
             (* monadic form of identity is: `\x -> pure x` *)
             let identity_resumption = E.Variable Names.pure in
             let%map op_clause =
               Generation.make_lambda_expr_1 (fun resume ->
                   E.Application (op_clause, [ arg; resume ])
                   |> Generation.return)
             in
             E.Construct_yield
               { marker; op_clause; resumption = identity_resumption })))
;;
