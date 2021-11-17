open Core
open Minimal_syntax

let infer_literal : Literal.t -> Type.Primitive.t = function
  | Literal.Int _ -> Type.Primitive.Int
  | Literal.Bool _ -> Type.Primitive.Bool
  | Literal.Unit -> Type.Primitive.Unit
;;

let rec infer : Context.t -> Expr.t -> Type.Mono.t Inference.t =
 fun env e ->
  let open Inference.Let_syntax in
  match e with
  | Expr.Literal lit -> Type.Mono.Primitive (infer_literal lit) |> return
  | Expr.Variable var ->
    (match Context.find env var with
    | None ->
      let message = sprintf "unbound variable: %s" (Variable.to_string var) in
      Inference.type_error message
    | Some t ->
      (match t with
      | Type.Mono t -> return t
      | Type.Poly s -> Inference.instantiate s)
      (* TODO: instantiation occurs within the Infer monad *))
  | Expr.Application (e_f, e_arg) ->
    let%bind t_f = infer env e_f in
    let%bind t_arg = infer env e_arg in
    let%bind t_result = Inference.fresh_metavariable in
    let t_result = Type.Mono.Metavariable t_result in
    let%map () = Inference.unify t_f (Type.Mono.Arrow (t_arg, t_result)) in
    (* TODO: definitely need to backsubstitute over the result - here we give a
       metavariable as the type *)
    t_result
  | Expr.Let (_, _, _)
  | Expr.Lambda (_, _)
  | Expr.Fix (_, _)
  | Expr.If_then_else (_, _, _) -> failwith "not implemented"
;;

(* | Expr. *)
(* appel p359 *)

let infer_type e =
  let _r = Inference.run (infer Context.empty e) in
  failwith "not implemented"
;;
(* TODO: [infer] doesn't give the final type, get that here (could save this
   type on the tree, and then apply the global substitution over the whole tree
   at the end) *)
