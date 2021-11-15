open Minimal_syntax

let infer_type _e = failwith "not implemented"

(* requirements: - name source - free variables of a thing - substitutions -
   contexts (composition etc.)

   chain together as a monad? *)

let infer_literal : Literal.t -> Type.Literal.t = function
  | Int _ -> Int
  | Bool _ -> Bool
  | Unit -> Unit
;;

let rec infer (env : Context.t) (e : Expr.t) : Substitution.t * Type.t =
  match e with
  | Expr.Literal lit -> infer_literal lit
  | Expr.Variable var ->
    (match Context.find env var with
     | None ->
       let message = sprintf "unbound variable: %s" (Variable.to_string var) in
       Static_error.type_error message
     | Some t ->
      let t =
        match t with
        | Mono t -> t
        | Poly s -> Type.Poly.instantiate ~name_source s
      in
      Substitution.identity, t)
  | Expr.Apply(e_f, e_arg) -> (
      let s0, t_f = infer env e_f in
      let s1, t_arg = infer env e_arg in
      let t_result = Type.Variable (fresh_name ()) in
      let s2 = unify (Substitution.apply s1 t_t) (Type.Arrow(t_arg, t_result)) in
      Substitution.compose s1 s0, Substitution.apply s2 t_result
      )
  | Expr.If_then_else(e_cond, e_yes, e_no) -> (
      (* this is unreadable... *)
      let s0, t_cond = infer env e_cond in
      let s1 = unify t_cond Type.Primitive.Bool in
      let s1s0 = Substitution.compose s1 s0 in
      let env' = Substitution.apply (s1s0) env in
      let s2, t_yes = infer env' e_yes in
      let s2s1s0 = Substitution.compose s2 s1s0 in
      let env'' = Substitution.apply (s2s1s0) env in
      let s2_no, t_no = infer env'' e_no in
      let _ = () in
      ()

    (*  BE CAREFUL - generalisation should act on a type after all substitutions have
        been applied
        e.g.  forall a. a->a
        is different to  forall b. (b->b) -> (b->b)
    *)
      (* let s0, t_cond = infer env e_cond in *)
      (* let env' = Substitution.apply s0, env in *)
      (* let s1, t_yes = infer (env') e_yes in *)
      (* let s2, t_no = infer (env') e_no in *)
      (* let s3 = unify t_cond (Type.Primitive Type.Primitive.Bool) *)
      (* let s4 = unify t_yes t_no in *)





  | Let (x, subject, body) ->
    let s0, t_subject = infer subject in
    let env = Substitution.apply s env in
    let poly_subject = Type.Scheme.generalise ~in_:env t_subject in
    let env' = Context.extend env ~var:x ~type_:(Type.Poly poly_subject) in
    let s1, t_body = infer env' body in
    Substitution.compose s1 s0, t_body

  | Lambda (x, body) ->
    let t_param = fresh_variable () in
    let context' = Context.extend ~var:x ~type_:(Type.Mono ( Type.Variable t_param )) in
    let s, t_body = infer body context' in
    s, Type.Arrow(Substitution.apply s t_param, t_body)

  | Fix (f, body) ->
;;
