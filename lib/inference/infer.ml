let infer_type e = failwith "not implemented"

(* requirements: - name source - free variables of a thing - substitutions -
   contexts (composition etc.)

   chain together as a monad? *)
let rec infer (e : Minimal_syntax.Expr.t) some_pass_through_params =
  match e with
  | Literal lit -> infer_literal lit
  | Variable var ->
    (match Context.find var with
    | Some t -> t
    | None -> Static_error.type_error)
  | Let (x, subject, body) ->
    let t_subject = infer subject in
    let t_subject = generalise t_subject in
    (* remember to keep applying the subtitutions everywhere *)
    let context' = Context.extend ~var:x ~type_:t_subject context in
    infer body context'
  | Lambda (x, body) ->
    let t_param = fresh_variable () in
    let context' = Context.extend ~var:x ~type_:(Type.Variable t_param) in
    infer body context'
;;

let infer_literal : Minimal_syntax.Literal.t -> Type.Literal.t = function
  | Int _ -> Int
  | Bool _ -> Bool
  | Unit -> Unit
;;
