open! Core
open! Import
open Evidence_passing_syntax

let is_name : Expr.t -> Variable.t -> bool =
  fun e name ->
  match[@warning "-4"] e with
  | Expr.Variable v -> Variable.(name = v)
  | _ -> false
;;

(** apply the monad law [return e1 >>= (fun y -> e2)] --> [let y = e1 in e2]

    which in the uncurried form is [(Pure e1, evv) >>= (fun y evv' -> e2)] -->
    [let y = e1 in let evv' = evv in e2] *)
let left_unit : Expr.t -> Expr.t Modified.t =
  Modified.original_for_none (function [@warning "-4"]
    | Expr.Application
        ( bind
        , [ Expr.Construct_pure e1; vector; Expr.Lambda ([ y; vector' ], e2) ]
        )
      when is_name bind Primitive_names.bind ->
      Expr.Let (y, e1, Expr.Let (vector', vector, e2)) |> Some
    | _ -> None)
;;

let rewrite_program (program : Program.t) =
  let open Generation.Let_syntax in
  let%map program = Bind_inlining.rewrite_program program in
  Modified.apply_while_changes
    ~f:(Rewriting_utils.apply_everywhere_to_program ~rewrite:left_unit)
    program
  |> Modified.value
;;
