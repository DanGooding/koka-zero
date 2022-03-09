open Core
open Import
open Evidence_passing_syntax

let is_name : Expr.t -> Variable.t -> bool =
 fun e name ->
  match[@warning "-4"] e with
  | Expr.Variable v -> Variable.(name = v)
  | _ -> false
;;

let require_is_name : Expr.t -> Variable.t -> unit option =
 fun e name -> if is_name e name then Some () else None
;;

(** apply the monad law [return e1 >>= (fun y -> e2)] --> [let y = e1 in e2] *)
let left_unit : Expr.t -> Expr.t Modified.t =
  Modified.original_for_none (function [@warning "-4"]
      | Expr.Application
          (bind, [ Expr.Application (pure, [ e1 ]); Expr.Lambda ([ y ], e2) ])
        ->
        let open Option.Let_syntax in
        let%bind () = require_is_name bind Primitives.Names.bind in
        let%map () = require_is_name pure Primitives.Names.pure in
        Expr.Let (y, e1, e2)
      | _ -> None)
;;

let rewrite_program : Program.t -> Program.t =
 fun program ->
  Modified.apply_while_changes
    ~f:(Rewriting_utils.apply_everywhere_to_program ~rewrite:left_unit)
    program
  |> Modified.value
;;
