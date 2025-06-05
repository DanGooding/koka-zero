open! Core
open! Import
module EPS = Evidence_passing_syntax

type t =
  | Pure of EPS.Expr.t
  | Effectful of EPS.Expr.t
[@@deriving sexp_of]

let map t ~f =
  match t with
  | Pure expr -> Pure (f expr)
  | Effectful expr -> Effectful (f expr)
;;

let to_effectful t : EPS.Expr.t =
  match t with
  | Pure expr -> EPS.Expr.Construct_pure expr
  | Effectful expr -> expr
;;

let combine t t' ~f =
  match t, t' with
  | Pure e, Pure e' -> Pure (f e e')
  | Effectful _, (Pure _ | Effectful _) | Pure _, Effectful _ ->
    let e = to_effectful t in
    let e' = to_effectful t' in
    Effectful (f e e')
;;

let make_bind_or_let
      t
      ~evv
      ~(f : EPS.Expr.t -> evv:EPS.Expr.t -> t Generation.t)
  =
  let open Generation.Let_syntax in
  match t with
  | Pure expr ->
    (* let x = [expr] in [f x] *)
    let%bind x = Generation.fresh_name in
    let%map rhs = f (EPS.Expr.Variable x) ~evv in
    map rhs ~f:(fun rhs -> EPS.Expr.Let (Variable x, expr, rhs))
  | Effectful expr ->
    (* bind [expr] [evv] (fun x evv -> [f x]) *)
    let%map rhs =
      Generation.make_lambda_expr_2 (fun x evv ->
        (* TODO: if [result] is [Pure] emit a call to [map]
           instead of [bind] - if [map] is cheaper *)
        let%map result = f x ~evv in
        to_effectful result)
    in
    Effectful
      (EPS.Expr.Application (Variable Primitive_names.bind, [ expr; evv; rhs ]))
;;

let make_map_or_let t ~evv ~f =
  let open Generation.Let_syntax in
  make_bind_or_let t ~evv ~f:(fun x ~evv:_ ->
    let%map (`Pure rhs) = f x in
    Pure rhs)
;;

let make_bind_or_let_many =
  let open Generation.Let_syntax in
  let rec make_bind_or_let_many ts ~evv ~xs_rev ~f =
    match ts with
    | [] ->
      let xs = List.rev xs_rev in
      f xs ~evv
    | t :: ts' ->
      let%bind m = t ~evv in
      make_bind_or_let m ~evv ~f:(fun x ~evv ->
        make_bind_or_let_many ts' ~evv ~xs_rev:(x :: xs_rev) ~f)
  in
  fun ts ~evv ~f -> make_bind_or_let_many ts ~evv ~xs_rev:[] ~f
;;

let make_lambda params body =
  let body = to_effectful body in
  params, body
;;

let make_lambda_expr params body = EPS.Expr.Lambda (make_lambda params body)
