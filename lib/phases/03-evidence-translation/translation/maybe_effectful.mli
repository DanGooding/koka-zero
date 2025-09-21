open! Core
open! Import
module EPS = Evidence_passing_syntax

type t =
  | Pure of EPS.Expr.t
  | Effectful of EPS.Expr.t
[@@deriving sexp_of]

(** wrap a [Pure] term in [Construct_pure] *)
val to_effectful : t -> EPS.Expr.t

(* preserves a term's purity *)
val map : t -> f:(EPS.Expr.t -> EPS.Expr.t) -> t

(** produce a [Pure] term if both inputs are pure *)
val combine : t -> t -> f:(EPS.Expr.t -> EPS.Expr.t -> EPS.Expr.t) -> t

val combine_many : t list -> f:(EPS.Expr.t list -> EPS.Expr.t) -> t

val combine_many_with_info
  :  ('a * t) list
  -> f:(('a * EPS.Expr.t) list -> EPS.Expr.t)
  -> t

(** [make_bind_or_let (Effectful e) ~evv ~f:(fun x ~evv -> e')] builds
    the expression [(e, evv) >>= fun (x: Pure, evv) -> e'].
    For [Pure] terms it builds a let binding instead *)
val make_bind_or_let
  :  t
  -> evv:EPS.Expr.t
  -> f:(EPS.Expr.t -> evv:EPS.Expr.t -> t Generation.t)
  -> t Generation.t

val make_map_or_let
  :  t
  -> evv:EPS.Expr.t
  -> f:(EPS.Expr.t -> [ `Pure of EPS.Expr.t ] Generation.t)
  -> t Generation.t

(** [make_bind_or_let_many [(fun ~evv -> Effectful e1);...;(fun ~evv -> Effectful en)] ~evv ~f:(fun [x1;...;xn] ~evv -> Effectful e')]
    builds the expression
    [(e1, evv) >>= fun (x1: Pure, evv) -> (e2, evv) >>= fun (x2: Pure, evv) -> ... (en, evv) >>= fun (xn: Pure, evv) -> e'].
    For [Pure] terms, let-bindings are built instead *)
val make_bind_or_let_many
  :  (evv:EPS.Expr.t -> t Generation.t) list
  -> evv:EPS.Expr.t
  -> f:(EPS.Expr.t list -> evv:EPS.Expr.t -> t Generation.t)
  -> t Generation.t

(** given arguments and a body, build a lambda with effectful return type *)
val make_lambda : Parameter.t list -> t -> EPS.Expr.lambda

val make_lambda_expr : Parameter.t list -> t -> EPS.Expr.t
