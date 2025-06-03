open! Core
module EPS = Evidence_passing_syntax

(*  TODO: consider a GADT where [Pure e : Expr.t t] and [Effectful e : (evv:Expr.t -> Expr.t) t]
    it would be nice to enforce that Pure code won't accidentally use [evv] *)
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

(** [make_bind_or_let (Effectful e) ~evv ~f:(fun x ~evv -> e')] builds
    the expression [(e, evv) >>= fun (x, evv) -> e'].
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
    [(e1, evv) >>= fun (x1, evv) -> (e2, evv) >>= fun (x2, evv) -> ... (en, evv) >>= fun xn -> e'].
    For [Pure] terms, let-bindings are built instead *)
val make_bind_or_let_many
  :  (evv:EPS.Expr.t -> t Generation.t) list
  -> evv:EPS.Expr.t
  -> f:(EPS.Expr.t list -> evv:EPS.Expr.t -> t Generation.t)
  -> t Generation.t

(** given arguments and a body, build a lambda with effectful return type *)
val make_lambda : EPS.Parameter.t list -> t -> EPS.Expr.lambda

val make_lambda_expr : EPS.Parameter.t list -> t -> EPS.Expr.t
