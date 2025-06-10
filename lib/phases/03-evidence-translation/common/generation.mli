open Core
open Import
module EPS = Evidence_passing_syntax
module E = EPS.Expr

(** monad encapsulating a name source's state for fresh variables *)
type 'a t

include Monad.S with type 'a t := 'a t
include Monad_utils.S with type 'a t := 'a t

(** perform a generation computation with names using a specifed prefix *)
val run : ?name_prefix:string -> 'a t -> 'a Or_static_error.t

(** fail due to attempting to translate a feature which is not yet implemented
*)
val unsupported_feature_error : string -> 'a t

(** create a globally unique variable *)
val fresh_name : Variable.t t

(* note passing parameters as expressions rather than variables is less
   descriptive, but generally allows more concise usage *)

(** [make_lambda_1 t f] builds a 1 argument lambda, whose argument is [x : Type.Pure], and
    whose body is [f (E.Variable x)] for some fresh name [x], with return type [t]
*)
val make_lambda_1 : EPS.Type.t -> (E.t -> E.t t) -> E.lambda t

(** [make_lambda_2 f] builds a 2 argument lambda, whose arguments are [x : Type.Pure] and
    [y : Type.Pure], and whose body is [f (E.Variable x) (E.Variable y)] for some fresh
    names [x] and [y] *)
val make_lambda_2 : EPS.Type.t -> (E.t -> E.t -> E.t t) -> E.lambda t

(** [make_lambda_n f] builds an [n] argument lambda, whose arguments are
    [ [x1; ...; xn] ], and whose body is
    [f (E.Variable x1) ... (E.Variable xn)] for some fresh names [x1] ... [xn]
*)
val make_lambda_3 : EPS.Type.t -> (E.t -> E.t -> E.t -> E.t t) -> E.lambda t

val make_lambda_4
  :  EPS.Type.t
  -> (E.t -> E.t -> E.t -> E.t -> E.t t)
  -> E.lambda t

val make_lambda_5
  :  EPS.Type.t
  -> (E.t -> E.t -> E.t -> E.t -> E.t -> E.t t)
  -> E.lambda t

(* analogues of [make_lambda_n] which wrap the result into an expression *)
val make_lambda_expr_1 : EPS.Type.t -> (E.t -> E.t t) -> E.t t
val make_lambda_expr_2 : EPS.Type.t -> (E.t -> E.t -> E.t t) -> E.t t
val make_lambda_expr_3 : EPS.Type.t -> (E.t -> E.t -> E.t -> E.t t) -> E.t t

val make_lambda_expr_4
  :  EPS.Type.t
  -> (E.t -> E.t -> E.t -> E.t -> E.t t)
  -> E.t t

val make_lambda_expr_5
  :  EPS.Type.t
  -> (E.t -> E.t -> E.t -> E.t -> E.t -> E.t t)
  -> E.t t

(** builds a [Match_ctl] expression in a similar way to [make_lambda_n], but
    labelling the parameters in the yield branch for clarity *)
val make_match_ctl
  :  E.t
  -> pure:(E.t -> E.t t)
  -> yield:(marker:E.t -> op_clause:E.t -> resumption:E.t -> E.t t)
  -> E.t t

(** builds a [Match_op] in the same way as [make_match_ctl] *)
val make_match_op : E.t -> normal:(E.t -> E.t t) -> tail:(E.t -> E.t t) -> E.t t
