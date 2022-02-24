open Core
open Koka_zero_util
open Import
module EPS = Evidence_passing_syntax
module E = EPS.Expr

(** monad encapsulating a name source's state for fresh variables *)
type 'a t

include Monad.S with type 'a t := 'a t
include Monad_utils.S with type 'a t := 'a t

(** perform a generation computation using the default internal name source *)
val run : 'a t -> 'a Or_static_error.t

(** fail due to attempting to translate a feature which is not yet implemented *)
val unsupported_feature_error : string -> 'a t

(** create a globally unique variable *)
val fresh_name : Variable.t t

(* note passing parameters as expressions rather than variables is less
   descriptive, but generally allows more concise usage *)

(** [make_lambda_1 f] builds a 1 argument lambda, whose argument is [x], and
    whose body is [f (E.Variable x)] for some fresh name [x] *)
val make_lambda_1 : (E.t -> E.t t) -> E.lambda t

(** [make_lambda_2 f] builds a 2 argument lambda, whose arguments are [x] and
    [y], and whose body is [f (E.Variable x) (E.Variable y)] for some fresh
    names [x] and [y] *)
val make_lambda_2 : (E.t -> E.t -> E.t t) -> E.lambda t

(** [make_lambda_n f] builds an [n] argument lambda, whose arguments are
    [ \[x1; ...; xn\] ], and whose body is
    [f (E.Variable x1) ... (E.Variable xn)] for some fresh names [x1] ... [xn] *)
val make_lambda_3 : (E.t -> E.t -> E.t -> E.t t) -> E.lambda t

val make_lambda_4 : (E.t -> E.t -> E.t -> E.t -> E.t t) -> E.lambda t

(* analogues of [make_lambda_n] which wrap the result into an expression *)
val make_lambda_expr_1 : (E.t -> E.t t) -> E.t t
val make_lambda_expr_2 : (E.t -> E.t -> E.t t) -> E.t t
val make_lambda_expr_3 : (E.t -> E.t -> E.t -> E.t t) -> E.t t
val make_lambda_expr_4 : (E.t -> E.t -> E.t -> E.t -> E.t t) -> E.t t

(** builds a [Match_ctl] expression in a similar way to [make_lambda_n], but
    labelling the parameters in the yield branch for clarity *)
val make_match_ctl
  :  E.t
  -> pure:(E.t -> E.t t)
  -> yield:(marker:E.t -> op_clause:E.t -> resumption:E.t -> E.t t)
  -> E.t t
