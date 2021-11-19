(* TODO: is the aim to annotate the tree, or just to check it and then forget
   about types *)
open Core
open Koka_zero_util

val infer_type : Minimal_syntax.Expr.t -> (Type.Mono.t, Static_error.t) Result.t
