open! Core
open! Import
open Evidence_passing_syntax

(* Any `match` where each branch's body just reconstructs the subject can be removed. *)
val remove_identity_match : Expr.t -> Expr.t Modified.t
