open Core
open Koka_zero_util
module Type = Type
module Minimal_syntax = Minimal_syntax

(* TODO: this should interface with [Koka_zero_parsing] *)
val infer_type : Minimal_syntax.Expr.t -> (Type.Mono.t, Static_error.t) Result.t
