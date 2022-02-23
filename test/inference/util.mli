open Koka_zero_inference
module M = Minimal_syntax
module E = M.Expr

val print_check_program_result : M.Program.t -> unit
val print_check_program_without_main_result : M.Program.t -> unit

val print_expr_inference_result
  :  ?declarations:M.Decl.t list
  -> M.Expr.t
  -> unit

module Parameter : sig
  val var : string -> M.Parameter.t
  val wildcard : M.Parameter.t
end

module Expr : sig
  val var : string -> E.t
  val lit_bool : bool -> E.t
  val lit_int : int -> E.t
  val lit_unit : E.t
  val decl_main : M.Decl.t
  val make_handle_expr : E.handler -> E.t -> E.t
  val decl_read : M.Decl.Effect.t
  val decl_exn : M.Decl.Effect.t
  val decl_query : M.Decl.Effect.t
  val decl_state : M.Decl.Effect.t
  val decl_choose : M.Decl.Effect.t

  val singleton_handler
    :  op_name:Variable.t
    -> op_argument:M.Parameter.t
    -> op_body:E.t
    -> shape:Operation_shape.t
    -> E.handler

  val read_handler : int -> E.handler
  val exn_handler : E.t -> E.handler
end
