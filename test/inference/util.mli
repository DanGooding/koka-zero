open! Core
open! Import

val print_check_program_result : M.Program.t -> unit
val print_check_program_without_main_result : M.Program.t -> unit

val print_expr_inference_result
  :  ?declarations:M.Decl.t list
  -> M.Expr.t
  -> unit

module Parameter : sig
  include module type of Parameter

  val var : string -> Parameter.t
  val wildcard : Parameter.t
end

module Expr : sig
  val var : string -> E.t
  val lit_bool : bool -> E.t
  val lit_int : int -> E.t
  val lit_unit : E.t
  val decl_main : M.Decl.t
  val make_handle_expr : E.handler -> E.t -> E.t
  val decl_read : Effect_decl.t
  val decl_exn : Effect_decl.t
  val decl_query : Effect_decl.t
  val decl_state : Effect_decl.t
  val decl_choose : Effect_decl.t

  val singleton_handler
    :  op_name:Variable.t
    -> op_argument:Parameter.t
    -> op_body:E.t
    -> shape:Operation_shape.t
    -> E.handler

  val read_handler : int -> E.handler
  val exn_handler : E.t -> E.handler
end
