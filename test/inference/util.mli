open Koka_zero_inference
module M = Minimal_syntax
module E = M.Expr

val print_check_program_result : M.Program.t -> unit

val print_expr_inference_result
  :  ?declarations:M.Decl.t list
  -> M.Expr.t
  -> unit

module Expr : sig
  val make_handle_expr : E.handler -> E.t -> E.t
  val decl_read : M.Decl.Effect.t
  val decl_exn : M.Decl.Effect.t
  val decl_query : M.Decl.Effect.t
  val decl_state : M.Decl.Effect.t

  val singleton_handler
    :  op_name:M.Variable.t
    -> op_argument:M.Variable.t
    -> op_body:E.t
    -> E.handler

  val read_handler : int -> E.handler
  val exn_handler : E.t -> E.handler
end
