open! Core
open! Import

(** a variant used to specify if if an expression is in tail-position,
    and the return type of a function compiling such an expression.

    For a tail position expression, we compile it and also generate a [return],
    and since no instructions will come after this, the result will be [unit].

    For all other expressions, we return the llvalue(s) holding the
    expression's result, which can be used in subsequent instructions. *)
type 'result t =
  | Tail_position : unit t
  | Non_tail_position : Ctl_repr.t t

val compile_conditional
  :  'result t
  -> 'result Control_flow.Compile_conditional.t

val compile_switch : 'result t -> 'result Control_flow.Compile_switch.t
