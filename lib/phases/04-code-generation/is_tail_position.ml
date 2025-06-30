open! Core
open! Import

type 'result t =
  | Tail_position : unit t
  | Non_tail_position : Ctl_repr.t t

let compile_conditional
      (type result)
      (t : result t)
      ~cond_i1
      ~(compile_true : unit -> result Codegen.t)
      ~(compile_false : unit -> result Codegen.t)
  : result Codegen.t
  =
  match t with
  | Tail_position ->
    Control_flow.Compile_conditional.tail_position
      ~cond_i1
      ~compile_true
      ~compile_false
  | Non_tail_position ->
    Control_flow.Compile_conditional.recombining
      ~cond_i1
      ~compile_true
      ~compile_false
      ~phi_builder:Ctl_repr.phi_builder
;;

let compile_switch
      (type result)
      (t : result t)
      subject
      ~(table : (Llvm.llvalue * string * (unit -> result Codegen.t)) list)
      ~(compile_default : unit -> result Codegen.t)
  : result Codegen.t
  =
  match t with
  | Tail_position ->
    Control_flow.Compile_switch.tail_position subject ~table ~compile_default
  | Non_tail_position ->
    Control_flow.Compile_switch.recombining
      subject
      ~table
      ~compile_default
      ~phi_builder:Ctl_repr.phi_builder
;;
