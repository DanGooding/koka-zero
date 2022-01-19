open Core
open Koka_zero_util
open Import

module Mutable_state = struct
  type t =
    { context : Llvm.llcontext
    ; module_ : Llvm.llmodule
    ; builder : Llvm.llbuilder
    }

  let create_initial ~module_id =
    let context = Llvm.global_context () in
    let module_ = Llvm.create_module context module_id in
    let builder = Llvm.builder context in
    { context; module_; builder }
  ;;
end

module State = struct
  type t = { symbol_suffix_source : Symbol_name.Suffix_source.t }
  [@@deriving sexp]

  let initial = { symbol_suffix_source = Symbol_name.Suffix_source.fresh }
end

module T = struct
  type 'a t = Mutable_state.t -> State.t -> ('a * State.t) Or_codegen_error.t

  let return x _mstate state = Result.Ok (x, state)

  let bind x ~f mstate state =
    let%bind.Result y, state' = x mstate state in
    f y mstate state'
  ;;

  let map =
    let map x ~f mstate state =
      let%map.Result y, state' = x mstate state in
      f y, state'
    in
    `Custom map
  ;;
end

module T' = struct
  include T
  include Monad.Make (T)
end

include T'
include Monad_utils.Make (T')

let dump_module { Mutable_state.module_; _ } state =
  Llvm.dump_module module_;
  Result.Ok ((), state)
;;

let write_module ~filename { Mutable_state.module_; _ } state =
  Llvm.print_module filename module_;
  Result.Ok ((), state)
;;

(** run a computation, in a fresh context, then cleanup that context. The
    computation may only return unit to reduce the chance of leaking a deleted
    context object *)
let run ~module_id (t : unit t) : unit Or_codegen_error.t =
  let mstate = Mutable_state.create_initial ~module_id in
  let state = State.initial in
  let result = t mstate state in
  let { Mutable_state.context; module_; builder = _ } = mstate in
  Llvm.dispose_module module_;
  Llvm.dispose_context context;
  let%map.Result (), _state = result in
  ()
;;

let run_then_write_module ~module_id ~filename (t : unit t)
    : unit Or_codegen_error.t
  =
  let open Let_syntax in
  run
    ~module_id
    (let%bind () = t in
     write_module ~filename)
;;

let run_then_dump_module ~module_id (t : unit t) : unit Or_codegen_error.t =
  let open Let_syntax in
  run
    ~module_id
    (let%bind () = t in
     dump_module)
;;

let use_builder f { Mutable_state.builder; _ } state =
  Result.Ok (f builder, state)
;;

let use_context f { Mutable_state.context; _ } state =
  Result.Ok (f context, state)
;;

let use_module f { Mutable_state.module_; _ } state =
  Result.Ok (f module_, state)
;;

let check_module_valid { Mutable_state.module_; _ } state =
  match Llvm_analysis.verify_module module_ with
  | None -> Result.Ok ((), state)
  | Some report -> Codegen_error.verifier_error report |> Result.Error
;;

let within_block local_block ~f =
  let open Let_syntax in
  let%bind previous_block = use_builder Llvm.insertion_block in
  let%bind () = use_builder (Llvm.position_at_end local_block) in
  let%bind () = f () in
  use_builder (Llvm.position_at_end previous_block)
;;

let impossible_error message _mstate _state =
  Codegen_error.impossible_error message |> Result.Error
;;

let unsupported_feature_error message _mstate _state =
  Codegen_error.unsupported_feature_error message |> Result.Error
;;

let symbol_of_local_name ~containing v _mstate state =
  let { State.symbol_suffix_source } = state in
  let symbol, symbol_suffix_source =
    Symbol_name.of_local ~containing v symbol_suffix_source
  in
  let state = { State.symbol_suffix_source } in
  Result.Ok (symbol, state)
;;

let symbol_of_local_anonymous ~containing _mstate state =
  let { State.symbol_suffix_source } = state in
  let symbol, symbol_suffix_source =
    Symbol_name.of_local_anonymous ~containing symbol_suffix_source
  in
  let state = { State.symbol_suffix_source } in
  Result.Ok (symbol, state)
;;
