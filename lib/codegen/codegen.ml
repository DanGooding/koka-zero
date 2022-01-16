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

module T = struct
  type 'a t = Mutable_state.t -> 'a Or_codegen_error.t

  let return x _state = Result.Ok x

  let bind x ~f state =
    let%bind.Result y = x state in
    f y state
  ;;

  let map =
    let map x ~f state =
      let%map.Result y = x state in
      f y
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

let dump_module { Mutable_state.module_; _ } =
  Llvm.dump_module module_;
  Result.Ok ()
;;

let write_module ~filename { Mutable_state.module_; _ } =
  Llvm.print_module filename module_;
  Result.Ok ()
;;

(** run a computation, in a fresh context, then cleanup that context. The
    computation may only return unit to reduce the chance of leaking a deleted
    context object *)
let run ~module_id (t : unit t) : unit Or_codegen_error.t =
  let state = Mutable_state.create_initial ~module_id in
  let result = t state in
  let { Mutable_state.context; module_; builder = _ } = state in
  Llvm.dispose_module module_;
  Llvm.dispose_context context;
  result
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

let use_builder f { Mutable_state.builder; _ } = f builder |> Result.Ok
let use_context f { Mutable_state.context; _ } = f context |> Result.Ok
let use_module f { Mutable_state.module_; _ } = f module_ |> Result.Ok

let current_block { Mutable_state.builder; _ } =
  Llvm.insertion_block builder |> Result.Ok
;;

let impossible_error message _state =
  Codegen_error.impossible_error message |> Result.Error
;;

let unsupported_feature_error message _state =
  Codegen_error.unsupported_feature_error message |> Result.Error
;;
