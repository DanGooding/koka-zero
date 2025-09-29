module Stable = struct
  open! Core.Core_stable

  module V1 = struct
    type t =
      { clang_exe : string
      ; runtime_path : string
      ; gc_path : string option
      ; prelude_path : string option
      }
    [@@sexp.alllow_extra_fields] [@@deriving sexp]
  end

  module Latest = V1
end

open! Core
open! Import
include Stable.Latest

let example =
  { clang_exe = "/usr/bin/clang"
  ; runtime_path =
      "/Users/dan/projects/koka-zero/lib/execution/runtime/runtime.c"
  ; gc_path = Some "/opt/homebrew/Cellar/bdw-gc/8.2.8"
  ; prelude_path =
      Some "/Users/dan/projects/koka-zero/lib/execution/prelude/prelude.kk"
  }
;;

let load filename =
  Or_error.try_with (fun () ->
    let contents = In_channel.read_all filename in
    Sexp.of_string_conv_exn contents [%of_sexp: Stable.V1.t])
;;

let write t filename =
  let data = Sexp.to_string_hum ([%sexp_of: Stable.V1.t] t) in
  Or_error.try_with (fun () -> Out_channel.write_all filename ~data)
;;
