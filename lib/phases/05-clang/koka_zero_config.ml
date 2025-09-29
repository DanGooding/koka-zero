module Stable = struct
  open! Core.Core_stable

  module Frontend = struct
    module V1 = struct
      type t = { prelude_path : string option } [@@deriving sexp]
    end

    module Latest = V1
  end

  module Backend = struct
    module V1 = struct
      type t =
        { clang_exe : string
        ; runtime_path : string
        ; gc_path : string option
        }
      [@@deriving sexp]
    end
  end

  module V2 = struct
    type t =
      { frontend_config : Frontend.V1.t
      ; backend_config : Backend.V1.t
      }
    [@@deriving sexp]
  end

  module V1 = struct
    type t =
      { clang_exe : string
      ; runtime_path : string
      ; gc_path : string option
      }
    [@@deriving sexp]

    let to_v2 { clang_exe; runtime_path; gc_path } : V2.t =
      { frontend_config = { prelude_path = None }
      ; backend_config = { clang_exe; runtime_path; gc_path }
      }
    ;;
  end

  module Latest = V2
end

open! Core
open! Import
include Stable.Latest

let example =
  { frontend_config =
      { prelude_path =
          Some "/Users/dan/projects/koka-zero/lib/execution/prelude/prelude.kk"
      }
  ; backend_config =
      { clang_exe = "/usr/bin/clang"
      ; runtime_path =
          "/Users/dan/projects/koka-zero/lib/execution/runtime/runtime.c"
      ; gc_path = Some "/opt/homebrew/Cellar/bdw-gc/8.2.8"
      }
  }
;;

module Frontend = struct
  include Stable.Frontend.Latest

  let load filename =
    Or_error.try_with (fun () ->
      let contents = In_channel.read_all filename in
      let raw_sexp = Sexp.of_string contents in
      match [%of_sexp: [ `v1 of Sexp.t ]] raw_sexp with
      | `v1 v1_sexp -> [%of_sexp: Stable.Frontend.V1.t] v1_sexp)
  ;;

  let write t filename =
    let data =
      Sexp.to_string_hum ([%sexp_of: [ `v1 ] * Stable.Frontend.V1.t] (`v1, t))
    in
    Or_error.try_with (fun () -> Out_channel.write_all filename ~data)
  ;;
end

module Backend = struct
  include Stable.Backend.V1
end

let load filename =
  Or_error.try_with (fun () ->
    let contents = In_channel.read_all filename in
    let raw_sexp = Sexp.of_string contents in
    (* V1 config files aren't prefixed with a version *)
    match [%of_sexp: [ `v2 of Sexp.t ]] raw_sexp with
    | `v2 v2_sexp -> [%of_sexp: Stable.V2.t] v2_sexp
    | exception _ -> [%of_sexp: Stable.V1.t] raw_sexp |> Stable.V1.to_v2)
;;

let write t filename =
  let data = Sexp.to_string_hum ([%sexp_of: [ `v2 ] * Stable.V2.t] (`v2, t)) in
  Or_error.try_with (fun () -> Out_channel.write_all filename ~data)
;;
