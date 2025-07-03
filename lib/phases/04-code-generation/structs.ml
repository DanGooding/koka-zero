open! Core
open! Import

module Evidence_entry = struct
  module T = struct
    type t = unit

    module Field = struct
      type t =
        | Handler
        | Marker
        | Handler_site_vector
      [@@deriving equal]

      let all () = [ Handler; Marker; Handler_site_vector ]

      let name t =
        match t with
        | Handler -> "handler"
        | Marker -> "marker"
        | Handler_site_vector -> "handler_site_vector"
      ;;

      let type_ t =
        match t with
        | Handler -> Types.pointer
        | Marker -> Types.marker
        | Handler_site_vector -> Types.pointer
      ;;
    end
  end

  include T
  include Struct.Make (T)
end

module Ctl_yield = struct
  module T = struct
    type t = unit

    module Field = struct
      type t =
        | Marker
        | Op_clause
        | Resumption
      [@@deriving equal]

      let all () = [ Marker; Op_clause; Resumption ]

      let name t =
        match t with
        | Marker -> "marker"
        | Op_clause -> "op_clause"
        | Resumption -> "resumption"
      ;;

      let type_ t =
        match t with
        | Marker -> Types.marker
        | Op_clause -> Types.pointer
        | Resumption -> Types.pointer
      ;;
    end
  end

  include T
  include Struct.Make (T)
end

module Op = struct
  module Tag = struct
    let type_ = Codegen.use_context Llvm.i8_type

    let const_tag i =
      let open Codegen.Let_syntax in
      let%map type_ = type_ in
      Llvm.const_int type_ i
    ;;

    let const_normal = const_tag 0
    let const_tail = const_tag 1
  end

  module T = struct
    type t = unit

    module Field = struct
      type t =
        | Tag
        | Clause
      [@@deriving equal]

      let all () = [ Tag; Clause ]

      let name t =
        match t with
        | Tag -> "op_tag"
        | Clause -> "op_clause"
      ;;

      let type_ t =
        match t with
        | Tag -> Tag.type_
        | Clause -> Types.pointer
      ;;
    end
  end

  include T
  include Struct.Make (T)
end
