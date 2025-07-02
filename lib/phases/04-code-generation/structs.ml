open! Core
open! Import

module Evidence_entry = struct
  module T = struct
    module Field = struct
      type t =
        | Handler
        | Marker
        | Handler_site_vector
      [@@deriving equal]

      let all = [ Handler; Marker; Handler_site_vector ]

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
    module Field = struct
      type t =
        | Marker
        | Op_clause
        | Resumption
      [@@deriving equal]

      let all = [ Marker; Op_clause; Resumption ]

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
