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
