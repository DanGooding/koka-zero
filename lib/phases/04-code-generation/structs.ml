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

module Closure = struct
  module T = struct
    type t = { num_captured : int }

    module Field = struct
      type t =
        | Code_address
        | Capture of { index : int }
      [@@deriving equal]

      let all { num_captured } =
        Code_address
        :: List.init num_captured ~f:(fun index -> Capture { index })
      ;;

      let name t =
        match t with
        | Code_address -> "code_address"
        | Capture { index } -> [%string "capture_%{index#Int}"]
      ;;

      let type_ t =
        match t with
        | Code_address | Capture _ -> Types.pointer
      ;;
    end
  end

  include T
  include Struct.Make (T)
end

module Handler = struct
  module T = struct
    type t = { num_ops : int }

    module Field = struct
      type t = Op of { index : int } [@@deriving equal]

      let all { num_ops } = List.init num_ops ~f:(fun index -> Op { index })
      let name (Op { index }) = [%string "op_%{index#Int}"]
      let type_ (Op { index = _ }) = Types.pointer
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

module List = struct
  module Tag = struct
    let type_ = Codegen.use_context Llvm.i8_type

    let const_tag i =
      let open Codegen.Let_syntax in
      let%map type_ = type_ in
      Llvm.const_int type_ i
    ;;

    let const_nil = const_tag 0
    let const_cons = const_tag 1
  end

  module T = struct
    type t = unit

    module Field = struct
      type t =
        | Tag
        | Head
        | Tail
      [@@deriving equal]

      let all () = [ Tag; Head; Tail ]

      let name t =
        match t with
        | Tag -> "list_tag"
        | Head -> "list_head"
        | Tail -> "list_tail"
      ;;

      let type_ t =
        match t with
        | Tag -> Tag.type_
        | Head -> Types.pointer
        | Tail -> Types.pointer
      ;;
    end
  end

  include T
  include Struct.Make (T)
end

module Tuple = struct
  module T = struct
    type t = { num_elements : int }

    module Field = struct
      type t = Element of { index : int } [@@deriving equal]

      let all { num_elements } =
        Core.List.init num_elements ~f:(fun index -> Element { index })
      ;;

      let name (Element { index }) = [%string "element_%{index#Int}"]
      let type_ (Element _) = Types.pointer
    end
  end

  include T
  module Non_empty_tuple = Struct.Make (T)

  let type_ t =
    match t.num_elements with
    | 0 -> Types.pointer
    | _ -> Non_empty_tuple.type_ t
  ;;

  let populate t pointer ~f =
    let open Codegen.Let_syntax in
    match t.num_elements with
    | 0 -> return ()
    | _ -> Non_empty_tuple.populate t pointer ~f
  ;;

  let project ?name t pointer field =
    (* unit has no fields so project can't be called *)
    Non_empty_tuple.project ?name t pointer field
  ;;

  let heap_allocate t ~name ~runtime =
    match t.num_elements with
    | 0 -> Immediate_repr.Unit.const_opaque ()
    | _ -> Non_empty_tuple.heap_allocate t ~name ~runtime
  ;;
end
