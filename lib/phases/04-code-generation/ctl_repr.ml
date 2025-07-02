open! Core
open! Import

module Maybe_yield_repr = struct
  type t =
    { is_yield_i1 : Llvm.llvalue
    ; content : Llvm.llvalue
    }

  let create ~is_yield_i1 ~content = { is_yield_i1; content }
  let get_is_yield_i1 t = Codegen.return t.is_yield_i1
  let get_content t = Codegen.return t.content

  let compile_construct_pure =
    fun content ->
    let open Codegen.Let_syntax in
    let%map i1_type = Codegen.use_context Llvm.i1_type in
    let is_yield_i1 = Llvm.const_int i1_type 0 in
    { is_yield_i1; content }
  ;;

  let compile_construct_yield
        ~(marker : Immediate_repr.Marker.t)
        ~(op_clause : Llvm.llvalue)
        ~(resumption : Llvm.llvalue)
        ~(runtime : Runtime.t)
    : t Codegen.t
    =
    let open Codegen.Let_syntax in
    let%bind ctl_yield_ptr =
      Structs.Ctl_yield.heap_allocate ~name:"ctl_yield" ~runtime
    in
    let (Unpacked marker) = marker in
    let%bind () =
      Structs.Ctl_yield.populate ctl_yield_ptr ~f:(function
        | Marker -> marker
        | Op_clause -> op_clause
        | Resumption -> resumption)
    in
    let%map i1_type = Codegen.use_context Llvm.i1_type in
    let is_yield_i1 = Llvm.const_int i1_type 1 in
    { is_yield_i1; content = ctl_yield_ptr }
  ;;
end

type t =
  | Pure of Value_repr.Lazily_packed.t
  | Ctl of Maybe_yield_repr.t

let pure t =
  match t with
  | Pure pure -> Codegen.return pure
  | Ctl _ -> Codegen.impossible_error "expected Pure llvalue, got Ctl"
;;

let pure_exn t =
  match t with
  | Pure pure -> pure
  | Ctl _ -> raise_s [%message "expected Pure but got Ctl"]
;;

let ctl t =
  match t with
  | Ctl is_yield -> Codegen.return is_yield
  | Pure _ -> Codegen.impossible_error "expected Ctl llvalue, got Pure"
;;

let type_ t : Evidence_passing_syntax.Type.t =
  match t with
  | Pure _ -> Pure
  | Ctl _ -> Ctl
;;

let phi_builder incoming =
  let open Codegen.Let_syntax in
  let pure, ctl =
    List.partition_map incoming ~f:(fun (t, block) ->
      match t with
      | Pure pure -> First (pure, block)
      | Ctl ctl -> Second (ctl, block))
  in
  match pure, ctl with
  | _ :: _, _ :: _ ->
    let types = List.map incoming ~f:(fun (value, _block) -> type_ value) in
    raise_s
      [%message
        "cannot build phi node to combine mixture of Ctl and Pure values"
          (types : Evidence_passing_syntax.Type.t list)]
  | pure, [] ->
    let%map pure = Value_repr.Lazily_packed.phi_builder pure in
    Pure pure
  | [], ctl ->
    let content =
      List.map ctl ~f:(fun ({ Maybe_yield_repr.content; _ }, block) ->
        content, block)
    in
    let is_yield_i1 =
      List.map ctl ~f:(fun ({ Maybe_yield_repr.is_yield_i1; _ }, block) ->
        is_yield_i1, block)
    in
    let%bind content =
      Codegen.use_builder (Llvm.build_phi content "content_incoming")
    in
    let%map is_yield_i1 =
      Codegen.use_builder (Llvm.build_phi is_yield_i1 "is_yield_incoming")
    in
    Ctl { Maybe_yield_repr.content; is_yield_i1 }
;;
