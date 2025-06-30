open! Core
open! Import

module Packed = struct
  type t = Llvm.llvalue
end

module Unpacked = struct
  module Function = struct
    type t =
      | Code_pointer of (Llvm.llvalue[@sexp.opaque])
      | Closure of (Llvm.llvalue[@sexp.opaque])
    [@@deriving sexp_of]

    (* this relies on the alignment of functions being greater than one byte *)
    let tag () =
      let open Codegen.Let_syntax in
      let%map i64 = Codegen.use_context Llvm.i64_type in
      Llvm.const_int i64 1
    ;;

    let unpack packed ~f ~compile_conditional =
      let open Codegen.Let_syntax in
      let%bind i64 = Codegen.use_context Llvm.i64_type in
      let%bind pointer_type = Types.pointer in
      let%bind tag = tag () in
      let%bind ptr_value =
        Codegen.use_builder (Llvm.build_ptrtoint packed i64 "ptr_value")
      in
      let%bind tag_bit =
        Codegen.use_builder (Llvm.build_and ptr_value tag "tag_bit")
      in
      let%bind tag_is_set =
        Codegen.use_builder (Llvm.build_icmp Eq tag_bit tag "tag_is_set")
      in
      compile_conditional
        ~cond_i1:tag_is_set
        ~compile_true:(fun () ->
          let%bind code_pointer =
            Codegen.use_builder
              (Llvm.build_xor ptr_value tag "code_pointer_value")
          in
          let%bind code_pointer =
            Codegen.use_builder
              (Llvm.build_inttoptr code_pointer pointer_type "code_pointer")
          in
          f (Code_pointer code_pointer))
        ~compile_false:(fun () -> f (Closure packed))
    ;;

    let pack t =
      let open Codegen.Let_syntax in
      match t with
      | Closure closure -> return closure
      | Code_pointer code_address ->
        let%bind i64 = Codegen.use_context Llvm.i64_type in
        let%bind pointer_type = Types.pointer in
        let%bind tag = tag () in
        let%bind ptr_value =
          Codegen.use_builder (Llvm.build_ptrtoint code_address i64 "ptr_value")
        in
        let%bind tagged_value =
          Codegen.use_builder (Llvm.build_or ptr_value tag "tagged_value")
        in
        Codegen.use_builder
          (Llvm.build_inttoptr tagged_value pointer_type "tagged")
    ;;
  end
end

module Lazily_packed = struct
  type t =
    | Function of Unpacked.Function.t
    | Packed of (Packed.t[@sexp.opaque])
  [@@deriving sexp_of]

  let pack t =
    match t with
    | Function f -> Unpacked.Function.pack f
    | Packed t -> Codegen.return t
  ;;

  let unpack_function t ~f ~compile_conditional =
    match t with
    | Function unpacked_function -> f unpacked_function
    | Packed packed -> Unpacked.Function.unpack packed ~f ~compile_conditional
  ;;

  let phi_builder t_incoming =
    let open Codegen.Let_syntax in
    let code_pointer_incoming, closure_incoming, packed_incoming =
      List.partition3_map t_incoming ~f:(fun (t, block) ->
        match t with
        | Function (Code_pointer code_pointer) -> `Fst (code_pointer, block)
        | Function (Closure closure) -> `Snd (closure, block)
        | Packed packed -> `Trd (packed, block))
    in
    match code_pointer_incoming, closure_incoming, packed_incoming with
    | _ :: _, [], [] ->
      let%map code_pointer =
        Control_flow.Phi_builder.llvalue code_pointer_incoming
      in
      Function (Code_pointer code_pointer)
    | [], _ :: _, [] ->
      let%map closure = Control_flow.Phi_builder.llvalue closure_incoming in
      Function (Closure closure)
    | _ ->
      (* need to pack all of them *)
      let%bind packed_incoming =
        List.map t_incoming ~f:(fun (t, block) ->
          let%map packed = pack t in
          packed, block)
        |> Codegen.all
      in
      let%map packed = Control_flow.Phi_builder.llvalue packed_incoming in
      Packed packed
  ;;
end
