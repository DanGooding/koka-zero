open! Core
open! Import

module Bool = struct
  type t = Unpacked of Llvm.llvalue

  let const_false () =
    let open Codegen.Let_syntax in
    let%map bool_type = Types.bool in
    (* all zeros *)
    Unpacked (Llvm.const_null bool_type)
  ;;

  let const_true () =
    let open Codegen.Let_syntax in
    let%map bool_type = Types.bool in
    Unpacked (Llvm.const_int bool_type 1)
  ;;

  let const_bool b = if b then const_true () else const_false ()

  let to_i1 (Unpacked b) =
    let open Codegen.Let_syntax in
    let%bind i1 = Codegen.use_context Llvm.i1_type in
    (* expects only [const_true]/[const_false] *)
    Codegen.use_builder (Llvm.build_trunc b i1 "bool_bit")
  ;;

  let of_i1 b =
    let open Codegen.Let_syntax in
    let%bind bool = Types.bool in
    let%map b = Codegen.use_builder (Llvm.build_zext b bool "bool") in
    Unpacked b
  ;;

  let to_opaque (Unpacked b) =
    let open Codegen.Let_syntax in
    let%bind ptr_type = Types.pointer in
    Codegen.use_builder (Llvm.build_inttoptr b ptr_type "opaque")
  ;;

  let of_opaque opaque =
    let open Codegen.Let_syntax in
    let%bind bool_type = Types.bool in
    let%map b =
      Codegen.use_builder (Llvm.build_ptrtoint opaque bool_type "bool")
    in
    Unpacked b
  ;;

  let compile_binary_operation
        (Unpacked left)
        (op : Operator.Bool.t)
        (Unpacked right)
    =
    let open Codegen.Let_syntax in
    let%bind result =
      (* [and]/[or] work directly on bools, don't need to convert to [i1] and
         back *)
      match op with
      | And -> Codegen.use_builder (Llvm.build_and left right "bool_and")
      | Or -> Codegen.use_builder (Llvm.build_or left right "bool_or")
    in
    to_opaque (Unpacked result)
  ;;

  let compile_unary_operation (op : Operator.Bool.Unary.t) (Unpacked b) =
    let open Codegen.Let_syntax in
    match op with
    | Not ->
      let%bind (Unpacked true_) = const_true () in
      Codegen.use_builder (Llvm.build_xor b true_ "not")
  ;;
end

module Int_common (Arg : sig
    val register_name : string
  end) =
struct
  type t = Unpacked of Llvm.llvalue

  let const i =
    let open Codegen.Let_syntax in
    let%map int_type = Types.int in
    Unpacked (Llvm.const_int int_type i)
  ;;

  let to_opaque (Unpacked i) =
    let open Codegen.Let_syntax in
    let%bind ptr_type = Types.pointer in
    Codegen.use_builder (Llvm.build_inttoptr i ptr_type "opaque")
  ;;

  let of_opaque opaque =
    let open Codegen.Let_syntax in
    let%bind int_type = Types.int in
    let%map i =
      Codegen.use_builder
        (Llvm.build_ptrtoint opaque int_type Arg.register_name)
    in
    Unpacked i
  ;;
end

module Int = struct
  (* holds a value of type [Types.int] *)
  include Int_common (struct
      let register_name = "int"
    end)

  let compile_binary_operation
        (Unpacked left)
        (op : Operator.Int.t)
        (Unpacked right)
    =
    let open Codegen.Let_syntax in
    let operation =
      match op with
      | Plus -> `Int Llvm.build_add
      | Minus -> `Int Llvm.build_sub
      | Times -> `Int Llvm.build_mul
      (* TODO: these both have undefined behaviour for division/modulo by zero!
         would prefer to exit or raise a koka exn *)
      | Divide -> `Int Llvm.build_sdiv
      (* TODO: good behaviour for modulo of a negative *)
      | Modulo -> `Int Llvm.build_srem
      | Equals -> `Bool Llvm.Icmp.Eq
      | Not_equal -> `Bool Llvm.Icmp.Ne
      | Less_than -> `Bool Llvm.Icmp.Slt
      | Less_equal -> `Bool Llvm.Icmp.Sle
      | Greater_equal -> `Bool Llvm.Icmp.Sge
      | Greater_than -> `Bool Llvm.Icmp.Sgt
    in
    match operation with
    | `Int build ->
      let%bind result = Codegen.use_builder (build left right "int_math") in
      to_opaque (Unpacked result)
    | `Bool icmp ->
      let%bind result =
        Codegen.use_builder (Llvm.build_icmp icmp left right "int_cmp")
      in
      let%bind result = Bool.of_i1 result in
      Bool.to_opaque result
  ;;
end

module Unit = struct
  let const_opaque () =
    let open Codegen.Let_syntax in
    let%map ptr_type = Types.pointer in
    Llvm.const_null ptr_type
  ;;
end

module Marker = struct
  (* holds a value of type [Types.marker] *)
  include Int_common (struct
      let register_name = "marker"
    end)

  let compile_equal (Unpacked left) (Unpacked right) =
    let open Codegen.Let_syntax in
    let%bind eq_i1 =
      Codegen.use_builder
        (Llvm.build_icmp Llvm.Icmp.Eq left right "markers_equal")
    in
    Bool.of_i1 eq_i1
  ;;
end

module Label = struct
  (* holds a value of type [Types.label] *)
  include Int_common (struct
      let register_name = "label"
    end)
end
