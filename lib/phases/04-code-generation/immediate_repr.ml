open! Core
open! Import

module Bool = struct
  type t = Bool of Llvm.llvalue

  let of_bool_llvalue b = Bool b

  let const_false () =
    let open Codegen.Let_syntax in
    let%map bool_type = Types.bool in
    (* all zeros *)
    Bool (Llvm.const_null bool_type)
  ;;

  let const_true () =
    let open Codegen.Let_syntax in
    let%map bool_type = Types.bool in
    Bool (Llvm.const_int bool_type 1)
  ;;

  let const_bool b = if b then const_true () else const_false ()

  let to_i1 (Bool b) =
    let open Codegen.Let_syntax in
    let%bind i1 = Codegen.use_context Llvm.i1_type in
    (* expects only [const_true]/[const_false] *)
    Codegen.use_builder (Llvm.build_trunc b i1 "bool_bit")
  ;;

  let of_i1 b =
    let open Codegen.Let_syntax in
    let%bind bool = Types.bool in
    let%map b = Codegen.use_builder (Llvm.build_zext b bool "bool") in
    Bool b
  ;;

  let to_opaque (Bool b) =
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
    Bool b
  ;;

  let compile_binary_operation (Bool left) (op : Operator.Bool.t) (Bool right) =
    let open Codegen.Let_syntax in
    let%bind result =
      (* [and]/[or] work directly on bools, don't need to convert to [i1] and
         back *)
      match op with
      | And -> Codegen.use_builder (Llvm.build_and left right "bool_and")
      | Or -> Codegen.use_builder (Llvm.build_or left right "bool_or")
    in
    to_opaque (Bool result)
  ;;

  let compile_unary_operation (op : Operator.Bool.Unary.t) (Bool b) =
    let open Codegen.Let_syntax in
    match op with
    | Not ->
      let%bind (Bool true_) = const_true () in
      Codegen.use_builder (Llvm.build_xor b true_ "not")
  ;;
end

module Int = struct
  (* holds a value of type [Types.int] *)
  type t = Int of Llvm.llvalue

  let const i =
    let open Codegen.Let_syntax in
    let%map int_type = Types.int in
    Int (Llvm.const_int int_type i)
  ;;

  let of_int_llvalue t = Int t
  let to_int_llvalue (Int t) = t

  let to_opaque (Int i) =
    let open Codegen.Let_syntax in
    let%bind ptr_type = Types.pointer in
    Codegen.use_builder (Llvm.build_inttoptr i ptr_type "opaque")
  ;;

  let of_opaque opaque =
    let open Codegen.Let_syntax in
    let%bind int_type = Types.int in
    let%map i =
      Codegen.use_builder (Llvm.build_ptrtoint opaque int_type "int")
    in
    Int i
  ;;

  let compile_binary_operation (Int left) (op : Operator.Int.t) (Int right) =
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
      to_opaque (Int result)
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
  type t = Marker of Llvm.llvalue

  let of_marker_llvalue marker = Marker marker
  let to_marker_llvalue (Marker marker) = marker

  let to_opaque (Marker m) =
    let open Codegen.Let_syntax in
    let%bind ptr_type = Types.pointer in
    Codegen.use_builder (Llvm.build_inttoptr m ptr_type "opaque")
  ;;

  let of_opaque opaque =
    let open Codegen.Let_syntax in
    let%bind marker_type = Types.marker in
    let%map m =
      Codegen.use_builder (Llvm.build_ptrtoint opaque marker_type "marker")
    in
    Marker m
  ;;

  let compile_equal (Marker left) (Marker right) =
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
  type t = Label of Llvm.llvalue

  let of_const_int i =
    let open Codegen.Let_syntax in
    let%map label = Types.label in
    Label (Llvm.const_int label i)
  ;;

  let to_label_llvalue (Label label) = label

  let to_opaque (Label label) =
    let open Codegen.Let_syntax in
    let%bind ptr_type = Types.pointer in
    Codegen.use_builder (Llvm.build_inttoptr label ptr_type "opaque")
  ;;

  let of_opaque opaque =
    let open Codegen.Let_syntax in
    let%bind marker_type = Types.marker in
    let%map label =
      Codegen.use_builder (Llvm.build_ptrtoint opaque marker_type "label")
    in
    Label label
  ;;
end
