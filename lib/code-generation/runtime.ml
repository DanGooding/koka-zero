type t =
  { exit : Llvm.llvalue
  ; exit_with_message : Llvm.llvalue
  ; malloc : Llvm.llvalue
  ; fresh_marker : Llvm.llvalue
  ; markers_equal : Llvm.llvalue
  ; nil_evidence_vector : Llvm.llvalue
  ; cons_evidence_vector : Llvm.llvalue
  ; evidence_vector_lookup : Llvm.llvalue
  ; get_evidence_marker : Llvm.llvalue
  ; get_evidence_handler : Llvm.llvalue
  ; print_int : Llvm.llvalue
  ; read_int : Llvm.llvalue
  }

let declare_function symbol return_type argument_types =
  let open Codegen.Let_syntax in
  let name = Symbol_name.to_string symbol in
  let type_ = Llvm.function_type return_type (Array.of_list argument_types) in
  let%map function_ = Codegen.use_module (Llvm.declare_function name type_) in
  Llvm.set_function_call_conv Llvm.CallConv.c function_;
  function_
;;

let declare =
  let open Codegen.Let_syntax in
  let%bind void_type = Codegen.use_context Llvm.void_type in
  let%bind i8 = Codegen.use_context Llvm.i8_type in
  let i8_ptr = Llvm.pointer_type i8 in
  let%bind i64 = Codegen.use_context Llvm.i64_type in
  let%bind opaque_pointer_type = Types.opaque_pointer in
  let%bind bool_type = Types.bool in
  let%bind int_type = Types.int in
  let%bind marker_type = Types.marker in
  let%bind label_type = Types.label in
  let%bind exit =
    let name = Symbol_name.of_runtime_exn "kkr_exit" in
    declare_function name void_type []
  in
  let%bind exit_with_message =
    let name = Symbol_name.of_runtime_exn "kkr_exit_with_message" in
    declare_function name void_type [ i8_ptr ]
  in
  let%bind malloc =
    let name = Symbol_name.of_runtime_exn "kkr_malloc" in
    declare_function name opaque_pointer_type [ i64 ]
  in
  let%bind fresh_marker =
    let name = Symbol_name.of_runtime_exn "kkr_fresh_marker" in
    declare_function name marker_type []
  in
  let%bind markers_equal =
    let name = Symbol_name.of_runtime_exn "kkr_markers_equal" in
    declare_function name bool_type [ marker_type; marker_type ]
  in
  let%bind nil_evidence_vector =
    let name = Symbol_name.of_runtime_exn "kkr_nil_evidence_vector" in
    declare_function name opaque_pointer_type []
  in
  let%bind cons_evidence_vector =
    let name = Symbol_name.of_runtime_exn "kkr_cons_evidence_vector" in
    declare_function
      name
      opaque_pointer_type
      [ label_type; marker_type; opaque_pointer_type; opaque_pointer_type ]
  in
  let%bind evidence_vector_lookup =
    let name = Symbol_name.of_runtime_exn "kkr_evidence_vector_lookup" in
    declare_function
      name
      opaque_pointer_type
      [ opaque_pointer_type; label_type ]
  in
  let%bind get_evidence_marker =
    let name = Symbol_name.of_runtime_exn "kkr_get_evidence_marker" in
    declare_function name marker_type [ opaque_pointer_type ]
  in
  let%bind get_evidence_handler =
    let name = Symbol_name.of_runtime_exn "kkr_get_evidence_handler" in
    declare_function name opaque_pointer_type [ opaque_pointer_type ]
  in
  let%bind print_int =
    let name = Symbol_name.of_runtime_exn "kkr_print_int" in
    declare_function name void_type [ int_type ]
  in
  let%map read_int =
    let name = Symbol_name.of_runtime_exn "kkr_read_int" in
    declare_function name int_type []
  in
  { exit
  ; exit_with_message
  ; malloc
  ; fresh_marker
  ; markers_equal
  ; nil_evidence_vector
  ; cons_evidence_vector
  ; evidence_vector_lookup
  ; get_evidence_marker
  ; get_evidence_handler
  ; print_int
  ; read_int
  }
;;
