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
    let name = "kkr_exit" in
    let type_ = Llvm.function_type void_type (Array.of_list []) in
    Codegen.use_module (Llvm.declare_function name type_)
  in
  let%bind exit_with_message =
    let name = "kkr_exit_with_message" in
    let type_ = Llvm.function_type void_type (Array.of_list [ i8_ptr ]) in
    Codegen.use_module (Llvm.declare_function name type_)
  in
  let%bind malloc =
    let name = "kkr_malloc" in
    let type_ =
      Llvm.function_type opaque_pointer_type (Array.of_list [ i64 ])
    in
    Codegen.use_module (Llvm.declare_function name type_)
  in
  let%bind fresh_marker =
    let name = "kkr_fresh_marker" in
    let type_ = Llvm.function_type marker_type (Array.of_list []) in
    Codegen.use_module (Llvm.declare_function name type_)
  in
  let%bind markers_equal =
    let name = "kkr_markers_equal" in
    let type_ =
      Llvm.function_type bool_type (Array.of_list [ marker_type; marker_type ])
    in
    Codegen.use_module (Llvm.declare_function name type_)
  in
  let%bind nil_evidence_vector =
    let name = "kkr_nil_evidence_vector" in
    let type_ = Llvm.function_type opaque_pointer_type (Array.of_list []) in
    Codegen.use_module (Llvm.declare_function name type_)
  in
  let%bind cons_evidence_vector =
    let name = "kkr_cons_evidence_vector" in
    let type_ =
      Llvm.function_type
        opaque_pointer_type
        (Array.of_list
           [ label_type; marker_type; opaque_pointer_type; opaque_pointer_type ])
    in
    Codegen.use_module (Llvm.declare_function name type_)
  in
  let%bind evidence_vector_lookup =
    let name = "kkr_evidence_vector_lookup" in
    let type_ =
      Llvm.function_type
        opaque_pointer_type
        (Array.of_list [ opaque_pointer_type; label_type ])
    in
    Codegen.use_module (Llvm.declare_function name type_)
  in
  let%bind get_evidence_marker =
    let name = "kkr_get_evidence_marker" in
    let type_ =
      Llvm.function_type marker_type (Array.of_list [ opaque_pointer_type ])
    in
    Codegen.use_module (Llvm.declare_function name type_)
  in
  let%bind get_evidence_handler =
    let name = "kkr_get_evidence_handler" in
    let type_ =
      Llvm.function_type
        opaque_pointer_type
        (Array.of_list [ opaque_pointer_type ])
    in
    Codegen.use_module (Llvm.declare_function name type_)
  in
  let%bind print_int =
    let name = "kkr_print_int" in
    let type_ = Llvm.function_type void_type (Array.of_list [ int_type ]) in
    Codegen.use_module (Llvm.declare_function name type_)
  in
  let%map read_int =
    let name = "kkr_read_int" in
    let type_ = Llvm.function_type int_type (Array.of_list []) in
    Codegen.use_module (Llvm.declare_function name type_)
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
