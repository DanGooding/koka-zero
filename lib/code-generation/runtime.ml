module Function = struct
  type t =
    { function_ : Llvm.llvalue (** callable function *)
    ; type_ : Llvm.lltype
    }
  
  let build_call { type_; function_ } ~args name = 
    Codegen.use_builder (Llvm.build_call type_ function_ args name)
end

type t =
  { init : Function.t
  ; exit : Function.t
  ; exit_with_message : Function.t
  ; malloc : Function.t
  ; fresh_marker : Function.t
  ; markers_equal : Function.t
  ; nil_evidence_vector : Function.t
  ; cons_evidence_vector : Function.t
  ; evidence_vector_lookup : Function.t
  ; get_evidence_marker : Function.t
  ; get_evidence_handler : Function.t
  ; get_evidence_handler_site_vector : Function.t
  ; println : Function.t
  ; print_int : Function.t
  ; read_int : Function.t
  }

let declare_function symbol return_type argument_types =
  let open Codegen.Let_syntax in
  let name = Symbol_name.to_string symbol in
  let type_ = Llvm.function_type return_type (Array.of_list argument_types) in
  let%map function_ = Codegen.use_module (Llvm.declare_function name type_) in
  Llvm.set_function_call_conv Llvm.CallConv.c function_;
  { Function.function_; type_ }
;;

let declare =
  let open Codegen.Let_syntax in
  let%bind void_type = Codegen.use_context Llvm.void_type in
  let%bind i8 = Codegen.use_context Llvm.i8_type in
  let%bind ptr = Types.pointer in
  let%bind i64 = Codegen.use_context Llvm.i64_type in
  let%bind pointer_type = Types.pointer in
  let%bind bool_type = Types.bool in
  let%bind int_type = Types.int in
  let%bind marker_type = Types.marker in
  let%bind label_type = Types.label in
  let%bind init =
    let name = Symbol_name.of_runtime_exn "kkr_init" in
    declare_function name void_type []
  in
  let%bind exit =
    let name = Symbol_name.of_runtime_exn "kkr_exit" in
    declare_function name void_type []
  in
  let%bind exit_with_message =
    let name = Symbol_name.of_runtime_exn "kkr_exit_with_message" in
    declare_function name void_type [ ptr ]
  in
  let%bind malloc =
    let name = Symbol_name.of_runtime_exn "kkr_malloc" in
    declare_function name pointer_type [ i64 ]
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
    declare_function name pointer_type []
  in
  let%bind cons_evidence_vector =
    let name = Symbol_name.of_runtime_exn "kkr_cons_evidence_vector" in
    declare_function
      name
      pointer_type
      [ label_type
      ; marker_type
      ; pointer_type (* handler *)
      ; pointer_type (* handler site vector *)
      ; pointer_type (* vector tail *)
      ]
  in
  let%bind evidence_vector_lookup =
    let name = Symbol_name.of_runtime_exn "kkr_evidence_vector_lookup" in
    declare_function
      name
      pointer_type
      [ pointer_type; label_type ]
  in
  let%bind get_evidence_marker =
    let name = Symbol_name.of_runtime_exn "kkr_get_evidence_marker" in
    declare_function name marker_type [ pointer_type ]
  in
  let%bind get_evidence_handler =
    let name = Symbol_name.of_runtime_exn "kkr_get_evidence_handler" in
    declare_function name pointer_type [ pointer_type ]
  in
  let%bind get_evidence_handler_site_vector =
    let name =
      Symbol_name.of_runtime_exn "kkr_get_evidence_handler_site_vector"
    in
    declare_function name pointer_type [ pointer_type ]
  in
  let%bind println =
    let name = Symbol_name.of_runtime_exn "kkr_println" in
    declare_function name void_type []
  in
  let%bind print_int =
    let name = Symbol_name.of_runtime_exn "kkr_print_int" in
    declare_function name void_type [ int_type (* value *); i8 (* newline? *) ]
  in
  let%map read_int =
    let name = Symbol_name.of_runtime_exn "kkr_read_int" in
    declare_function name int_type []
  in
  { init
  ; exit
  ; exit_with_message
  ; malloc
  ; fresh_marker
  ; markers_equal
  ; nil_evidence_vector
  ; cons_evidence_vector
  ; evidence_vector_lookup
  ; get_evidence_marker
  ; get_evidence_handler
  ; get_evidence_handler_site_vector
  ; println
  ; print_int
  ; read_int
  }
;;
