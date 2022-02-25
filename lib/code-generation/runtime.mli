(** declarations of runtime's functions *)
type t =
  { init : Llvm.llvalue
  ; exit : Llvm.llvalue
  ; exit_with_message : Llvm.llvalue
  ; malloc : Llvm.llvalue
  ; fresh_marker : Llvm.llvalue
  ; markers_equal : Llvm.llvalue
  ; nil_evidence_vector : Llvm.llvalue
  ; cons_evidence_vector : Llvm.llvalue
  ; evidence_vector_lookup : Llvm.llvalue
  ; get_evidence_marker : Llvm.llvalue
  ; get_evidence_handler : Llvm.llvalue
  ; get_evidence_handler_site_vector : Llvm.llvalue
  ; print_int : Llvm.llvalue
  ; read_int : Llvm.llvalue
  }

(** build declarations for all functions *)
val declare : t Codegen.t
