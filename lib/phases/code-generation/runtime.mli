module Function : sig
  type t =
    { function_ : Llvm.llvalue
    ; type_ : Llvm.lltype
    }

  val build_call
    :  t
    -> args:Llvm.llvalue array
    -> string
    -> Llvm.llvalue Codegen.t
end

(** declarations of runtime's functions *)
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

(** build declarations for all functions *)
val declare : t Codegen.t
