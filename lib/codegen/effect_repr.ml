open Import

type t =
  { id : int
  ; hnd_type : Llvm.lltype
  ; operation_indices : int Variable.Map.t
  }
