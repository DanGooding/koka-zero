open Import

type t =
  { id : int
  ; hnd_type : Llvm.lltype
  ; operations : Variable.t list
  }
