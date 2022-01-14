(** pointer to an arbitrary runtime value *)
let opaque_pointer : Llvm.lltype Codegen.t =
  let open Codegen.Let_syntax in
  (* byte pointer *)
  let%map i8 = Codegen.use_context Llvm.i8_type in
  Llvm.pointer_type i8
;;

let bool : Llvm.lltype Codegen.t =
  (* could use i1 (single bit) but can't allocate that little, nor can an i8*
     point to it *)
  Codegen.use_context Llvm.i8_type
;;

let int : Llvm.lltype Codegen.t = Codegen.use_context Llvm.i64_type
let unit : Llvm.lltype Codegen.t = Codegen.use_context Llvm.i8_type
