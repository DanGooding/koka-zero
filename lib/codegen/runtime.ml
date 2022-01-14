(** builds declaration of malloc: [i8* kkr_malloc(i64)] *)
let declare_malloc =
  let open Codegen.Let_syntax in
  let malloc_name = "kkr_malloc" in
  let%bind i64 = Codegen.use_context Llvm.i64_type in
  let%bind misc_pointer_type = Types.opaque_pointer in
  let malloc_type =
    Llvm.function_type misc_pointer_type (ArrayLabels.of_list [ i64 ])
  in
  Codegen.use_module (Llvm.declare_function malloc_name malloc_type)
;;

type t = { malloc : Llvm.llvalue }

let declare =
  let open Codegen.Let_syntax in
  let%map malloc = declare_malloc in
  { malloc }
;;
