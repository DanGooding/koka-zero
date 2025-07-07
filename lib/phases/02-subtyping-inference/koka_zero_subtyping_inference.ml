open! Core
open! Import

let infer_program = Infer.infer_program (Infer.create ())

module Private = struct
  let infer_expr_toplevel = Infer.infer_expr_toplevel (Infer.create ())

  let infer_program_without_main =
    Infer.infer_program_without_main (Infer.create ())
  ;;
end
