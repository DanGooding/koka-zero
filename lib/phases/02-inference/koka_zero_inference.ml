open! Core
open! Import

let infer_program = Infer.infer_program

module Private = struct
  let infer_expr_toplevel = Infer.infer_expr_toplevel
  let infer_program_without_main = Infer.infer_program_without_main
end
