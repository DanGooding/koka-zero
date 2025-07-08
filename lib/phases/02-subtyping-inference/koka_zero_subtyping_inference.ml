open! Core
open! Import

let infer_program program = Infer.infer_program (Infer.create ()) program

module Private = struct
  module Polar_type = Polar_type

  let infer_expr_toplevel expr ~declarations =
    Infer.infer_expr_toplevel (Infer.create ()) expr ~declarations
  ;;

  let infer_program_without_main program =
    Infer.infer_program_without_main (Infer.create ()) program
  ;;
end
