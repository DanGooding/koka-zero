open! Core
open! Import

let rewrite_program (program : Evidence_passing_syntax.Program.t) =
  let open Generation.Let_syntax in
  (* bind inlining need only be applied once. *)
  let%map program = Bind_inlining.rewrite_program program in
  (* other rewrites are applied repeatedly until it stops changing *)
  let rewrites = [ Monad_identify.left_unit ] in
  Modified.apply_while_changes
    ~f:(fun program ->
      Modified.list_fold rewrites ~init:program ~f:(fun program rewrite ->
        Rewriting_utils.apply_everywhere_to_program program ~rewrite))
    program
  |> Modified.value
;;
