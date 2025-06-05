open! Core
open! Import

let rewrite_program program =
  Generation.run (Rewriting.rewrite_program program) ~name_prefix:"opt_"
;;

module Private = struct
  let apply_bind_inlining expr ~toplevel =
    Generation.run (Bind_inlining.rewrite expr ~toplevel) ~name_prefix:"opt_"
  ;;
end
