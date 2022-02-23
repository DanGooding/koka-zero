module T = struct
  type t =
    | Control
    | Fun
  [@@deriving sexp]
end (* disable "fragile-match" for generated code *) [@warning "-4"]

include T

let can_implement ~handler ~declaration =
  match handler, declaration with
  (* interesting cases: *)
  | Fun, Control -> true
  (* reflexive: *)
  | Fun, Fun -> true
  | Control, Control -> true
  (* exhaustive false cases: *)
  | Control, Fun -> false
;;
