open! Core
open! Import

module Scrutinee = struct
  type t =
    | Primitive of Type.Primitive.t
    | List
  [@@deriving equal, sexp_of]
end

type t =
  | Parameter of Parameter.t
  | Literal of Literal.t
  | Construction of Constructor.t * Parameter.t list
[@@deriving sexp_of]

let scrutinee t : Scrutinee.t option =
  match t with
  | Parameter _ -> None
  | Literal (Int _) -> Some (Primitive Int)
  | Literal (Bool _) -> Some (Primitive Bool)
  | Literal Unit -> Some (Primitive Unit)
  | Construction ((List_nil | List_cons), _) -> Some List
;;

let bound_variables t =
  match t with
  | Parameter p -> Parameter.bound_variables p
  | Literal _ -> Variable.Set.empty
  | Construction (_, ps) ->
    List.map ps ~f:Parameter.bound_variables |> Variable.Set.union_list
;;
