open Core
open Koka_zero_util

(* Names: *)

module Var_id : Identifiable.S = String
module Wildcard_id : Identifiable.S = String

module Identifier = struct
  module T = struct
    type t = Var of Var_id.t
    (* | Wildcard of Wildcard_id.t *)
    (* | Operator of Operator_id.t *)
    (* | Constructor of Constructor_id.t *)
    [@@deriving compare, sexp]
  end (* disable "fragile-match" for generated code *) [@warning "-4"]

  include T
  include Comparable.Make (T)
end

let resume_keyword = Identifier.Var (Var_id.of_string "resume")

(* Kinds: *)

type kind_atom =
  | Kind_effect_type
  | Kind_effect_row
  | Kind_value
[@@deriving sexp]

type kind =
  | Kind_arrow of kind list * kind
  | Kind_atom of kind_atom
[@@deriving sexp]

(* types and effects: *)

type type_parameter =
  { id : Var_id.t
  ; kind : kind option
  }
[@@deriving sexp]

(* note: typechecker will remove the nonsensical cases based on kind checking

   the parser does nothing in this regard, but here is little it can do, since
   `a` can always be a type variable or an effect variable, and `eff` can always
   be a type or an effect *)

type type_ =
  | Arrow of type_ * type_result
  | Effect_row of effect_row
  | Scheme of type_scheme
  | Type_atom of
      { constructor : type_constructor
      ; arguments : type_ list
      }
  | Annotated_type of
      { type_ : type_
      ; kind : kind
      }
  | Parameters_or_tuple of parameter_type list
[@@deriving sexp]

and type_scheme =
  { forall_quantified : type_parameter list
  ; body : type_
  }
[@@deriving sexp]

and type_constructor =
  | Variable_or_name of Var_id.t
  | Type_wildcard of Wildcard_id.t
  (* builtin types *)
  | Type_int
  | Type_bool
[@@deriving sexp]

and parameter_type =
  { parameter_id : Identifier.t option
  ; type_ : type_
  }
[@@deriving sexp]

and type_result =
  { effect : type_
  ; result : type_
  }
[@@deriving sexp]

and effect_row =
  | Closed of type_ list
  | Open of type_ Non_empty_list.t * type_
[@@deriving sexp]

let total_effect_row : effect_row = Closed []

(* Parameters and binders: *)

type parameter_id =
  | Parameter_id of Identifier.t
  | Parameter_wildcard
[@@deriving sexp]

type parameter =
  { id : parameter_id
  ; type_ : type_
  }
[@@deriving sexp]

type pattern =
  | Pattern_id of Identifier.t
  | Pattern_wildcard
[@@deriving sexp]

type annotated_pattern =
  { pattern : pattern
  ; scheme : type_scheme option
  }
[@@deriving sexp]

type pattern_parameter =
  { pattern : pattern
  ; type_ : type_ option
  }
[@@deriving sexp]

type binder =
  { id : Identifier.t
  ; type_ : type_ option
  }
[@@deriving sexp]

let pattern_parameter_of_binder : binder -> pattern_parameter =
 fun binder ->
  let ({ id; type_ } : binder) = binder in
  { pattern = Pattern_id id; type_ }
;;

(* operations: *)

(* TODO: better name *)
type operation_shape =
  (* TODO: name fields? *)
  | Shape_val of type_
  | Shape_fun of parameter list * type_
  | Shape_except of parameter list * type_
  | Shape_control of parameter list * type_
[@@deriving sexp]

type operation_declaration =
  { id : Var_id.t
  ; type_parameters : type_parameter list
  ; shape : operation_shape
  }
[@@deriving sexp]

type effect_declaration =
  { id : Var_id.t
  ; type_parameters : type_parameter list
  ; kind : kind option
  ; operations : operation_declaration list
  }
[@@deriving sexp]

type type_declaration =
  (* TODO: records/Effect.t *)
  | Effect_declaration of effect_declaration
(* | Type *)
[@@deriving sexp]

type operation_parameter =
  { id : parameter_id
  ; type_ : type_ option
  }
[@@deriving sexp]

(* expressions: *)

type literal =
  | Unit
  | Int of int
  | Bool of bool
[@@deriving sexp]

type unary_operator = Exclamation [@@deriving sexp]

type binary_operator =
  | Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | And
  | Or
  | Equals
  | Not_equal
  | Less_than
  | Less_equal
  | Greater_than
  | Greater_equal
[@@deriving sexp]

type expr =
  | Return of expr
  | If_then_else of expr * block * block
  | If_then of expr * block
  (* | Match *)
  (* TODO: which of [handle]/[handler] is the logical primitive? *)
  | Handler of effect_handler
  | Handle of
      { subject : expr
      ; handler : effect_handler
      }
  | Fn of fn
  | Binary_op of expr * binary_operator * expr
  | Unary_op of unary_operator * expr
  | Application of expr * expr list
  | Identifier of Identifier.t
  | Literal of literal
  (* | Tuple of expr list *)
  (* | List of expr list *)
  | Annotated_expr of expr * type_scheme
[@@deriving sexp]

and statement =
  | Declaration of declaration
  (* note this may be a return expression! *)
  | Expr of expr
[@@deriving sexp]

and block =
  { statements : statement list
  ; last : expr
  }
[@@deriving sexp]

and fn =
  { type_parameters : type_parameter list
  ; parameters : pattern_parameter list
  ; result_type : type_result option
  ; body : block
  }
[@@deriving sexp]

and declaration =
  | Fun of fun_declaration
  | Val of annotated_pattern * block
[@@deriving sexp]

and fun_declaration =
  { id : Identifier.t
  ; fn : fn
  }
[@@deriving sexp]

and operation_handler =
  | Op_val of
      { id : Var_id.t
      ; type_ : type_ option
      ; value : block
      }
  (* TODO: sharing between different shapes? *)
  | Op_fun of
      { id : Var_id.t
      ; parameters : operation_parameter list
      ; body : block
      }
  | Op_except of
      { id : Var_id.t
      ; parameters : operation_parameter list
      ; body : block
      }
  | Op_control of
      { id : Var_id.t
      ; parameters : operation_parameter list
      ; body : block
      }
  | Op_return of
      { parameter : operation_parameter
      ; body : block
      }
[@@deriving sexp]

and effect_handler = Effect_handler of operation_handler list
[@@deriving sexp]

type pure_declaration =
  | Top_val of binder * block
  | Top_fun of fun_declaration
[@@deriving sexp]

type toplevel_declaration =
  | Pure_declaration of pure_declaration
  | Type_declaration of type_declaration
[@@deriving sexp]

type program = Program of toplevel_declaration list [@@deriving sexp]

let singleton_block last = { statements = []; last }

let block_cons s block =
  let { statements; last } = block in
  let statements = s :: statements in
  { statements; last }
;;

let anonymous_of_block : block -> fn =
 fun body -> { type_parameters = []; parameters = []; result_type = None; body }
;;

let anonymous_of_bound_block : binder:binder -> block:block -> fn =
 fun ~binder ~block ->
  let parameter = pattern_parameter_of_binder binder in
  { type_parameters = []
  ; parameters = [ parameter ]
  ; result_type = None
  ; body = block
  }
;;

let insert_with_callback : callback:fn -> expr -> expr =
 fun ~callback e ->
  match e with
  | Application (f, args) -> Application (f, args @ [ Fn callback ])
  (* note: technically, [Annotated(e, scheme)] should be possible if [e] is an
     application, but real koka also forbids this. (There may be some
     difficulies with capture of type variables) *)
  | Annotated_expr (_, _)
  | Return _
  | If_then_else (_, _, _)
  | If_then (_, _)
  | Handler _ | Handle _ | Fn _
  | Binary_op (_, _, _)
  | Unary_op (_, _)
  | Identifier _ | Literal _ -> Application (e, [ Fn callback ])
;;
