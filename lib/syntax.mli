(* TODO: rewrite to use modules? *)
(* XXX: desugaring now *)
(* TODO: ** doc comments *)
(* TODO: deriving sexp *)
(* TODO: Identifiable for names *)

(* TODO: don't copy grammar too closely *)
(* TODO: but make sure to desscribe the syntax only (don't throw out
   badly typed terms)*)

open Core

module Var_id : Identifiable
module Operator_id : Identifiable
module Constructor_id : Identifiable

module Identifier : sig
  module T : sig
    type t =
      | Var of Var_id.t
      (* | Operator of Operator_id.t *)
      (* | Constructor of Constructor_id.t *)
      [@@deriving compare, hash, sexp]
    (* TODO: bin_io too? *)

    val module_name : string
    include Stringable.S with type t := t
  end
  include T
  (* don't want this abstract .. *)
  include Identifiable.Make (T)
end


type type_parameter =
  (* kind annotation is always optional *)
  (Var_id.t * kind option)

type type_result = 
type monotype =
  | Arrow of monotype * ()
  |
;;

type kind_atom =
  | Effect_type
  | Effect_row
  | Value

(* TODO: is slightly more permissive than the grammar *)
type kind =
  | Arrow of kind list * kind_atom
  | Atom of kind_atom

type type_scheme =
  { forall_quantified : type_parameter list
  ; body : monotype
  }

type 'a operation_declaration =
  { id : Var_id.t
  ; type_parameters : type_parameter list
  ; parameters : 'a
  ; result_type : tatomic
  }

(* TODO: better name *)
type operation_shape =
  (* TODO: name fields? *)
  | Val of tatomic
  | Fun of parameters * tatomic
  | Except of parameters * tatomic
  | Control of parameters * tatomic

type operation_declaration =
  { id : Var_id.t
  ; type_parameters : type_parameter list
  ; shape : operation_shape
  }

type effect_declaration =
  { id : Var_id.t
  ; type_parameters : type_parameter list
  ; kind_annotation : option kind
  ; operations : operation_declaration list
  }

type type_declaration =
  (* TODO: records/Effect.t *)
  | Effect_declaration of effect_declaration
(* | Type *)

(* TODO: better naming *)
type fn =
  { type_parameters : type_parameter list
  ; parameters : parameters
  ; result_type : tresult option
  ; body : ??
  }

type fun_declaration =
  { id : funid
  ; fn : fn
  }




type declaration =
  | Fun of fun_declaration
  | Val of apattern * blockexpr

type literal =
  | Int of int
  (* TODO: hardcode bools *)

type unary_operator =
  | Exclamation

type binary_operator =
  | Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | And
  | Or
  | Not
  (* TODO: equals will require overloading? *)
  | Equals
  | Not_equal
  | Less_than
  | Less_equal
  | Greater_than
  | Greater_equal

type argument = expr

(* these do coincide for now, but they may not later *)
type parameter_id =
  | Id of Identifier.t
  | Wildcard
type parameter =
  { id : parameter_id
  ; type_ : type_
  }

type pattern =
  | Id of Identifier.t
  | Wildcard

type pattern_parameter =
  { pattern : pattern
  ; type_ : option type_
  }


type expr =
  | block
  | Return of expr
  (* | withexpr *)
  | Val_in of apattern * blockexpr * expr
  | If_then_else of ntl_expr * expr * expr
  | If_then of ntl_expr * expr
  (* | Match *)
  | Handler
  | Fn of fn
  | Binary_op of expr * binary_operator * expr
  | Unary_op of unary_operator * expr
  | Application of expr * argument list
  | Application of application_expr
  | Identifier of Identifier.t
  | Literal of literal
  (* | Tuple of expr list *)
  (* | List of expr list *)
  (* TODO: how best to do annotations? *)
  | Annotated of expr * type_scheme



type statement =
  | Declaration of declaration
  | With of with_statement
  | With_in of with_statement * block_expr
  | Return of return_expr
  | Basic_expr or Expr of ...
;;

type block = statement list

type binder = Identifier.t * type_ option
type pure_declaration =
  | Val of binder * blockexpr
  | Fun of fun_declaration

type toplevel_declaration =
  | Pure_declaration of pure_declaration
  | Type_declaration of type_declaration

type program = toplevel_declaration list
