(* TODO: rewrite to use modules? *)
(* XXX: desugaring now *)
(* TODO: ** doc comments *)
(* TODO: deriving sexp *)
(* TODO: Identifiable for names *)

(* TODO: don't copy grammar too closely *)
(* TODO: but make sure to desscribe the syntax only (don't throw out
   badly typed terms)*)

type type_parameter =
  (* kind annotation is always optional *)
  (varid * kind option)

type type_result = 
type monotype =
  | Arrow of monotype * ()
  |
;;

(* TODO: just haave builtin kinds V, E ? *)
type kind =
  (* TODO: grammar seems to allow curried & uncurried forms - equivalent? *)
  | Arrow_uncurried of kind nonempty_list * conid
  | Arrow_curried of conid * kind
  | Atom of conid (* TODO: alias conid -> katom? *)

type type_scheme =
  { forall_quantified : type_parameter list
  ; body : monotype
  }

type 'a operation_declaration =
  { id : varid
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
  { id : varid
  ; type_parameters : type_parameter list
  ; shape : operation_shape
  }

type effect_declaration =
  { id : varid
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
  | Not

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
  | Binary_op of expr * operator * expr
  | Unary_op of unary_operator * expr
  | Application of expr * expr list
  (* (\* this must be desugared later, the parser cannot *)
  (*    guarantee it is correct, due to cases such as {[ *)
  (*      val foo = fn(x) { x + 1 } *)
  (*      42.foo *)
  (*    ]} *\) *)
  (* | Dot_application of expr * expr *)
  | Application of application_expr
  | Identifier of id
  | Literal of literal
  (* | Tuple of expr list *)
  (* | List of expr list *)



type statement =
  | Declaration of declaration
  | With of with_statement
  | With_in of with_statement * block_expr
  | Return of return_expr
  | Basic_expr or Expr of ...
;;

type block = statement list
(* TODO:
   is there a semantic difference between expr/basic_expr
   block/block_expr etc.

*)

type binder = identifier * type_ option
type pure_declaration =
  | Val of binder * blockexpr
  | Fun of fun_declaration

type toplevel_declaration =
  | Pure_declaration of pure_declaration
  | Type_declaration of type_declaration

type program = toplevel_declaration list

(* TODO: are ids all the same type or not? varid != opid != conid but what aboout type
   vs.function vs. effect? *)
