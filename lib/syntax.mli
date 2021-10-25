(* TODO: rewrite to use modules? *)
(* XXX: desugaring now *)
(* TODO: ** doc comments *)
(* TODO: deriving sexp *)
(* TODO: Identifiable for names *)

(* TODO: don't copy grammar too closely *)
(* TODO: but make sure to desscribe the syntax only (don't throw out
   badly typed terms)*)

type type_parameter =
  (* production is varid, but this is a type_id really *)
  (* kind annotation is always optional *)
  (varid * kind option)

type type_result = 
type monotype =
  | Arrow of monotype * ()
  | 

type type_scheme =
  { forall_quantified : type_parameter list
  ; body : monotype
  }

(* TODO: namespace by effect (to distinguish from operator) *)
type operation =
  (* TODO: name fields *)
  (* TODO: factor out duplication *)
  | Val of identifier * type_parameter list * tatomic
  | Fun of identifier * type_parameter list * parameters * tatomic
  | Except of identifier * type_parameter list * parameters * tatomic
  | Control of identifier * type_parameter list * parameters * tatomic

type type_declaration =
  (* TODO: records/Effect.t *)
  | Effect_declaration of varid * type_parameter list * kannot * operation list
(* | Type *)

type fun_declaration = funid * type_parameter list * parameters * tresult option * block
(* TODO: should type_parameter list go here, or in pure_declaration.Fun? *)

type declaration =
  | Fun of fun_declaration
  | Val of apattern * blockexpr
type statement =
  | Declaration of declaration
  | With of with_statement
  | With_in of with_statement * block_expr
  | Return of return_expr
  | Basic_expr or Expr of ...
;;
type block = statement list

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
