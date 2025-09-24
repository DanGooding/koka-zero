open Core
open Koka_zero_util

(* Names: *)

module Var_id : Identifiable.S
module Constructor_id : Identifiable.S
module Wildcard_id : Identifiable.S

module Identifier : sig
  (** augments a [Var_id.t] with information about which kind of name it is.
      This information is derived from the string (capitalisation etc.), so the
      variants can be collapsed back together without issues of collision. *)
  type t =
    | Var of Var_id.t
    | Constructor of Constructor_id.t
  [@@deriving compare, sexp_of]

  include Comparable.S_plain with type t := t
end

val resume_keyword : Identifier.t

(* Kinds: *)

type kind_atom =
  | Kind_effect_type
  | Kind_effect_row
  | Kind_value
[@@deriving sexp_of]

type kind =
  | Kind_arrow of kind list * kind
  | Kind_atom of kind_atom
[@@deriving sexp_of]

(* types and effects: *)

(** parameter which stands for a type

    e.g. in `forall<a> a -> list<a>`, `forall` and `list` each accept one
    [type_parameter] *)
type type_parameter =
  { id : Var_id.t
  ; kind : kind option
  }
[@@deriving sexp_of]

(* note: typechecker will remove the nonsensical cases based on kind checking

   the parser does nothing in this regard, but here is little it can do, since
   `a` can always be a type variable or an effect variable, and `eff` can always
   be a type or an effect *)

(** types and effects are the same thing. *)
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
[@@deriving sexp_of]

and type_scheme =
  { forall_quantified : type_parameter list
  ; body : type_
  }
[@@deriving sexp_of]

and type_constructor =
  | Variable_or_name of Var_id.t
  | Type_wildcard of Wildcard_id.t
  (* builtin types *)
  | Type_int
  | Type_bool
  | Type_list
  | Type_option
[@@deriving sexp_of]

(** represents the `x : int` in `(x : int, y : int) -> int` *)
and parameter_type =
  { parameter_id : Identifier.t option
  ; type_ : type_
  }
[@@deriving sexp_of]

(** a function call results may perform some effects, and result in a value

    if a result type is given, the effect must be given too, or it defaults to
    total (<>) *)
and type_result =
  { effect_ : type_
  ; result : type_
  }
[@@deriving sexp_of]

(** A fixed length effect row such as `<x,y,z>`, or one with a variable tail
    such as `<x,y,z|e>` *)
and effect_row =
  | Closed of type_ list
  | Open of type_ Non_empty_list.t * type_
[@@deriving sexp_of]

(** an empty effect row - i.e. the 'total' effect (`<>`) *)
val total_effect_row : effect_row

(* Parameters and binders: *)

type parameter_id =
  | Parameter_id of Identifier.t
  | Parameter_wildcard
[@@deriving sexp_of]

(** a 'plain' parameter with just a name and type. used when declaring effect
    operations *)
type parameter =
  { id : parameter_id
  ; type_ : type_
  }
[@@deriving sexp_of]

type irrefutable_pattern =
  | Pattern_id of Identifier.t
  | Pattern_wildcard
  | Pattern_tuple of irrefutable_pattern list
[@@deriving sexp_of]

type annotated_pattern =
  { pattern : irrefutable_pattern
  ; scheme : type_scheme option
  }
[@@deriving sexp_of]

(** a function parameter, which may be annotated, and perform an irrefutable
    pattern match *)
type pattern_parameter =
  { pattern : irrefutable_pattern
  ; type_ : type_ option
  }
[@@deriving sexp_of]

(** binds a single identifier, such as in a [Val] operation *)
type binder =
  { id : Identifier.t
  ; type_ : type_ option
  }
[@@deriving sexp_of]

val pattern_parameter_of_binder : binder -> pattern_parameter

(* operations: *)

type operation_shape =
  | Shape_val of type_
  | Shape_fun of parameter list * type_
  | Shape_except of parameter list * type_
  | Shape_control of parameter list * type_
[@@deriving sexp_of]

type operation_declaration =
  { id : Var_id.t
  ; type_parameters : type_parameter list
  ; shape : operation_shape
  }
[@@deriving sexp_of]

type effect_declaration =
  { id : Var_id.t
  ; type_parameters : type_parameter list
  ; kind : kind option
  ; operations : operation_declaration list
  }
[@@deriving sexp_of]

type type_declaration = Effect_declaration of effect_declaration
(* | Type *)
[@@deriving sexp_of]

(* expressions: *)

type literal =
  | Int of int
  | Bool of bool
[@@deriving sexp_of]

type unary_operator = Exclamation [@@deriving sexp_of]

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
[@@deriving sexp_of]

type pattern =
  | Irrefutable_pattern of irrefutable_pattern
  | Pattern_literal of literal
  | Pattern_constructor of Constructor_id.t * pattern list
[@@deriving sexp_of]

(** an expression - evaluates to a value *)
type expr =
  | Return of expr
  | If_then_else of expr * block * block
  | If_then of expr * block
  | Match of expr * (pattern * block) list
  | Handler of effect_handler
  | Handle of
      { subject : expr (** this expression evaluates to a 0 argument function *)
      ; handler : effect_handler
        (** which is called under this effect handler *)
      }
  | Fn of fn
  | Binary_op of expr * binary_operator * expr
  | Unary_op of unary_operator * expr
  | Application of expr * expr list
  | Tuple_construction of expr list
  | Identifier of Identifier.t
  | Literal of literal
  | Annotated_expr of expr * type_scheme
[@@deriving sexp_of]

and statement =
  | Declaration of declaration
  (* note this may be a return expression! *)
  | Expr of expr
[@@deriving sexp_of]

(** a list of statements, the last an expression, therefore this evaluates to a
    value *)
and block =
  { statements : statement list
  ; last : expr
  }
[@@deriving sexp_of]

(** a function, either anonymous or named, but the name must be held elsewhere
*)
and fn =
  { type_parameters : type_parameter list
  ; parameters : pattern_parameter list
  ; result_type : type_result option
  ; body : block
  }
[@@deriving sexp_of]

and declaration =
  | Fun of fun_declaration
  | Val of annotated_pattern * block
[@@deriving sexp_of]

(** declaration of a named function *)
and fun_declaration =
  { id : Identifier.t
  ; fn : fn
  }
[@@deriving sexp_of]

and operation_handler =
  | Op_val of
      { id : Var_id.t
      ; type_ : type_ option
      ; value : block
      }
  | Op_fun of
      { id : Var_id.t
      ; parameters : pattern_parameter list
      ; body : block
      }
  | Op_except of
      { id : Var_id.t
      ; parameters : pattern_parameter list
      ; body : block
      }
  | Op_control of
      { id : Var_id.t
      ; parameters : pattern_parameter list
      ; body : block
      }
  | Op_return of
      { parameter : pattern_parameter
      ; body : block
      }
[@@deriving sexp_of]

and effect_handler = Effect_handler of operation_handler list
[@@deriving sexp_of]

(** a declaration of a value/function which can appear at the toplevel *)
type pure_declaration =
  | Top_val of binder * block
  | Top_fun of fun_declaration
[@@deriving sexp_of]

type toplevel_declaration =
  | Pure_declaration of pure_declaration
  | Type_declaration of type_declaration
[@@deriving sexp_of]

(** root of the AST: represents and entire program *)
type program = Program of toplevel_declaration list [@@deriving sexp_of]

(** build a single line block from a single expression *)
val singleton_block : expr -> block

(** add a statement to the start of a block *)
val block_cons : statement -> block -> block

(** builds a 0 argument anonymous function with a given body *)
val anonymous_of_block : block -> fn

(** builds a 1 argument anonymous function with a given parameter and body *)
val anonymous_of_bound_block : binder:binder -> block:block -> fn

(** `with` syntax: insert an anonymous function as the last argument to an
    application (or apply the [expr] to the callback if it is not already an
    [Application]) *)
val insert_with_callback : callback:fn -> expr -> expr
