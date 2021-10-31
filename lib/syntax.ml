(* TODO: rewrite to use modules? *)
(* TODO: ** doc comments *)
(* TODO: deriving sexp *)

open Core

(* Names: *)

module Var_id : Identifiable = String
module Operator_id : Identifiable = String
module Constructor_id : Identifiable = String

module Identifier = struct
  module T = struct
    type t = Var of Var_id.t
    (* | Operator of Operator_id.t *)
    (* | Constructor of Constructor_id.t *)
    [@@deriving compare, bin_io, hash, sexp]

    let module_name = "Identifier"
    let of_string s = Sexp.of_string s |> t_of_sexp
    let to_string t = sexp_of_t t |> Sexp.to_string
  end

  include T
  include Identifiable.Make (T)
end

(* Kinds: *)

type kind_atom =
  | Effect_type
  | Effect_row
  | Value

type kind =
  | Arrow of kind list * kind_atom
  | Atom of kind_atom

(* types and effects: *)

(** parameter which stands for a type

    e.g. in `forall<a> a -> list<a>`, `forall` and `list` each accept one [type_parameter] *)
type type_parameter =
  { id : Var_id.t
  ; kind : kind option
  }

(* note: typechecker will remove the nonsensical cases based on kind checking

   the parser does nothing in this regard, but here is little it can do, since `a` can
   always be a type variable or an effect variable, and `eff` can always be a type or an
   effect *)

(** types and effects are the same thing. *)
type type_ =
  | Arrow of parameter_type list * type_result
  | Effect_row of effect_row
  | Scheme of type_scheme
  | Atom of
      { constructor : type_constructor
      ; arguments : type_ list
      }
  | Annotated of
      { type_ : type_
      ; kind : kind
      }

and type_scheme =
  { forall_quantified : type_parameter list
  ; body : type_
  }

and type_constructor =
  | Variable_or_name of Var_id.t
  (* TODO: wildcard names are disjoint from Var_id.t, maybe a new type? *)
  | Wildcard of Var_id.t
  (* builtin types *)
  | Int
  | Bool

(** represents the `x : int` in `(x : int, y : int) -> int` *)
and parameter_type =
  { parameter_id : Identifier.t option
  ; type_ : type_
  }

(** a function call results may perform some effects, and result in a value

    if a result type is given, the effect must be given too, or it defaults to total (<>) *)
and type_result =
  { effect : type_
  ; result : type_
  }

(** A fixed length effect row such as `<x,y,z>`, or one with a variable tail such as
    `<x,y,z|e>` *)
and effect_row =
  | Closed of type_ list
  (* note: open row is guaranted to be nonempty *)
  (* TODO: current repr has redundancy [e]==[<|e>] *)
  | Open of type_ list * type_

(** an empty effect row - i.e. the 'total' effect (`<>`) *)
let total_effect_row : effect_row = Closed []

(* Parameters and binders: *)

type parameter_id =
  | Id of Identifier.t
  | Wildcard

(** a 'plain' parameter with just a name and type. used when declaring effect operations *)
type parameter =
  { id : parameter_id
  ; type_ : type_
  }

type pattern =
  | Id of Identifier.t
  | Wildcard

type annotated_pattern =
  { pattern : pattern
  ; scheme : type_scheme
  }

(** a function parameter, which may be annotated, and perform an irrefutable pattern match *)
type pattern_parameter =
  { pattern : pattern
  ; type_ : type_ option
  }

(** binds a single identifier, such as in a [Val] operation *)
type binder =
  { id : Identifier.t
  ; type_ : type_ option
  }

let pattern_parameter_of_binder : binder -> pattern_parameter =
 fun binder ->
  let ({ id; type_ } : binder) = binder in
  { pattern = Id id; type_ }
;;

(* operations: *)

(* TODO: better name *)
type operation_shape =
  (* TODO: name fields? *)
  | Val of type_
  | Fun of parameter list * type_
  | Except of parameter list * type_
  | Control of parameter list * type_

type operation_declaration =
  { id : Var_id.t
  ; type_parameters : type_parameter list
  ; shape : operation_shape
  }

type effect_declaration =
  { id : Var_id.t
  ; type_parameters : type_parameter list
  ; kind : kind option
  ; operations : operation_declaration list
  }

type type_declaration =
  (* TODO: records/Effect.t *)
  | Effect_declaration of effect_declaration
(* | Type *)

type operation_parameter =
  { id : parameter_id
  ; type_ : type_ option
  }

(* expressions: *)

type literal =
  | Int of int
  | Bool of bool

type unary_operator = Exclamation

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

(** an expression - evaluates to a value *)
type expr =
  | Return of expr
  (* TODO: val_in_ syntax not common in koka - drop? *)
  | Val_in of annotated_pattern * block * expr
  | If_then_else of expr * expr * expr
  | If_then of expr * expr
  (* | Match *)
  (* TODO: which of [handle]/[handler] is the logical primitive? *)
  | Handler of effect_handler
  | Fn of fn
  | Binary_op of expr * binary_operator * expr
  | Unary_op of unary_operator * expr
  | Application of expr * expr list
  | Identifier of Identifier.t
  | Literal of literal
  (* | Tuple of expr list *)
  (* | List of expr list *)
  | Annotated of expr * type_scheme

and statement =
  | Declaration of declaration
  (* note this may be a return expression! *)
  | Expr of expr

(** a list of statements, the final an expression, evaluates to a value*)
and block =
  { statements : statement list
  ; last : expr
  }

(** a function, either anonymous or named, but the name must be held elsewhere *)
and fn =
  { type_parameters : type_parameter list
  ; parameters : pattern_parameter list
  ; result_type : type_result option
  ; body : block
  }

and declaration =
  | Fun of fun_declaration
  | Val of annotated_pattern * block

(** declaration of a named function *)
and fun_declaration =
  { id : Identifier.t
  ; fn : fn
  }

and operation_handler =
  | Op_val of
      { id : Var_id.t
      ; type_ : type_ option
      ; value : block
      }
  (* TODO: sharing betweem different shapes? *)
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

and effect_handler = Effect_handler of operation_handler list

(** a declaration of a value/function which can appear at the toplevel *)
type pure_declaration =
  | Val of binder * block
  | Fun of fun_declaration

type toplevel_declaration =
  | Pure_declaration of pure_declaration
  | Type_declaration of type_declaration

(** root of the AST: represents and entire program *)
type program = Program of toplevel_declaration list

(** builds a 0 argument anonymous function with a given body *)
let anonymous_of_block : block -> fn =
 fun body -> { type_parameters = []; parameters = []; result_type = None; body }
;;

(** builds a 1 argument anonymous function with a given parameter and body *)
let anonymous_of_bound_block : binder:binder -> block:block -> fn =
 fun ~binder ~block ->
  let parameter = pattern_parameter_of_binder binder in
  { type_parameters = []; parameters = [ parameter ]; result_type = None; body = block }
;;

(** `with` syntax: insert an anonymous function as the last argument to an application (or
    apply the [expr] to the callback if it is not already an [Application]) *)
let insert_with_callback : callback:fn -> expr -> expr =
 fun ~callback e ->
  match e with
  | Application (f, args) -> Application (f, args @ [ Fn callback ])
  (* note: technically, [Annotated(e, scheme)] should be possible if [e] is an
     application, but real koka also forbids this. (There may be some difficulies with
     capture of type variables) *)
  | Annotated (_, _)
  | Return _
  | Val_in (_, _, _)
  | If_then_else (_, _, _)
  | If_then (_, _)
  | Handler _ | Fn _
  | Binary_op (_, _, _)
  | Unary_op (_, _)
  | Identifier _ | Literal _ -> Application (e, [ Fn callback ])
;;
