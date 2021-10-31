(* TODO: rewrite to use modules? *)
(* XXX: desugaring now *)
(* TODO: ** doc comments *)
(* TODO: deriving sexp *)
(* TODO: Identifiable for names *)

(* TODO: don't copy grammar too closely *)
(* TODO: but make sure to desscribe the syntax only (don't throw out badly typed terms)*)

open Core
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

type kind_atom =
  | Effect_type
  | Effect_row
  | Value

type kind =
  | Arrow of kind list * kind_atom
  | Atom of kind_atom

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

(* TODO: better name *)
type operation_shape =
  (* TODO: name fields? *)
  | Val of type_
  | Fun of parameters * type_
  | Except of parameters * type_
  | Control of parameters * type_

type operation_declaration =
  { id : Var_id.t
  ; type_parameters : type_parameter list
  ; shape : operation_shape
  }

type effect_declaration =
  { id : Var_id.t
  ; type_parameters : type_parameter list
  ; kind : option kind
  ; operations : operation_declaration list
  }

type operation_parameter =
  { id : parameter_id
  ; type_ : option type_
  }

type operation_handler =
  | Val of
      { id : Var_id.t
      ; type_ : option type_
      ; value : blockexpr
      }
  (* TODO: sharing betweem different shapes? *)
  | Fun of
      { id : Var_id.t
      ; parameters : operation_parameter list
      ; body : block
      }
  | Except of
      { id : Var_id.t
      ; parameters : operation_parameter list
      ; body : block
      }
  | Control of
      { id : Var_id.t
      ; parameters : operation_parameter list
      ; body : block
      }
  | Return of
      { parameter : operation_parameter
      ; body : block
      }

type handler = Handler of operation_handler list

type type_declaration =
  (* TODO: records/Effect.t *)
  | Effect_declaration of effect_declaration
(* | Type *)

(* TODO: better naming *)
type fn =
  { type_parameters : type_parameter list
  ; parameters : pattern_parameter list
  ; result_type : type_result option
  ; body : block
  }

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

type fun_declaration =
  { id : Identifier.t
  ; fn : fn
  }

type declaration =
  | Fun of fun_declaration
  | Val of apattern * block

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

let pattern_parameter_of_binder : pattern_parameter -> binder =
 fun binder ->
  let { id; type_ } = binder in
  { pattern = Id id; type_ }
;;

type annotated_pattern =
  { pattern : pattern
  ; scheme : type_scheme
  }

type expr =
  | Return of expr
  (* | withexpr *)
  | Val_in of apattern * blockexpr * expr
  | If_then_else of ntl_expr * expr * expr
  | If_then of ntl_expr * expr
  (* | Match *)
  (* TODO: which of [handle]/[handler] is the logical primitive? *)
  | Handler of handler
  | Fn of fn
  | Binary_op of expr * binary_operator * expr
  | Unary_op of unary_operator * expr
  | Application of expr * argument list
  | Identifier of Identifier.t
  | Literal of literal
  (* | Tuple of expr list *)
  (* | List of expr list *)
  (* TODO: how best to do annotations? *)
  | Annotated of expr * type_scheme

type statement =
  | Declaration of declaration
  (* note this may be a return expression! *)
  | Expr of expr

(** `with` syntax: insert an anonymous function as the last argument to an application (or
    apply the [expr] to the callback if it is not already an [Application]) *)
let insert_with_callback : callback:fn -> expr -> expr =
 fun ~callback e ->
  match e with
  | Application (f, args) -> Application (f, args @ [ Fn callback ])
  (* TODO: wildcard match is brittle *)
  | _ -> Application (e, [ Fn callback ])
;;

type block =
  { statements : statement list
  ; last : expr
  }

type binder =
  { id : Identifier.t
  ; type_ : type_ option
  }

type pure_declaration =
  | Val of binder * block
  | Fun of fun_declaration

type toplevel_declaration =
  | Pure_declaration of pure_declaration
  | Type_declaration of type_declaration

type program = Program of toplevel_declaration list
