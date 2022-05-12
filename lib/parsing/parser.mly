(*
  Modified from the koka (v2.3.2) grammar specification
  https://github.com/koka-lang/koka/blob/v2.3.2/doc/spec/grammar/parser.y
  the license of which is reproduced below.
  Modifications include converting from Bison to Menhir syntax, removing
  productions for syntax not supported in KokaZero, restructuring productions 
  to resolve ambiguities, and building an AST from the parse.
*)
(* Copyright 2012-2021, Microsoft Research, Daan Leijen
   This is free software; you can redistribute it and/or modify it under the
   terms of the Apache License, Version 2.0.
*)

%token EOF

%token <string> ID (* CONID OP IDOP *)
%token <string> WILDCARD

%token <int> INT
%token <bool> BOOL
(* %token <Float> FLOAT *)
(* %token <string> STRING *)
(* %token <char> CHAR *)

(* string literals define nicer syntax for use in the production rules
   they do not determine when the lexer produces these tokens *)
%token OPEN_ROUND  "(" CLOSE_ROUND  ")"
(* %token OPEN_SQUARE "[" CLOSE_SQUARE "]" *)
%token OPEN_CURLY  "{" CLOSE_CURLY  "}"

%token LESS_THAN "<" GREATER_THAN ">"
%token PIPE "|"
%token DOT "."
%token COLON ":"
%token DCOLON "::"
%token COMMA ","
%token SEMI ";"
%token EQUALS "="
%token EXCLAMATION_MARK "!"

%token IF THEN ELSE ELIF
%token WITH
(* %token MATCH *)
%token RARROW "->" LARROW "<-"

%token FUN FN VAL (* VAR *) CONTROL EXCEPT
%token (* TYPE STRUCT *) EFFECT
(* %token ALIAS CON *)
%token FORALL

(* %token INFIX INFIXL INFIXR *)

(* %token ASSIGN *)
%token RETURN

%token HANDLER HANDLE (* NAMED MASK *)

%token ID_INITIALLY ID_FINALLY

(* builtins *)
%token KIND_V KIND_E, KIND_X

%token OP_OR "||"
%token OP_AND "&&"
%token OP_NOT_EQUAL "!="
%token OP_EQUAL_EQUAL "=="
%token OP_GREATER_EQUAL "<="
%token OP_LESS_THAN ">="
%token OP_PLUS "+"
%token OP_MINUS "-"
%token OP_TIMES "*"
%token OP_DIVIDE "/"
%token OP_MODULO "%"

%token TYPE_BOOL
%token TYPE_INT


(* precedence declarations are in increasing order,
   i.e. the last precedence declaration has the highest precedence.
*)

(* resolve s/r conflict by shifting on ELSE so the ELSE binds to the closest
   IF. *)
%nonassoc THEN
%nonassoc ELSE ELIF

(* resolve s/r conflict to have a `FN funparams -> expr` span as far as possible,
   e.g. `fn(x) -> x + 1` is `(fn(x) -> x + 1)` and not `(fn(x) -> x) + 1`
   and  `fn(x) -> x.foo` is `(fn(x) -> x.foo)` and not `(fn(x) -> x).foo`
   note: we could avoid these rules by disallowing the `->` form in trailing
   lambdas.
*)
%nonassoc RARROW                     (* -> *)
%nonassoc "(" (* "[" *) FN "{" "."         (* applications *)
(* %nonassoc (\* OP ASSIGN *\) ">" "<" (\* "|" *\)      (\* operators *\) *)
(* %prec "?" *)

%{

open Syntax
open Koka_zero_util

(** an intermediate type for desugaring function application *)
type partial_appexpr =
  [ `Application of expr * expr list
     (** an [`Application] can be followed by a trailing lambda *)
  | `Expr of expr
     (** an [`Expr] is opaque to the desugaring algorithm -
         it maybe have been wrapped in brackets, so we should not look inside it
     *)
  | `Dot_application of expr * expr
     (** a [`Dot_application] can be followed by more bracketed args *)
  ]
;;

(** desugar the application of [app] (<= [partial_appexpr]) to [trailing] *)
let desugar_add_trailing_lambda app (trailing : expr) : partial_appexpr =
    match app with
    | `Application(f, args)     -> `Application(f, args @ [trailing])
    | `Dot_application(f, arg0) -> `Application(f, [arg0; trailing])
    | `Expr e                   -> `Application(e, [trailing])
;;

let desugar_add_args app (args : expr list) : partial_appexpr =
  match app with
  | `Dot_application(f, arg0) -> `Application(f, arg0 :: args)
  | `Application(f, args')    -> `Application(Application(f, args'), args)
  | `Expr f                   -> `Application(f, args)
;;

(** turn a partial desugaring intermediate back into a plain expression *)
let close_after_desugaring app : expr =
  match app with
  | `Dot_application(f, arg0) -> Application(f, [arg0])
  | `Application(f, args)     -> Application(f, args)
  | `Expr e                   -> e
;;

%}

%start program

%type <program> program
%type <toplevel_declaration list> declarations
%type <toplevel_declaration list> topdecls
%type <toplevel_declaration> topdecl
%type <type_declaration> typedecl
%type <effect_declaration> effectdecl
%type <operation_declaration list> opdecls
%type <operation_declaration list> operations
%type <operation_declaration> operation
%type <pure_declaration> puredecl
%type <fun_declaration> fundecl
%type <binder> binder
%type <Identifier.t> funid
%type <fn> funbody
%type <block> block
%type <block> blockcontents
%type <declaration> decl
%type <expr> exprstatement
%type <expr> withstat
%type <block> bodyexpr
%type <block> blockexpr
%type <expr> expr_except_block
%type <expr> expr
%type <expr> basicexpr
%type <expr> fnexpr
%type <expr> returnexpr
%type <expr> ifexpr
%type <block> elifs
%type <expr> opexpr
%type <expr> opexpr_of(ntlprefixexpr, prefixexpr)
%type <expr> opexpr_of(ntlprefixexpr, ntlprefixexpr)
%type <binary_operator> op_n40
%type <binary_operator> op_l60
%type <binary_operator> op_l70
%type <expr> prefixexpr
%type <expr> appexpr
%type <partial_appexpr> auxappexpr
%type <expr> trailinglambda
%type <expr> ntlexpr
%type <expr> ntlopexpr
%type <expr> ntlprefixexpr
%type <expr> ntlappexpr
%type <partial_appexpr> auxntlappexpr
%type <expr> atom
%type <literal> literal
%type <expr list> arguments
%type <expr> argument
%type <parameter list> parameters
%type <parameter list> parameters1
%type <parameter> parameter
%type <parameter_id> paramid
%type <type_> paramtype
%type <pattern_parameter list> pparameters
%type <pattern_parameter> pparameter
(* %type <expr list> aexprs *)
%type <expr> aexpr
%type <type_scheme option> annot
%type <Identifier.t> identifier
%type <Var_id.t> varid
%type <annotated_pattern> apattern
%type <pattern> pattern
%type <expr> handlerexpr
%type <effect_handler> opclauses
%type <operation_handler> opclausex
%type <operation_handler> opclause
%type <operation_parameter list> opparams
%type <operation_parameter> opparam
%type <type_parameter list> tbinders
%type <type_parameter> tbinder
%type <type_scheme> typescheme
%type <type_> type_
%type <type_parameter list> typeparams
%type <type_parameter list> typeparams1
%type <type_> tarrow
%type <type_result> tresult
%type <type_> tatomic
%type <type_> tbasic
%type <type_> typeapp
%type <type_constructor> typecon
%type <parameter_type list> tparams
%type <parameter_type> tparam
%type <type_ list> targuments
%type <type_ Non_empty_list.t> targuments1
%type <type_> anntype
%type <kind option> kannot
%type <kind> kind
%type <kind list> kinds1
%type <kind_atom> katom

%%

(* ---------------------------------------------------------
-- Program
----------------------------------------------------------*)

(* %type <program> program *)
program:
  | semi*; ds = declarations; EOF
    { Program ds }
  ;

semi:
  | ";"
  { () }
  ;

(* TODO inline this? *)
semi_terminated_list(X):
  | xs = list(x = X; semi+ { x })
    { xs }

semi_terminated_nonempty_list(X):
  | xs = nonempty_list(x = X; semi+ { x })
    { xs }

(* ---------------------------------------------------------
-- Top level declarations
----------------------------------------------------------*)

(* %type <toplevel_declaration list> declarations *)
declarations:
  (* | fixitydecl semi+ declarations *)
  | ds = topdecls
    { ds }
  ;

(* fixitydecl:
  | fixity oplist1 *)
(*  ; *)

(* fixity:
  | INFIX NAT *)
(*  | INFIXR NAT *)
(*  | INFIXL NAT *)
(*  ; *)

(* oplist1:
  | oplist1 "," identifier *)
(*  | identifier *)
(*  ; *)


(* %type <toplevel_declaration list> topdecls *)
topdecls:
  | ts = list(topdecl)
    { ts }
  ;
(* TODO: error recovery? [(topdecl | error) semi+] *)

(* %type <toplevel_declaration> topdecl *)
topdecl:
  | p = puredecl { Pure_declaration p }
  (* TODO: keep aliases? *)
  (* | aliasdecl                            { printDecl("alias",$2); } *)
  | t = typedecl { Type_declaration t }
  ;




(* ---------------------------------------------------------
-- Type declarations
----------------------------------------------------------*)
(* aliasdecl:
  | ALIAS typeid typeparams kannot "=" type_     { Var_id.of_string $2; } *)
(*  ; *)

(* %type <type_declaration> typedecl *)
typedecl:
  (* | typemod TYPE typeid typeparams kannot typebody       *)
  (* | structmod STRUCT typeid typeparams kannot conparams  *)
  | e = effectdecl { Effect_declaration e }
  ;

(* %type <effect_declaration> effectdecl *)
effectdecl:
  | EFFECT;
    id = varid;
    type_parameters = typeparams;
    kind = kannot;
    operations = opdecls;
    semi*
    { { id; type_parameters; kind; operations } }
  | EFFECT;
    type_parameters = typeparams;
    kind = kannot;
    operation = operation;
    semi+
    {
      let { id; _ } : operation_declaration = operation in
      let operations = [operation] in
      { id; type_parameters; kind; operations }
    }
  ;

(* typemod:
  | structmod *)
(*  ; *)

(* structmod:
  | *)
(*  (\* | ID_VALUE *\) *)
(*  (\* | ID_REFERENCE *\) *)
(*  | (\* empty *\) *)
(*  ; *)

(* typebody:
  | "{" semi* constructors "}" *)
(*  | (\* empty *\) *)
(*  ; *)

(* typeid:
  | "(" commas ")"     { Var_id.of_string "(,)"; }       (\* tuples *\) *)
(*  | "[" "]"          { Var_id.of_string "[]"; }        (\* lists *\) *)
(*  | "<" ">"          { Var_id.of_string "<>"; }         (\* total effect *\) *)
(*  | "<" "|" ">"      { Var_id.of_string "<|>"; }    (\* effect extension *\) *)
(*  | varid            { Var_id.of_string $1; } *)
(*  ; *)


(* constructors: constructors1 semi+ *)
(*  | (\* empty *\) *)
(*  ; *)

(* constructors1: constructors1 semi+ constructor *)
(*  | constructor *)
(*  ; *)

(* constructor:
  | con conid typeparams conparams *)
(*  | con STRING typeparams conparams *)
(*  ; *)

(* con:
  | CON *)
(*  | (\* empty *\) *)
(*  ; *)

(* conparams:
  | "(" parameters1 ")"          (\* deprecated *\) *)
(*  | "{" semi* sconparams "}" *)
(*  | (\* empty *\) *)
(*  ; *)

(* sconparams:
  | sconparams parameter semi+ *)
(*  | (\* empty *\) *)
(*  ; *)


(* ---------------------------------------------------------
-- Effect declarations
----------------------------------------------------------*)


(* %type <operation_declaration list> opdecls *)
opdecls:
  | "{"; semi*; operations = operations; "}"
    { operations }
  ;

(* %type <operation_declaration list> operations *)
operations:
  | operations = semi_terminated_list(operation)
    { operations }

(* %type <operation_declaration> operation *)
operation:
  | VAL; id = varid; type_parameters = typeparams; ":"; result_type = tatomic
    { let shape = Shape_val result_type in
      { id; type_parameters; shape } }
  | FUN;
    id = varid; type_parameters = typeparams;
    "("; parameters = parameters; ")"; ":"; result_type = tatomic
    { let shape = Shape_fun(parameters, result_type) in
      { id; type_parameters; shape } }
  | EXCEPT; id = varid; type_parameters = typeparams;
    "("; parameters = parameters; ")"; ":"; result_type = tatomic
    { let shape = Shape_except(parameters, result_type) in
      { id; type_parameters; shape } }
  | CONTROL; id = varid; type_parameters = typeparams;
    "("; parameters = parameters; ")"; ":"; result_type = tatomic
    { let shape = Shape_control(parameters, result_type) in
      { id; type_parameters; shape } }
  ;


(* ---------------------------------------------------------
-- Pure (top-level) Declarations
----------------------------------------------------------*)
(* %type <pure_declaration> puredecl *)
puredecl:
  | VAL; binder = binder; "="; body = blockexpr; semi+
    { Top_val(binder, body) }
  | FUN; f = fundecl; semi*
    { Top_fun f }
  ;

(* TODO: update puredecl to include this? *)
(* %type <fun_declaration> fundecl *)
fundecl:
  | id = funid; fn = funbody
    { { id; fn } }
  ;

(* %type <binder> binder *)
binder:
  | id = identifier
    { { id; type_ = None } : binder }
  | id = identifier; ":"; type_ = type_
    { { id; type_ = Some type_ } : binder }
  ;

(* %type <Identifier.t> funid *)
funid:
  | id = identifier
    { id }
  (* TODO: how can a function name be [,,,]? *)
  (* | "[" commas "]"     { Var_id.of_string "[]"; } *)
  (* TODO: are literals as function names needed? *)
  (* | STRING             { Var_id.of_string $1; } *)
  ;

(* %type <fn> funbody *)
funbody:
  | type_parameters = typeparams; "("; parameters = pparameters; ")";
    body = bodyexpr
    { { type_parameters; parameters; result_type = None; body } }
  | type_parameters = typeparams; "("; parameters = pparameters; ")";
    ":"; result_type = tresult;
    body = block
    { { type_parameters; parameters; result_type = Some result_type; body } }
  ;

(* annotres??
  | ":" tresult *)
(*  | (\* empty *\) *)
(*  ; *)


(* ---------------------------------------------------------
-- Statements
----------------------------------------------------------*)

(* TODO: error recovery? {statement | error} *)
(* %type <block> block *)
block:
  (* must end with an expression statement (and not a declaration) *)
  | "{"; semi*; block = blockcontents; "}"
    { block }
  ;

(* merely a helper function for `with` statement desugaring *)
(* %type <block> blockcontents *)
blockcontents:
  (* last in block: expression *)
  | e = exprstatement; semi*
    { singleton_block e }
  (* a with statement wraps the remainder of the block *)
  | e = withstat
    { singleton_block e }
  (* expressions can fgo anywhere in a block, but declarations
     cannot be last *)
  | e = exprstatement; semi+; block = blockcontents
    { block_cons (Expr e) block }
  | d = decl; semi+; block = blockcontents
    { block_cons (Declaration d) block }
  ;

(* %type <declaration> decl *)
decl:
  | FUN; f = fundecl
    { Fun f }
  (* local value declaration can use a pattern binding *)
  | VAL; annotated_pattern = apattern; "="; body = blockexpr
    { Val(annotated_pattern, body) : declaration }
  (* | VAR binder ASSIGN blockexpr   (\* local variable declaration *\) *)
  ;

(* %type <expr> exprstatement *)
exprstatement:
  | e = returnexpr
    { e }
  | e = basicexpr
    { e }
  ;

(* this grabs the rest of the containing block
   so can only be [laststatement] *)
(* %type <expr> withstat *)
withstat:
  | WITH; e = basicexpr; semi+; block = blockcontents
    { let callback = anonymous_of_block block in
      insert_with_callback ~callback e
    }
  (* shorthand for handler *)
  | WITH; handler = opclauses; semi+; block = blockcontents
    { let callback = anonymous_of_block block in
      Application(Handler handler, [Fn callback])
    }
  | WITH; binder = binder; "<-"; e = basicexpr; semi+; block = blockcontents
    { let callback = anonymous_of_bound_block ~binder ~block in
      insert_with_callback ~callback e
    }
  (* deprecated *)
  | WITH; binder = binder; "="; e = basicexpr; semi+; block = blockcontents
    { let callback = anonymous_of_bound_block ~binder ~block in
      insert_with_callback ~callback e
    }
  ;

(* ---------------------------------------------------------
-- Expressions
----------------------------------------------------------*)
(** The body of an [if]/[match] branch, or of an anonymous function.
    This may be a block, or a single expression (without braces) *)
(* %type <block> bodyexpr *)
bodyexpr:
  | block = blockexpr
    { block }
  ;

(* %type <block> blockexpr *)
blockexpr:
  (* a `block` is not interpreted as an anonymous function but as statement
     grouping *)
  | b = block
    { b }
  | e = expr_except_block
    { singleton_block e }
  ;

(* %type <expr> expr_except_block *)
expr_except_block:
  | e = returnexpr
    { e }
  | e = basicexpr
    { e }
  ;

(* %type <expr> expr *)
expr:
  (* `block` interpreted as an anonymous function *)
  | b = block
    { Fn (anonymous_of_block b) }
  | e = expr_except_block
    { e }

(* %type <expr> basicexpr *)
basicexpr:
  | e = ifexpr
    { e }
  (* | matchexpr *)
  | e = handlerexpr
    { e }
  | e = fnexpr
    { e }
  | e = opexpr
    { e }
  ;


(* keyword expressions *)

(* matchexpr: *)
(*   | MATCH ntlexpr "{" semi* matchrules "}" *)
(*   ; *)

(* %type <expr> fnexpr *)
fnexpr:
  (* anonymous function *)
  | FN; f = funbody
    { Fn f }
  ;

(* %type <expr> returnexpr *)
returnexpr:
  | RETURN; e = expr
    { Return e }
  ;

(* %type <expr> ifexpr *)
ifexpr:
  | IF; cond = ntlexpr; THEN; yes_branch = blockexpr; no_branch = elifs
    { If_then_else(cond, yes_branch, no_branch) }
  | IF; cond = ntlexpr; THEN; branch = blockexpr
    { If_then(cond, branch) }
  | IF; cond = ntlexpr; RETURN; e = expr
    { If_then(cond, singleton_block (Return e)) }
  ;

(* %type <block> elifs *)
elifs:
  | ELIF; cond = ntlexpr; THEN; yes_branch = blockexpr; no_branch = elifs
    { If_then_else(cond, yes_branch, no_branch)
      |> singleton_block }
  | ELSE; block = blockexpr
    { block }
  ;


(* operator expression *)

(* %type <expr> opexpr *)
opexpr:
  | e = opexpr_of(ntlprefixexpr, prefixexpr)
    { e }
  ;

(** [opexpr_of] is parameterised by [operand] and the nonterminal which
    operators are surrounded by, and also [rightmost_operand], used for the
    last operand.
    This allows sharing between standard and non-trailing-lambda usages.
    It is intended that [rightmost_operand] produces more strings than
    [operand], and this is used to remove ambiguity, by only allowing trailing
    lambdas in the final operands.
 *)
(* %type <expr> opexpr_of(ntlprefixexpr, prefixexpr) *)
(* %type <expr> opexpr_of(ntlprefixexpr, ntlprefixexpr) *)
opexpr_of(operand, rightmost_operand):
  | e = opexpr_r20(operand, rightmost_operand)
    { e }
  ;

(* Not yet allowing user defined operators,
   so instead hardcoding common ones into the grammar

   Koka operator precedence chart:
   {[
    infixr 80  (^)
    infixl 70  ( * ), (%), (/), cdiv, cmod
    infixr 60  (++)
    infixl 60  (+), (-)
    infix  40  (!=), (==), (<=), (>=), (<), (>)
    infixr 30  (&&)
    infixr 20  (||)
  ]}
  (obtained by grepping the koka standard library):
*)

(* opexpr_[associativity][koka precedence level] *)
opexpr_r20(operand, rightmost_operand):
  | el = opexpr_r30(operand, operand); "||"; er = opexpr_r20(operand, rightmost_operand)
    { Binary_op(el, Or, er) }
  | e = opexpr_r30(operand, rightmost_operand)
    { e }
  ;

opexpr_r30(operand, rightmost_operand):
  | el = opexpr_n40(operand, operand); "&&"; er = opexpr_r30(operand, rightmost_operand)
    { Binary_op(el, And, er) }
  | e = opexpr_n40(operand, rightmost_operand)
    { e }
  ;

(* %type <binary_operator> op_40 *)
op_n40:
  | "==" { Equals }
  | "!=" { Not_equal }
  | "<=" { Less_equal }
  | ">=" { Greater_equal }
  | "<"  { Less_than }
  | ">"  { Greater_than }
  ;

(* non-associative *)
opexpr_n40(operand, rightmost_operand):
  | el = opexpr_l60(operand, operand);
    op = op_n40;
    er = opexpr_l60(operand, rightmost_operand)
    { Binary_op(el, op, er) }
  | e = opexpr_l60(operand, rightmost_operand)
    { e }
  ;

(* %type <binary_operator> op_l60 *)
op_l60:
  | "+" { Plus }
  | "-" { Minus }
  ;

opexpr_l60(operand, rightmost_operand):
  | el = opexpr_l60(operand, operand);
    op = op_l60;
    er = opexpr_l70(operand, rightmost_operand)
    { Binary_op(el, op, er) }
  | e = opexpr_l70(operand, rightmost_operand)
    { e }
  ;

(* %type <binary_operator> op_l70 *)
op_l70:
  | "*" { Times }
  | "%" { Modulo }
  | "/" { Divide }
  ;

opexpr_l70(operand, rightmost_operand):
  | el = opexpr_l70(operand, operand); op = op_l70; er = rightmost_operand
    { Binary_op(el, op, er) }
  | e = rightmost_operand
    { e }
  ;

(* %type <expr> prefixexpr *)
prefixexpr:
  | "!"; e = prefixexpr
    { Unary_op(Exclamation, e) }
  (* | "~"; e = prefixexpr *)
  (*   { Unary_op(?, e) } *)
  | e = appexpr
    %prec RARROW
    { e }
  (* note: unary minus is handled in the lexer:
     [-NUMBER] is always unary, [- ANYTHING] is always binary *)
  ;

(* %type <expr> appexpr *)
appexpr:
  | a = auxappexpr
    %prec RARROW
    { close_after_desugaring a }
  | e = ntlappexpr
    %prec RARROW
    { e }
  ;

(* %type <partial_appexpr> auxappexpr *)
auxappexpr:
  (* application *)
  | f = auxappexpr; "("; args = arguments; ")"
    { desugar_add_args f args }
  (* dot application *)
  | arg0 = appexpr; "."; f = atom
    { `Dot_application(f, arg0) }
  (* trailing function application *)
  | app = auxappexpr; last_arg = trailinglambda
  | app = auxntlappexpr; last_arg = trailinglambda
    { desugar_add_trailing_lambda app last_arg }
  ;

(* %type <expr> trailinglambda *)
trailinglambda:
  | e = fnexpr
    { e }
  | body = block
    { Fn (anonymous_of_block body) }
  ;


(* non-trailing-lambda expression *)
(* %type <expr> ntlexpr *)
ntlexpr:
  | e = ntlopexpr
    { e }
  ;

(* %type <expr> ntlopexpr *)
ntlopexpr:
  | e = opexpr_of(ntlprefixexpr, ntlprefixexpr)
    { e }
  ;

(* %type <expr> ntlprefixexpr *)
ntlprefixexpr:
  (* | "~"; e = ntlprefixexpr *)
  | "!"; e = ntlprefixexpr
    { Unary_op(Exclamation, e) }
  | e = ntlappexpr
    { e }
  ;

(* %type <expr> ntlappexpr *)
ntlappexpr:
  | a = auxntlappexpr
    %prec RARROW
    { close_after_desugaring a }

(* %type <partial_appexpr> auxntlappexpr *)
auxntlappexpr:
  (* application *)
  | f = auxntlappexpr; "("; args = arguments; ")"
    { desugar_add_args f args }
  (* dot application *)
  | arg0 = ntlappexpr; "."; f = atom
    { `Dot_application(f, arg0) }
  | e = atom
    { `Expr e }
  ;

(* atomic expressions *)

(* %type <expr> atom *)
atom:
  | id = identifier
    { Identifier id }
  (* | constructor *)
  | lit = literal
    { Literal lit }
  (* | mask *)
  | "("; e = aexpr; ")"
    { e }
  (* not yet supported: *)
    (* unit, parenthesized (possibly annotated) expression, tuple expression *)
    (* list expression (elements may be terminated with comma instead of
       separated) *)

(* %type <literal> literal *)
literal:
  | i = INT
    { Int i }
  | b = BOOL
    { Bool b }
  (* TODO: better to add tuples with elements to `atom` *)
  | "("; ")"
    { Unit }
  (* | FLOAT | CHAR | STRING *)
  ;

(* mask:
  | MASK behind "<" tbasic ">" *)
(*  ; *)

(* behind:
  | ID_BEHIND *)
(*  | (\* empty *\) *)
(*  ; *)

(* arguments: separated by comma *)

(* %type <expr list> arguments *)
arguments:
  | args = separated_list(",", argument)
    { args }
  ;

(* %type <expr> argument *)
argument:
  | e = expr
    { e }
  (* | identifier "=" expr                  (\* named arguments *\) *)
  ;

(* parameters: separated by comma, must have a type *)

(* these are used only in constructor definitions *)

(* %type <parameter list> parameters *)
parameters:
  | ps = parameters1
    { ps }
  | (* empty *)
    { [] }
  ;

(* TODO: other productions (constructor) require the
   nonempty property, but just [list] is okay for now *)
(* %type <parameter list> parameters1 *)
parameters1:
  | ps = separated_nonempty_list(",", parameter)
    { ps }
  ;

(* %type <parameter> parameter *)
parameter:
  | id = paramid; ":"; type_ = paramtype
    { { id; type_ } : parameter }
  (* | paramid ":" paramtype "=" expr *)
  ;

(* %type <parameter_id> paramid *)
paramid:
  | id = identifier
    { Parameter_id id }
  | WILDCARD
    { Parameter_wildcard }
  ;

(* %type <type_> paramtype *)
paramtype:
  | t = type_
    { t }
  (* TODO: is this just for optional parameters? *)
  (* | '?' type_ *)
  ;


(* pattern matching parameters: separated by comma *)

(* %type <pattern_parameter list> pparameters *)
pparameters:
  | ps = separated_list(",", pparameter)
    { ps }
  ;

(* %type <pattern_parameter> pparameter *)
pparameter:
  | pattern = pattern
    { { pattern; type_ = None } }
  | pattern = pattern; ":"; type_ = paramtype
    { let type_ = Some type_ in
      { pattern; type_ }
    }
  (* pattern=expr - strange form of pattern matching
     seems to be for struct members, or e.g. (fst=1, snd=2) *)
  (* | pattern ":" paramtype "=" expr *)
  (* | pattern "=" expr *)
  ;


(* annotated expressions: separated or terminated by comma *)

(* %type <expr list> aexprs *)
(* aexprs: *)
(*   | es = separated_list(",", aexpr) *)
(*     { es } *)
(*   ; *)

(* %type <expr> aexpr *)
aexpr:
  (* TODO: annot is nullable - this may cause a conflict? *)
  | e = expr; scheme = annot
    { match scheme with
      | Some scheme -> Annotated_expr(e, scheme)
      | None -> e
    }
  ;

(* %type <type_scheme option> annot *)
annot:
  | ":"; s = typescheme
    { Some s }
  | (* empty *)
    { None }
  ;



(* ---------------------------------------------------------
-- Identifiers and operators
----------------------------------------------------------*)

(* operator: *)
(*   | op *)
(*   ; *)

(* TODO: until add user defined operators,
   operators are not a first class concept

   note this loses prefix syntax: [xs.fold(0, (+))]
*)

(* %type <Identifier.t> identifier *)
identifier:
  | var_id = varid
    { Identifier.Var var_id }
  (* | IDOP *)
  ;

(* %type <Var_id.t> varid *)
varid:
  | id = ID
    { Var_id.of_string id }
  (* allow reserved words to be used as identifiers
     in unambiguous contexts *)
  | ID_INITIALLY    { Var_id.of_string "initially" }
  | ID_FINALLY      { Var_id.of_string "finally" }
  (* note: commented out in original spec *)
  (* | ID_NAMED        { Var_id.of_string "named"; } *)
  ;

(* %type <Constructor_id.t> constructor *)
(* constructor: *)
(*   | conid *)
(*   ; *)


(* %type <Constructor_id.t> conid *)
(* conid: *)
(*   | id = CONID *)
(*     { Constructor_id.of_string id } *)
(*   | KIND_E *)
(*     { Constructor_id.of_string "E" } *)
(*   | KIND_X *)
(*     { Constructor_id.of_string "X" } *)
(*   | KIND_V *)
(*     { Constructor_id.of_string "V" } *)
(*   ; *)

(* %type <Operator.t> op *)
(* op: *)
(*   | op = OP *)
(*     { Operator_id.of_string op } *)
(*   | ">" *)
(*     { Operator_id.of_string "<" } *)
(*   | "<" *)
(*     { Operator_id.of_string ">" } *)
(*   | "|" *)
(*     { Operator_id.of_string "|" } *)
(*   | ASSIGN *)
(*     { Operator_id.of_string ":=" } *)
(*   ; *)


(* ---------------------------------------------------------
-- Matching
----------------------------------------------------------*)

(* matchrules: *)
(*   | list(matchrule semi+) *)
(*   ; *)

(* matchrule: *)
(*   | patterns1 "|" expr "->" blockexpr *)
(*   | patterns1 "->" blockexpr *)
(*   ; *)

(* patterns1: *)
(*   | separated_nonempy_list(",", pattern) *)
(*   ; *)

(* apatterns: *)
(*   | separated_list(",", apattern) *)
(*   ; *)

(* %type <annotated_pattern> apattern *)
apattern:
  (* annotated pattern *)
  | pattern = pattern; scheme = annot
    { { pattern; scheme } }
  ;

(* %type <pattern> pattern *)
pattern:
  | id = identifier
    { Pattern_id id }
  (* (* named pattern *) *)
  (* | identifier AS pattern *)
  (* | conid *)
  (* | conid "(" patargs ")" *)
  (* (* unit, parenthesized, and tuple pattern *) *)
  (* | "(" apatterns ")" *)
  (* (* list pattern *) *)
  (* | "[" apatterns "]" *)
  (* | literal *)
  | WILDCARD
    { Pattern_wildcard }
  ;

(* patargs:
  | patargs1 *)
(*  | (\* empty *\) *)
(*  ; *)

(* patargs1:
  | patargs "," patarg *)
(*  | patarg *)
(*  ; *)

(* patarg:
  | identifier "=" apattern            (\* named argument *\) *)
(*  | apattern *)
(*  ; *)


(* ---------------------------------------------------------
-- Handlers
----------------------------------------------------------*)
(* %type <expr> handlerexpr *)
handlerexpr:
  (* [val h = handler { ops }; h(action)] *)
  | HANDLER; handler = opclauses
    { Handler handler }
  (* [handle (action) { ops }] *)
  | HANDLE; subject = ntlexpr; handler = opclauses
    { Handle { subject; handler } }
  ;

(* %type <effect_handler> opclauses *)
opclauses:
  | op = opclause
    { Effect_handler [op] }
  | "{"; semi*; ops = semi_terminated_list(opclausex); "}"
    { Effect_handler ops }
  ;

(* %type <operation_handler> opclausex *)
opclausex:
  (* | ID_FINALLY bodyexpr *)
  (* | ID_INITIALLY bodyexpr *)
  | op = opclause
    { op }
  ;

(* %type <operation_handler> opclause *)
opclause:
  | VAL; id = varid; "="; value = blockexpr
    { Op_val { id; type_ = None; value }}
  | VAL; id = varid; ":"; type_ = type_; "="; value = blockexpr
    { Op_val { id; type_ = Some type_; value }}
  | FUN; id = varid; parameters = opparams; body = bodyexpr
    { Op_fun { id; parameters; body } }
  | EXCEPT; id = varid; parameters = opparams; body = bodyexpr
    { Op_except { id; parameters; body } }
  | CONTROL; id = varid; parameters = opparams; body = bodyexpr
    { Op_control { id; parameters; body } }
  (* | RCONTROL varid opparams bodyexpr *)
  | RETURN; "("; parameter = opparam; ")"; body = bodyexpr
    { Op_return { parameter; body } }
  (* | RETURN paramid bodyexpr               (\* deprecated *\) *)
  ;

(* %type <operation_parameter list> opparams *)
opparams:
  | "("; params = separated_list(",", opparam); ")"
    { params }
  ;

(* %type <operation_parameter> opparam *)
opparam:
  | id = paramid
    { { id; type_ = None } : operation_parameter }
  | id = paramid; ":"; type_ = type_
    { { id; type_ = Some type_ } : operation_parameter }
  ;


(* ---------------------------------------------------------
-- Types
----------------------------------------------------------*)

(* %type <type_parameter list> tbinders *)
tbinders:
  | ts = separated_list(",", tbinder)
    { ts }
  ;

(* %type <type_parameter> tbinder *)
tbinder:
  | id = varid; kind = kannot
    { { id; kind } }
  ;


(* full type *)
(* used for type annotations *)
(* %type <type_scheme> typescheme *)
typescheme:
  | FORALL; forall_quantified = typeparams1; body = tarrow
    { { forall_quantified; body } }
  | body = tarrow
    { { forall_quantified = []; body } }
  (* note: spec allowed `some<>` as well here *)
  ;

(* %type <type_> type_ *)
type_:
  | FORALL; forall_quantified = typeparams1; body = tarrow
    { Scheme { forall_quantified; body } }
  | t = tarrow
    { t }
  ;

(* %type <type_parameter list> typeparams *)
typeparams:
  | ps = typeparams1
    { ps }
  | (* empty *)
    { [] }
  ;

(* TODO: currently discarding the 'nonempty' invariant *)
(* %type <type_parameter list> typeparams1 *)
typeparams1:
  | "<"; ps = tbinders; ">"
    { ps }
  ;

(* 'mono' types *)
(* %type <type_> tarrow *)
tarrow:
  (* TODO: the grammar leaves a lot of decisions until after parsing
     - perhaps move these back into the parser *)
  | source = tatomic; "->"; result = tresult
    { Arrow(source, result) }
  | t = tatomic
    { t }
  ;

(* %type <type_result> tresult *)
tresult:
  (* effect and result type *)
  | effect = tatomic; result = tbasic
    { { effect; result } }
  (* just a result type (with a default total effect) *)
  | result = tatomic
    { { effect = Effect_row total_effect_row; result } }
  ;

(* %type <type_> tatomic *)
tatomic:
  | t = tbasic
    { t }
  (* extensible effect type *)
  | "<"; heads = targuments1; "|"; tail = tatomic; ">"
    { Effect_row (Open(heads, tail)) }
  (* fixed effect type *)
  | "<"; ts = targuments; ">"
    { Effect_row (Closed(ts)) }
  ;

(* %type <type_> tbasic *)
tbasic:
  | t = typeapp
    { t }
  (* note: this below appears to permit named tuple members in types! *)
  (* unit, parenthesis, tuple, named parameters *)
  | "("; ts = tparams; ")"
    { Parameters_or_tuple ts }
  (* TODO: [] as the list type doesn't seem to actually be supported *)
  (* | "[" anntype "]"                (\* list type *\) *)
  ;

(* %type <type_> typeapp *)
typeapp:
  | constructor = typecon
    { Type_atom { constructor ; arguments = [] } }
  | constructor = typecon; "<"; arguments = targuments; ">"
    { Type_atom { constructor ; arguments } }
  ;

(* %type <type_constructor> typecon *)
typecon:
  (* type name *)
  | id = varid
    { Variable_or_name id }
  (* wildcard type variable *)
  | id = WILDCARD
    { Type_wildcard (Wildcard_id.of_string id) }
  | TYPE_INT
    { Type_int }
  | TYPE_BOOL
    { Type_bool }
  (* these unapplied forms don't seem to actually exist *)
  (* | "(" commas1 ")"                (\* tuple constructor *\) *)
  (* | "[" "]"                        (\* list constructor *\) *)
  (* function constructor *)
  (* | "(" "->" ")" *)
  ;


(* %type <parameter_type list> tparams *)
tparams:
  | ps = separated_list(",", tparam)
    { ps }
  ;

(* the `x : int` part of `(x : int, y : int) -> bool` *)
(* %type <parameter_type> tparam *)
tparam:
  (* named parameter *)
  | id = identifier; ":"; type_ = anntype
    { { parameter_id = Some id; type_} }
  | type_ = anntype
    { { parameter_id = None; type_} }
  ;


(* %type <type_ list> targuments *)
targuments:
  | anntypes = targuments1
    { Non_empty_list.to_list anntypes }
  | (* empty *)
    { [] }
  ;

(* %type <type_ Non_empty_list.t> targuments1 *)
targuments1:
  | anntypes = separated_nonempty_list(",", anntype)
    { Non_empty_list.of_list_exn anntypes }
  ;

(* %type <type_> anntype *)
anntype:
  | type_ = type_; kind = kannot
    { match kind with
      | Some kind -> Annotated_type { type_; kind }
      | None      -> type_
    }
  ;


(* ---------------------------------------------------------
-- Kinds
----------------------------------------------------------*)
(* %type <kind option> kannot *)
kannot:
  | "::"; k = kind
    { Some k }
  | (* empty *)
    { None }
  ;

(* %type <kind> kind *)
kind:
  | "("; ks = kinds1; ")"; "->"; a = katom
    { Kind_arrow(ks, (Kind_atom a)) }
  | a = katom; "->"; k = kind
    { Kind_arrow([Kind_atom a], k) }
  | a = katom
    { Kind_atom a }
  ;

(* %type <kind list> kinds1 *)
kinds1:
  | ks = separated_nonempty_list(",", kind)
    { ks }

(* %type <kind_atom> katom *)
katom:
  | KIND_E
    { Kind_effect_row }
  | KIND_X
    { Kind_effect_type }
  | KIND_V
    { Kind_value }
  ;

%%
