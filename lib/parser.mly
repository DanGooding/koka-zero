(*
  Modified from the koka (v2.3.2) grammar specification
  https://github.com/koka-lang/koka/blob/v2.3.2/doc/spec/grammar/parser.y
  the license of which is reproduced below
*)
(* Copyright 2012-2021, Microsoft Research, Daan Leijen
   This is free software; you can redistribute it and/or modify it under the
   terms of the Apache License, Version 2.0.
*)

%token <string> ID CONID (* OP IDOP *)
%token WILDCARD

%token <int> INT
%token <bool> BOOL
(* %token <Float> FLOAT *)
(* %token <string> STRING *)
(* %token <char> CHAR *)

(* string literals define nicer syntax for use in the production rules
   they do not determine when the lexer produces these tokens *)
%token OPEN_ROUND  "(" CLOSE_ROUND  ")"
%token OPEN_SQUARE "[" CLOSE_SQUARE "]"
%token OPEN_CURLY  "{" CLOSE_CURLY  "}"

%token LESS_THAN "<" GREATER_THAN ">"
%token PIPE "|"
%token DOT "."
%token COLON ":"
%token DCOLON "::"
%token COMMA ","
%token SEMI ";"
%token EQUALS "="
%token TILDE "~"
%token EXCLAMATION_MARK "!"

(* TODO: remove tokens not present in any production *)
%token IF THEN ELSE ELIF
%token WITH IN
(* %token MATCH *)
%token RARROW "->" LARROW "<-"

%token FUN FN VAL VAR CONTROL EXCEPT
%token (* TYPE STRUCT *) EFFECT
(* %token ALIAS CON *)
%token FORALL

(* %token INFIX INFIXL INFIXR *)

%token ASSIGN
%token RETURN

%token HANDLER HANDLE (* NAMED MASK *)

%token ID_INITIALLY ID_FINALLY

%token KIND_V KIND_E, KIND_X

(* TODO: check if I need precedence stuff (including some on individual rules) *)
(* precedence declarations are in increasing order,
   i.e. the last precedence declaration has the highest precedence.
*)

(* resolve s/r conflict by shifting on ELSE so the ELSE binds to the closest IF.*)
%nonassoc THEN
%nonassoc ELSE ELIF

(* TODO: should I support this syntax - it makes parsing etc. harder *)
(* resolve s/r conflict to have a `FN funparams -> expr` span as far as possible,
   e.g. `fn(x) -> x + 1` is `(fn(x) -> x + 1)` and not `(fn(x) -> x) + 1`
   and  `fn(x) -> x.foo` is `(fn(x) -> x.foo)` and not `(fn(x) -> x).foo`
   note: we could avoid these rules by disallowing the `->` form in trailing lambdas.
*)
%nonassoc RARROW                     (* -> *)
%nonassoc "(" "[" FN "{" "."         (* applications *)
%nonassoc OP ASSIGN ">" "<" "|"      (* operators *)

(* %prec "?" *)

%start program

%%

(* ---------------------------------------------------------
-- Program
----------------------------------------------------------*)

%type <program> program
program:
  | semi*; ds = declarations; EOF
    { ds }
  ;

semi:
  | ";"
  | INSERTED_SEMI
  ;


(* ---------------------------------------------------------
-- Top level declarations
----------------------------------------------------------*)

%type <toplevel_declaration list> declarations
declarations:
  (* | fixitydecl semi+ declarations *)
  | topdecls
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


%type <toplevel_declaration list> topdecls
topdecls:
  | list(t = topdecl; semi+ { t })
  ;
(* TODO: error recovery? [(topdecl | error) semi+] *)

%type <toplevel_declaration> topdecl
topdecl:
  | p = puredecl { Pure_declaration p }
  (* TODO: keep aliases? *)
  (* | aliasdecl                            { printDecl("alias",$2); } *)
  | t = typedecl { Type_delcaration t }
  ;




(* ---------------------------------------------------------
-- Type declarations
----------------------------------------------------------*)
(* aliasdecl:
  | ALIAS typeid typeparams kannot "=" type_     { Var_id.of_string $2; } *)
(*  ; *)

%type <type_declaration> typedecl
typedecl:
  (* | typemod TYPE typeid typeparams kannot typebody      { Var_id.of_string $3; } *)
  (* | structmod STRUCT typeid typeparams kannot conparams { Var_id.of_string $3; } *)
  | e = effectdecl { Effect_declaration e }
  ;

%type <effect_declaration> effectdecl
effectdecl:
  | EFFECT;
    id = varid;
    type_parameters = typeparams;
    kind_annotation = kannot;
    operations = opdecls
    { { id; type_parameters; kind_annotation; operations } }
  | EFFECT;
    type_parameters = typeparams;
    kind_annotation = kannot;
    operation = operation
    { let id = operation.id in
      let operations = [operation] in
      { id; type_parameters; kind_annotation; operations }
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
  | "(" commas ")"      { Var_id.of_string "(,)"; }       (\* tuples *\) *)
(*  | "[" "]"             { Var_id.of_string "[]"; }        (\* lists *\) *)
(*  | "<" ">"             { Var_id.of_string "<>"; }        (\* total effect *\) *)
(*  | "<" "|" ">"         { Var_id.of_string "<|>"; }       (\* effect extension *\) *)
(*  | varid               { Var_id.of_string $1; } *)
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


%type <operation_declaration list> operations
opdecls:
  | "{"; semi*; operations = operations; "}"
    { operations }
  ;

%type <operation_declaration list> operations
operations:
  | operations = separated_list(semi+, operation) semi+
    { operations }

%type <operation_declaration> operation
operation:
  | VAL; id = varid; type_parameters = typeparams; ":"; result_type = tatomic
    { let shape = Val result_type in
      { id; type_parameters; shape } }
  | FUN;
    id = varid; type_parameters = typeparams;
    "("; parameters = parameters; ")"; ":"; result_type = tatomic
    { let shape = Fun(parameters, result_type) in
      { id; type_parameters; shape } }
  | EXCEPT; id = varid; type_parameters = typeparams;
    "("; parameters = parameters; ")"; ":"; result_type = tatomic
    { let shape = Except(parameters, result_type) in
      { id; type_parameters; shape } }
  | CONTROL; id = varid; type_parameters = typeparams;
    "("; parameters = parameters; ")"; ":"; result_type = tatomic
    { let shape = Control(parameters, result_type) in
      { id; type_parameters; shape } }
  ;


(* ---------------------------------------------------------
-- Pure (top-level) Declarations
----------------------------------------------------------*)
%type <pure_declaration> puredecl
puredecl:
  | VAL; binder = binder; "="; body = blockexpr
    { Val(binder, body) }
  | FUN; f = fundecl
    { Fun f }
  ;

(* TODO: update puredecl to include this? *)
%type <fun_declaration> fundecl
fundecl:
  | id = funid; fn = funbody
    { { id; fn } }
  ;

%type <binder> binder
binder:
  | id = identifier
    { { id; type_ = None } }
  | id = identifier; ":"; type_ = type_
    { { id; type_ } }
  ;

%type <Identifier.t> funid
funid:
  | id = identifier
    { id }
  (* TODO: how can a function name be [,,,]? *)
  (* | "[" commas "]"     { Var_id.of_string "[]"; } *)
  (* TODO: are literals as function names needed? *)
  (* | STRING             { Var_id.of_string $1; } *)
  ;

%type <fn> funbody
funbody:
  | type_parameters = typeparams; "("; parameters = pparameters; ")";
    body = bodyexpr
    { { type_parameters; parameteres; result_type = None; body } }
  | type_parameters = typeparams; "("; parameters = pparameters; ")";
    ":"; result_type = tresult;
    body = block
    { { type_parameters; parameteres; result_type; body } }
  ;

(* annotres??
  | ":" tresult *)
(*  | (\* empty *\) *)
(*  ; *)


(* ---------------------------------------------------------
-- Statements
----------------------------------------------------------*)

(* TODO: error recovery? {statement | error} *)
%type <block> block
block:
  (* must end with an expression statement (and not a declaration) *)
  | "{"; block = blockcontents; "}"
    { block }
  ;

(* merely a helper function for `with` statement desugaring *)
%type <block> blockcontents
blockcontents:
  | semi*;
    statements = separated_list(semi+, statement); semi+;
    last = laststatement; semi*
    { { statements; last } }
  ;

%type <statement> statement
statement:
  | d = decl
    { Declaration d }
  | e = exprstatement
    { Expr e }
  ;

%type <declaration> decl
decl:
  | FUN; f = fundecl
    { Fun f }
  (* local value declaration can use a pattern binding *)
  | VAL; annotated_pattern = apattern; "="; body = blockexpr
    { Val(annotated_patten, body) }
  (* | VAR binder ASSIGN blockexpr   (\* local variable declaration *\) *)
;

%type <expr> exprstatement
exprstatement:
  | e = returnexpr
    { e }
  | e = basicexpr
    { e }
  ;

%type <expr> laststatement
laststatement:
  | e = withstat
    { e }
  (* has unclear semantics, leaving out for now *)
  (* | withstat; IN; blockexpr *)
  | e = exprstatement
    { e }
  ;

(* this grabs the rest of the containing block
   so can only be [laststatement] *)
%type <expr> withstat
withstat:
  | WITH; e = basicexpr; block = blockcontents
    { let callback = anonymous_of_block block in
      insert_with_callback ~callback e
    }
  (* shorthand for handler *)
  | WITH; handler = opclauses; block = blockcontents
    { let callback = anonymous_of_block block in
      Application(Handler handler, callback)
    }
  (* note "=" syntax is deprecated *)
  | WITH; binder = binder; ("<-" | "="); e = basicexpr; block = blockcontents
    { let callback = anonymous_of_bound_block ~binder ~block:body in
      insert_with_callback ~callback e
    }
;

(* ---------------------------------------------------------
-- Expressions
----------------------------------------------------------*)
(** The body of an [if]/[match] branch, or of an anonymous function.
    This may be a block, or a single expression (without braces) *)
%type <block> bodyexpr
bodyexpr:
  | block = blockexpr
    { block }
  ;

%type <block> blockexpr
blockexpr:
  (* a `block` is not interpreted as an anonymous function but as statement
     grouping *)
  | b = block
    { b }
  | e = expr_except_block
    { { statements = []; last = e } }
  ;

%type <expr> expr_except_block
expr_except_block:
  | e = returnexpr
    { e }
  | e = valexpr
    { e }
  | e = basicexpr
    { e }
  ;

%type <expr> expr
expr:
  (* `block` interpreted as an anonymous function *)
  | b = block
    { anonymous_of_block b }
  | e = expr_except_block
    { e }

%type <expr> basicexpr
basicexpr:
  | e = ifexpr
    { e }
  (* | matchexpr *)
  | e = handlerexpr
    { e }
  | e = fnexpr
    { e }
  | opexpr
    %prec RARROW
    { e }
  ;


(* keyword expressions *)

(* matchexpr: *)
(*   | MATCH ntlexpr "{" semi* matchrules "}" *)
(*   ; *)

%type <expr> fnexpr
fnexpr:
  (* anonymous function *)
  | FN; f = funbody
    { Fn f }
  ;

%type <expr> returnexpr
returnexpr:
  | RETURN; e = expr
    { Return e }
  ;

%type <expr> ifexpr
ifexpr:
  | IF; cond = ntlexpr; THEN; a = expr; b = elifs
    { If_then_else(cond, a, b) }
  | IF; cond = ntlexpr; THEN; e = expr
    { If_then(cond, e) }
  | IF; cond = ntlexpr; RETURN; e = expr
    { If_then(cond, Return e) }
  ;

%type <expr> elifs
elifs:
  | ELIF; cond = ntlexpr; THEN; a = expr; b = elifs
    { If_then_else(cond, a, b)  }
  | ELSE; e = expr
    { e }
  ;

%type <expr> valexpr
valexpr:
  | VAL; pat = apattern; "="; e1 = blockexpr; IN; e2 = expr
    { Val_in(pat, e1, e2) }
  ;


(* operator expression *)

%type <expr> opexpr
opexpr:
  | e = opexpr_of(prefixexpr)
    { e }
  ;

(** [opexpr_of] is parameterised by [prefixexpr_]
    the nonterminal which operators are surrounded by.
    This allows sharing between standard and
    non-trailing-lambda usages *)
%type <expr> opexpr_of(prefixexpr)
%type <expr> opexpr_of(ntlprefixexpr)
opexpr_of(prefixexpr_):
  | e = opexpr_r20(prefixexpr_)
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
opexpr_r20(prefixexpr_):
  | el = opexpr_r30(prefixexpr_); "||"; er = opexpr_r20(appexpr_)
    { Binary_op(el, Or, er) }
  | e = opexpr_r30(prefixexpr_)
    { e }
  ;

opexpr_r30(prefixexpr_):
  | el = opexpr_40(prefixexpr_); "&&"; er = opexpr_r30(appexpr_)
    { Binary_op(el, And, er) }
  | e = opexpr_40(prefixexpr_)
    { e }
  ;

%type <binary_operator> op_40
op_n40:
  | "==" { Equals }
  | "!=" { Not_equal }
  | "<=" { Less_equal }
  | ">=" { Greater_equal }
  | "<"  { Less_than }
  | ">"  { Greater_than }
  ;

(* non-associative *)
opexpr_n40(prefixexpr_):
  | el = opexpr_l60(prefixexpr_); op = op_n40; er = opexpr_l60(appexpr_)
    { Binary_op(el, op, er) }
  | e = opexpr_l60(prefixexpr_)
    { e }
  ;

%type <binary_operator> op_l60
op_l60:
  | "+" { Plus }
  | "-" { Minus }
  ;

opexpr_l60(prefixexpr_):
  | el = opexpr_l60(prefixexpr_); op = op_l60; er = opexpr_l70(appexpr_)
    { Binay_op(el, op, er) }
  | e = opexpr_l70(prefixexpr_)
    { e }
  ;

%type <binary_operator> op_l70
op_l70:
  | "*" { Times }
  | "%" { Modulo }
  | "/" { Divide }
  ;

opexpr_l70(prefixexpr_):
  | el = opexpr_l70(prefixexpr_); op = op_l70; er = prefixexpr(appexpr_)
    { Binary_op(el, op, er) }
  | e = prefixexpr_
    { e }
  ;

%type <expr> prefixexpr
prefixexpr:
  | "!"; e = prefixexpr
    { Un_op(Exclamation, e) }
  (* | "~"; e = prefixexpr *)
  (*   { Un_op(?, e) } *)
  | e = appexpr
    %prec RARROW
    { e }
  (* note: unary minus is handled in the lexer:
     [-NUMBER] is always unary, [- ANYTHING] is always binary *)
  ;

%type <expr> appexpr
appexpr:
  | a = auxappexpr
    { match a with
      | `Dot_application(f, arg0) -> Application(f, [arg0])
      | `Application(f, args)
      | `Expr e -> e
    }

%type <
  [ (** a [`Dot_application] can be followed by more bracketed args *)
    `Dot_application of expr * argument
    (** an [`Application] can be followed by a trailing lambda *)
  | `Application of expr * argument list
  | `Expr of expr
  ]> auxappexpr
auxappexpr:
  (* application *)
  | f = auxappexpr; "("; args = arguments; ")"
    { match f with
      | `Dot_application(f', arg0) -> `Application(f', arg0 :: args)
      | `Application(f', args')    -> `Application(Application(f', args'), args)
      | `Expr f'                   -> `Application(f', args)
    }
  (* dot application *)
  | arg0 = appexpr; "."; f = atom
    { `Dot_application(f, arg0) }
  (* trailing function application *)
  | app = auxappexpr; last_arg = trailinglambda
    { match app with
      | `Application(f, args)     -> `Application(f, args @ [last_arg])
      | `Dot_application(f, arg0) -> `Application(f, [arg0; last_arg])
      | `Expr e                   -> `Application(Expr e, [last_arg])
    }
  | e = atom
    { `Expr e }
  ;

%type <expr> trailinglambda
trailinglambda:
  | e = fnexpr
    { e }
  | body = block
    { Fn (anonymous_of_body body) }
  ;


(* non-trailing-lambda expression *)
%type <expr> ntlexpr
ntlexpr:
  | e = ntlopexpr
    { e }
  ;

%type <expr> ntlopexpr
ntlopexpr:
  | e = opexpr_of(ntlprefixexpr)
    { e }
  ;

%type <expr> ntlprefixexpr
ntlprefixexpr:
  (* | "~"; e = ntlprefixexpr *)
  | "!" ntlprefixexpr
    { Unary_op(Exclamation, e) }
  | e = ntlappexpr
    { e }
  ;


%type <
  [ (** a [`Dot_application] can be followed by more bracketed args *)
    `Dot_application of expr * argument
  | `Expr of expr
  ]> ntlappexpr
auxntlappexpr:
  (* application *)
  | f = auxntlappexpr; "("; args = arguments; ")"
    { match f with
      | `Dot_application(f', arg0) -> `Expr(Application(f, arg0 :: args))
      | `Expr e -> `Expr(Application(e, args))
    }
  (* dot application *)
  | arg0 = ntlappexpr; "."; f = atom
    { `Dot_application(f, arg0) }
  | e = atom
    { `Expr e }
  ;

%type <expr> ntlappexpr
ntlappexpr:
  | a = auxntlappexpr
    { match a with
      | `Dot_application(f, arg0) -> Application(f, [arg0])
      | `Expr e -> e
    }

(* atomic expressions *)

%type <expr> atom
atom:
  | id = identifier
    { Identfier id }
  | constructor
  | lit = literal
    { Literal lit }
  (* | mask *)
  | "("; e = aexpr; ")"
    { e }
  (* not yet supported: *)
    (* unit, parenthesized (possibly annotated) expression, tuple expression *)
    (* list expression (elements may be terminated with comma instead of separated) *)

%type <literal> literal
literal:
  | i = INT
    { Int i }
  | b = BOOL
    { Bool b }
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

%type <argument list> arguments
arguments:
  | separated_list(",", argument)
  ;

%type <argument> argument
argument:
  | e = expr
    { e }
  (* | identifier "=" expr                  (\* named arguments *\) *)
  ;

(* parameters: separated by comma, must have a type *)

(* these are used only in constructor definitions *)

%type <parameter list> parameters
parameters:
  | parameters1
  | (* empty *)
  ;

(* TODO: other productions (constructor) require the
   nonempty property, but just [list] is okay for now *)
%type <parameter list> parameters1
parameters1:
  | separated_nonempty_list("," parameter)
  ;

%type <parameter> parameter
parameter:
  | id = paramid; ":"; type_ = paramtype
    { {id; type_} }
  (* | paramid ":" paramtype "=" expr *)
  ;

%type <parameter_id> paramid
paramid:
  | id = identifier
    { Id id }
  | WILDCARD
    { Wildcard }
  ;

%type <type_> paramtype
paramtype:
  | t = type_
    { t }
  (* TODO: is this just for optional parameters? *)
  (* | '?' type_ *)
  ;


(* pattern matching parameters: separated by comma *)

%type <pattern_parameter list> pparameters
pparameters:
  | ps = separated_list(",", pparameter)
    { ps }
  ;

%type <pattern_parameter> pparameter
pparameter:
  | pattern = pattern
    { { pattern; type_ = None } }
  | pattern = pattern; ":"; type_ = paramtype
    { { pattern; type_ } }
  (* pattern=expr - strange form of pattern matching
     seems to be for struct members, or e.g. (fst=1, snd=2) *)
  (* | pattern ":" paramtype "=" expr *)
  (* | pattern "=" expr *)
  ;


(* annotated expressions: separated or terminated by comma *)

%type <expr list>
aexprs:
  | es = separated_list(",", aexpr)
    { es }
  ;

%type <expr>
aexpr:
  | e = expr; a = annot
    { Annotated(e, a) }
  ;

%type <type_scheme> annot
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

%type <Identifier> identifier
identifier:
  | var_id = varid
    { Var var_id }
  (* | IDOP *)
  ;

%type <Var_id.t> varid
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

%type <annotated_pattern> apattern
apattern:
  (* annotated pattern *)
  | pattern = pattern; annotation = annot
    { { pattern; annotation } }
  ;

%type <pattern> pattern
pattern:
  | id = identifier
    { Id id }
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
    { Wildcard }
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
%type <expr> handlerexpr
handlerexpr:
  (* [val h = handler { ops }; h(action)] *)
  | HANDLER; handler = opclauses
    { Handler handler }
  (* [handle (action) { ops }] *)
  | HANDLE; subject = ntlexpr; handler = opclauses
    { Application(Handler handler, subject) }
  ;

%type <handler> opclauses
opclauses:
  | op = opclause
    { [op] }
  | "{"; semi*; ops = list(opclausex semi+); "}"
    { ops }
  ;

%type <operation_handler> opclausex
opclausex:
  (* | ID_FINALLY bodyexpr *)
  (* | ID_INITIALLY bodyexpr *)
  | op = opclause
    { op }
  ;

%type <operation_handler> opclause
opclause:
  | VAL; id = varid; "="; value = blockexpr
    { Val { id; type_ = None; value }}
  | VAL; id = varid; ":"; type_ = type_; "="; value = blockexpr
    { Val { id; type_; value }}
  | FUN; id = varid; parameters = opparams; body = bodyexpr
    { Fun { id; parameters; body } }
  | EXCEPT; id = varid; parameters = opparams; body = bodyexpr
    { Except { id; parameters; body } }
  | CONTROL; id = varid; parameters = opparams; body = bodyexpr
    { Control { id; parameters; body } }
  (* | RCONTROL varid opparams bodyexpr *)
  | RETURN; "("; parameter = opparam; ")"; body = bodyexpr
    { Return { parameter; body } }
  (* | RETURN paramid bodyexpr               (\* deprecated *\) *)
  ;

%type <operation_parameter list> opparams
opparams:
  | "("; params = separated_list(",", opparam); ")"
    { params }
  ;

%type <operation_parameter> opparam
opparam:
  | id = paramid
    { { id; type_ = None } }
  | id = paramid; ":"; type_ = type_
    { { id; type_ } }
  ;


(* ---------------------------------------------------------
-- Types
----------------------------------------------------------*)

%type <type_parameter list> tbinders
tbinders:
  | ts = separated_list(",", tbinder)
    { ts }
  ;

%type <type_parameter> tbinder
tbinder:
  | id = varid; kind = kannot
    { { id; kind } }
  ;


(* full type *)
(* used for type annotations *)
%type <type_scheme> typescheme
typescheme:
  | FORALL; forall_quantified = typeparams1; type_ = tarrow
    { { forall_quanitifed; type_ } }
  | type_ = tarrow
    { { forall_quantified = []; type_ } }
  (* note: spec allowed `some<>` as well here *)
  ;

%type <type_>
type_:
  | FORALL; forall_quantified = typeparams1; type_ = tarrow
    { Scheme { forall_quantified; type_ } }
  | t = tarrow
    { t }
  ;

%type <type_parameter list> typeparams
typeparams:
  | ps = typeparams1
    { ps }
  | (* empty *)
    { [] }
  ;

(* TODO: currently discarding the 'nonempty' invariant *)
%type <type_parameter list> typeparams
typeparams1:
  | "<" tbinders ">"
  ;

(* 'mono' types *)
%type <type_> tarrow
tarrow:
  | "("; ps = tparams; ")"; "->"; result = tresult
    { Arrow(ps, result) }
  (* TODO: I think there will be a shift/reduce conflict here
     for e.g. `(int) -> int` *)
  | p = tatomic; "->"; result = tresult
    { Arrow([p], result) }
  | t = tatomic
    { t }
  ;

%type <type_result> tresult
tresult:
  (* effect and result type *)
  | effect = tatomic; result = tbasic
    { { effect; result } }
  (* just a result type (with a default total effect) *)
  | result = tatomic
    { { effect = total_effect_row; result } }
  ;

%type <type_> tatomic
tatomic:
  | t = tbasic
    { t }
  (* extensible effect type *)
  | "<"; heads = targuments1; "|"; tail = tatomic; ">"
    { Effect_row Open(heads, tail) }
  (* fixed effect type *)
  | "<"; ts = targuments; ">"
    { Effect_row Closed(ts) }
  ;

%type <type_> tbasic
tbasic:
  | t = typeapp
    { t }
  (* kept to allow returning functions (brackets for precedence only) *)
  | "("; t = anntype; ")"
    { t }
  (* note: this below appears to permit named tuple members in types! *)
  (* unit, parenthesis, tuple, named parameters *)
  (* | "(" tparams ")" *)
  (* TODO: [] as the list type doesn't seem to actually be supported *)
  (* | "[" anntype "]"                (\* list type *\) *)
  ;

%type <type_> typeapp
typeapp:
  | t = typecon
    { Atom(t, []) }
  | t = typecon; "<"; args = targuments; ">"
    { Atom(t, args) }
  ;

%type <type_constructor> typecon
typecon:
  (* type name *)
  | id = varid
    { Variable_or_name (Var_id.of_string id) }
  (* wildcard type variable *)
  | id = WILDCARD
    { Wildcard (Var_id.of_string id) }
  | TYPE_INT
    { Int }
  | TYPE_BOOL
    { Bool }
  (* these unapplied forms don't seem to actually exist *)
  (* | "(" commas1 ")"                (\* tuple constructor *\) *)
  (* | "[" "]"                        (\* list constructor *\) *)
  (* function constructor *)
  (* | "(" "->" ")" *)
  ;


%type <parameter_type list> tparams
tparams:
  | ps = separated_list(",", tparam)
    { ps }
  ;

(* the `x : int` part of `(x : int, y : int) -> bool` *)
%type <parameter_type> tparam
tparam:
  (* named parameter *)
  | id = identifier; ":"; anntype = anntype
    { (Some id, anntype) }
  | anntype = anntype
    { (None, anntype) }
  ;


%type <type_ list> targuments1
targuments:
  | anntypes = targuments1
    { anntypes }
  | (* empty *)
    { [] }
  ;

%type <type_ list> targuments1
targuments1:
  | anntypes = separated_nonempy_list(",", anntype)
    { anntypes }
  ;

%type <type_> anntype
anntype:
  | type_ = type_; kind = kannot
    { match kind with
      | Some kind -> Annotated { type_; kind }
      | None      -> type_
    }
  ;


(* ---------------------------------------------------------
-- Kinds
----------------------------------------------------------*)
%type <kind option> kannot
kannot:
  | "::"; k = kind
    { Some k }
  | (* empty *)
    { None }
  ;

%type <kind> kind
kind:
  | "("; ks = kinds1; ")"; "->"; a = katom
    { Arrow ks (Atom a) }
  | a = katom; "->"; k = kind
    { Arrow [Atom a] k }
  | a = katom
    { Atom a }
  ;

%type <kind list> kinds1
kinds1:
  | ks = separated_nonempty_list(",", kind)
    { ks }

%type <kind_atom> katom
katom:
  | KIND_E
    { Effect_row }
  | KIND_X
    { Effect_type }
  | KIND_V
    { Value }
  ;

%%
