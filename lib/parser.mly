(*
  Modified from the koka (v2.3.2) grammar specification
  https://github.com/koka-lang/koka/blob/v2.3.2/doc/spec/grammar/parser.y
  the license of which is reproduced below
*)
(* Copyright 2012-2021, Microsoft Research, Daan Leijen
   This is free software; you can redistribute it and/or modify it under the
   terms of the Apache License, Version 2.0.
*)

(* TODO: wildcard doesn't need id *)
%token <string> ID CONID OP IDOP
%token WILDCARD

%token <int> INT
(* %token <Float> FLOAT *)
/* %token <string> STRING */
/* %token <char> CHAR */

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
%token RARROW LARROW

%token FUN FN VAL VAR CONTROL (* RCONTROL *) EXCEPT
%token (* TYPE STRUCT *) EFFECT
(* %token ALIAS CON *)
(* TODO: I can't see EXISTS ever being used, and SOME is undocumented *)
%token FORALL (* EXISTS SOME *)

(* %token IMPORT AS MODULE *)
(* %token PUBLIC PRIVATE ABSTRACT *)
(* %token EXTERN *)
(* %token INFIX INFIXL INFIXR *)

%token ASSIGN
%token RETURN

%token HANDLER HANDLE (* NAMED MASK *)
(* %token IFACE UNSAFE *)

(* %token ID_CO ID_REC *)
(* %token ID_INLINE ID_NOINLINE *)
(* %token ID_C ID_CS ID_JS ID_FILE *)
(* %token ID_LINEAR ID_OPEN ID_EXTEND *)
(* %token ID_BEHIND *)
(* %token ID_VALUE ID_REFERENCE ID_SCOPED *)
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

%start <Syntax.program> program

%type <Syntax.toplevel_declaration> topdecl
%type <Syntax.pure_declaration> puredecl
%type <Syntax.type_declaration> typedecl
%type <Syntax.effect_declaration> effectdecl
%type <Syntax.operation_declaration> operation

%type <Syntax.statement> statement

%%

(* ---------------------------------------------------------
-- Program
----------------------------------------------------------*)

program:
  | semi* ds = declarations { ds }
  ;

semi:
  | ";"
  | INSERTED_SEMI
  ;


(* ---------------------------------------------------------
-- Top level declarations
----------------------------------------------------------*)

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


topdecls:
  | list(t = topdecl semi+ { t })
  ;
(* TODO: error recovery? [(topdecl | error) semi+] *)

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

typedecl:
  (* | typemod TYPE typeid typeparams kannot typebody      { Var_id.of_string $3; } *)
  (* | structmod STRUCT typeid typeparams kannot conparams { Var_id.of_string $3; } *)
  | e = effectdecl { Effect_declaration e }
  ;

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


opdecls:
  | "{"; semi*; operations = operations; "}"
    { operations }
  ;

operations:
  | operations = separated_list(semi+, operation) semi+
    { operations }

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
puredecl:
  | VAL binder "=" blockexpr      { Var_id.of_string $3; }
  | FUN funid funbody             { Var_id.of_string $3; }
  ;

(* TODO: update puredecl to include this? *)
fundecl:
  | funid funbody                { Var_id.of_string $1; }
  ;

binder:
  | identifier                    { Var_id.of_string $1; }
  | identifier ":" type_           { Var_id.of_string $1; }
  ;

funid:
  | identifier         { Var_id.of_string $1; }
  (* TODO: how can a function name be [,,,]? *)
  (* | "[" commas "]"     { Var_id.of_string "[]"; } *)
  (* TODO: are literals as function names needed? *)
  (* | STRING             { Var_id.of_string $1; } *)
  ;

(* TODO: why does one call bodyexpr, and the other call block? *)
funbody:
  | typeparams "(" pparameters ")" bodyexpr
  | typeparams "(" pparameters ")" ":" tresult block
  ;

(* annotres??
  | ":" tresult *)
(*  | (\* empty *\) *)
(*  ; *)


(* ---------------------------------------------------------
-- Statements
----------------------------------------------------------*)

(* TODO: template for this pattern (shared with operations) *)
block:
  | "{" semi* statements1 "}"    (* must end with an expression statement (and not a declaration) *)
  ;

(* TODO: error recovery? {statement | error} *)
statements1:
  | statements = separated_list(semi+, statement); semi+
    { statements }
  ;

statement:
  (* XXX working here *)
  (* TODO: don't _need_ the wraper type - decl could produce a [statement] *)
  | d = decl { Declaration d }
  | w = withstat { With w }
  | w = withstat; IN; b = blockexpr
    { With_in(w, b) }
  | returnexpr
  | basicexpr
  ;

decl:
  | FUN fundecl
  (* local value declaration can use a pattern binding *)
  | VAL apattern "=" blockexpr
  (* TODO: keep := in ? *)
  (* | VAR binder ASSIGN blockexpr   (\* local variable declaration *\) *)
  ;


(* ---------------------------------------------------------
-- Expressions
----------------------------------------------------------*)
bodyexpr:
  | blockexpr
  ;

blockexpr:
  (* a `block` is not interpreted as an anonymous function but as statement
     grouping *)
  | block
  | expr_except_block
  ;

expr_except_block:
  (* | withexpr *)
  | returnexpr
  | valexpr
  | basicexpr
  ;

expr:
  (* `block` interpreted as an anonymous function *)
  | block
  | expr_except_block

basicexpr:
  | ifexpr
  | matchexpr
  | handlerexpr
  | fnexpr
  | opexpr             %prec RARROW
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
(* TODO: what am I doing about operators? *)

%type <expr> opexpr
opexpr:
  | el = opexpr; op = operator; er = prefixexpr
    { Bin_op(el, op, er) }
  | e = prefixexpr
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
      (* koka allows three syntaxes for passing arguments
         (normal, dot, trailing lambda), any two can be used together
         for a single call, but not all three *)
      | `Dot_application(f', arg0) -> `Expr (Application(f', arg0 :: args))
      | `Application(f', args')    -> `Application(Application(f', args'), args)
      | `Expr f'                   -> `Application(f', args)
    }
  (* | appexpr "[" arguments "]"             (\* index expression *\) *)
  (* dot application *)
  | arg0 = appexpr; "."; f = atom
    { `Dot_application(f, arg0) }

  (* trailing function application *)
  | auxappexpr block
    (* TODO: copy the below case, once work out relation between
       [block] and function body *)
  (* trailing function application *)
  | app = auxappexpr; last_arg = fnexpr
    { match app with
        | `Application(f, args)     -> `Application(f, args @ [last_arg])
        | `Dot_application(f, arg0) -> `Application(f, [arg0])
        | `Expr e                   -> `Application(Expr e, [last_arg])
    }
  | e = atom
    { `Expr e }
  ;

(* non-trailing-lambda expression *)
%type <expr> ntlexpr
ntlexpr:
  | e = ntlopexpr
    { e }
  ;

%type <expr> ntlopexpr
ntlopexpr:
  | el = ntlopexpr; op = operator; er = ntlprefixexpr
    { Binary_op(el, op, er) }
  | e = ntlprefixexpr
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
  (* | ntlappexpr "[" arguments "]"             (\* index expression *\) *)
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
  (* unit, parenthesized (possibly annotated) expression, tuple expression *)
  | "("; es = aexprs; ")"
    { match es with
      | [e] -> e
      | _ -> Tuple es
    }

  (* list expression (elements may be terminated with comma instead of separated) *)
  (* not yet supported *)

%type <literal> literal
literal:
  | i = INT
    { Int i }
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

parameters:
  | parameters1
  | (* empty *)
  ;

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

operator:
  | op
  ;

identifier:
  | varid
  | IDOP
  ;

%type <Var_id.t> varid
varid:
  | id = ID
    { Var_id.of_string id }
  (* allow reserved words to be used as identifiers
     in unambiguous contexts *)
  | ID_C            { Var_id.of_string "c" }
  | ID_CS           { Var_id.of_string "cs" }
  | ID_JS           { Var_id.of_string "js" }
  | ID_FILE         { Var_id.of_string "file" }
  | ID_INLINE       { Var_id.of_string "inline" }
  | ID_NOINLINE     { Var_id.of_string "noinline" }
  | ID_OPEN         { Var_id.of_string "open" }
  | ID_EXTEND       { Var_id.of_string "extend" }
  | ID_LINEAR       { Var_id.of_string "linear" }
  | ID_BEHIND       { Var_id.of_string "behind" }
  | ID_VALUE        { Var_id.of_string "value" }
  | ID_REFERENCE    { Var_id.of_string "reference" }
  | ID_SCOPED       { Var_id.of_string "scoped" }
  | ID_INITIALLY    { Var_id.of_string "initially" }
  | ID_FINALLY      { Var_id.of_string "finally" }
  | ID_REC          { Var_id.of_string "rec" }
  | ID_CO           { Var_id.of_string "co" }
  (* note: commented out in original spec *)
  (* | ID_NAMED        { Var_id.of_string "named"; } *)
  ;

constructor:
  | conid
  ;


%type <Constructor_id.t> conid
conid:
  | id = CONID
    { Constructor_id.of_string id }
  | KIND_E
    { Constructor_id.of_string "E" }
  | KIND_X
    { Constructor_id.of_string "X" }
  | KIND_V
    { Constructor_id.of_string "V" }
  ;

(* TODO: decide whether operators should be special cases, or done like funcitons *)
%type <Operator.t> op
op:
  | op = OP
    { Operator_id.of_string op }
  | ">"
    { Operator_id.of_string "<" }
  | "<"
    { Operator_id.of_string ">" }
  | "|"
    { Operator_id.of_string "|" }
  | ASSIGN
    { Operator_id.of_string ":=" }
  ;


(* ---------------------------------------------------------
-- Matching
----------------------------------------------------------*)
(* TODO: pattern matching removed for now *)

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

(* apattern: *)
(*   (* annotated pattern *) *)
(*   | pattern annot *)
(*   ; *)

pattern:
  | identifier
  (* | identifier AS pattern              (\* named pattern *\) *)
  (* | conid *)
  (* | conid "(" patargs ")" *)
  (* | "(" apatterns ")"                  (\* unit, parenthesized, and tuple pattern *\) *)
  (* | "[" apatterns "]"                  (\* list pattern *\) *)
  (* | literal *)
  | WILDCARD
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
handlerexpr:
  (* handler is apparantly a function, handle is not *)
  | HANDLER witheff opclauses
  (* TODO: have never seen this used *)
  | HANDLE witheff ntlexpr opclauses
  ;

(* TODO: what does an annotation on a handler mean? *)
witheff:
  | "<" anntype ">"
  | (* empty *)
  ;

withstat:
  | WITH basicexpr
  (* shorthand for handler *)
  | WITH witheff opclauses
  | WITH binder LARROW basicexpr
  (* deprecated: *)
  | WITH binder "=" basicexpr
  ;

(* withexpr:
  | withstat IN blockexpr *)
(*  (\* note: already commented out in spec *\) *)
(*  (\* | withstat *\) *)
(*  ; *)

opclauses:
  | opclause
  | "{" semi* list(opclausex semi+) "}"
  ;

opclausex:
  (* | ID_FINALLY bodyexpr *)
  (* | ID_INITIALLY bodyexpr *)
  | opclause
  ;

opclause:
  | VAL identifier "=" blockexpr
  | VAL identifier ":" type_ "=" blockexpr
  | FUN varid opparams bodyexpr
  | EXCEPT varid opparams bodyexpr
  | CONTROL varid opparams bodyexpr
  (* | RCONTROL varid opparams bodyexpr *)
  | RETURN "(" opparam ")" bodyexpr
  (* | RETURN paramid bodyexpr               (\* deprecated *\) *)
  ;

opparams:
  | "(" separated_list(",", opparam) ")"
  ;

opparam:
  | paramid
  | paramid ":" type_
  ;


(* ---------------------------------------------------------
-- Types
----------------------------------------------------------*)
tbinders:
  | separated_list(",", tbinder)
  ;

tbinder:
  | varid kannot
  ;


(* full type *)
typescheme:
  | someforalls tarrow         (* used for type annotations *)
  ;

type_:
  | FORALL typeparams1 tarrow
  | tarrow
  ;

someforalls:
  | FORALL typeparams1
  | (* empty *)
  ;

typeparams:
  | typeparams1
  | (* empty *)
  ;

typeparams1:
  | "<" tbinders ">"
  ;

(* mono types *)
tarrow:
  | tatomic "->" tresult
  | tatomic
  ;

tresult:
  (* effect and result type *)
  | tatomic tbasic
  (* just a result type (with a default total effect) *)
  | tatomic
  ;

tatomic:
  | tbasic
  (* extensible effect type *)
  | "<" targuments1 "|" tatomic ">"
  (* fixed effect type *)
  | "<" targuments ">"
  ;

tbasic:
  | typeapp
  (* unit, parenthesis, tuple, named parameters *)
  | "(" tparams ")"
  (* TODO: [] as the list type doesn't seem to actually be supported *)
  (* | "[" anntype "]"                (\* list type *\) *)
  ;

typeapp:
  | typecon
  | typecon "<" targuments ">"
  ;

typecon:
  (* type name *)
  | varid
  (* wildcard type variable *)
  | WILDCARD
  (* TODO: I think the (,,,)<a,b,c> form isn't needed *)
  (* | "(" commas1 ")"                (\* tuple constructor *\) *)
  (* | "[" "]"                        (\* list constructor *\) *)
  (* function constructor *)
  | "(" "->" ")"
  ;


tparams:
  | separated_list(",", tparam)
  ;

tparam:
  (* named parameter *)
  | identifier ":" anntype
  | anntype
  ;


targuments:
  | targuments1
  | (* empty *)
  ;

targuments1:
  | separated_nonempy_list(",", anntype)
  ;

anntype:
  | type_ kannot
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
