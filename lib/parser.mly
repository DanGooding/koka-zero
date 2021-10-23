(*
  Modified from the koka grammar specification
  https://github.com/koka-lang/koka/blob/master/doc/spec/grammar/parser.y
  the license of which is reproduced below
*)
(* Copyright 2012-2021, Microsoft Research, Daan Leijen
   This is free software; you can redistribute it and/or modify it under the
   terms of the Apache License, Version 2.0.
*)


(* token structure. The memory for Id and String is kept in a list and deallocated after parsing (in 'doneScanState') *)
%union {
  const char*   Id;      (* used for operators OP too *)
  const char*   String;  (* 'modified' UTF-8 string (\0 chars are encoded as \xC0\x80) *)
  double        Float;
  unsigned long Nat;
  unsigned int  Char;
}


(* TODO: why are brackets of type Id? what about keywords? (maybe a C thing) *)
%token <Id>     ID CONID OP IDOP WILDCARD '(' ')' '[' ']'
(* TODO: rename nat->int? *)
%token <Nat>    NAT
%token <Float>  FLOAT
%token <String> STRING
%token <Char>   CHAR

(* note: commented out in spec *)
(* %token APP  *) (* '(' for applications *)
(* %token IDX  *) (* '[' for indexing *)

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

(* TODO: why is whitespace here? - they are never used... *)
%token LEX_WHITE LEX_COMMENT
%token INSERTED_SEMI
%token LE ASSIGN DCOLON EXTEND
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

%type <Id>  varid conid qvarid qconid op
%type <Id>  identifier operator constructor
%type <Id>  funid typeid modulepath binder
%type <Id>  fundecl aliasdecl typedecl externdecl puredecl

(* TODO: check if I need precedence stuff (including some on individual rules) *)
(* precedence declarations are in increasing order,
   i.e. the last precedence declaration has the highest precedence.
*)

(* resolve s/r conflict by shifting on ELSE so the ELSE binds to the closest IF.*)
%precedence THEN
%precedence ELSE ELIF

(* TODO: should I support this syntax - it makes parsing etc. harder *)
(* resolve s/r conflict to have a `FN funparams -> expr` span as far as possible,
   e.g. `fn(x) -> x + 1` is `(fn(x) -> x + 1)` and not `(fn(x) -> x) + 1`
   and  `fn(x) -> x.foo` is `(fn(x) -> x.foo)` and not `(fn(x) -> x).foo`
   note: we could avoid these rules by disallowing the `->` form in trailing lambdas.
*)
%precedence RARROW                     (* -> *)
%precedence '(' '[' FN '{' '.'         (* applications *)
%precedence OP ASSIGN '>' '<' '|'      (* operators *)

(* %precedence '?' *)

%%

(* ---------------------------------------------------------
-- Program
----------------------------------------------------------*)

(* TODO: eliminate trivial productions like this *)
program     : semis declarations
            ;

(* TODO: rewrite using */+ *)
semis1      : semis1 semi
            | semi
            ;

semis       : semis semi
            | (* empty *)
            ;

semi        : ';'
            | INSERTED_SEMI
            ;


(* ---------------------------------------------------------
-- Top level declarations
----------------------------------------------------------*)

declarations:
            (* | fixitydecl semis1 declarations *)
            | topdecls
            ;

(* fixitydecl  : fixity oplist1 *)
(*             ; *)

(* fixity      : INFIX NAT *)
(*             | INFIXR NAT *)
(*             | INFIXL NAT *)
(*             ; *)

(* oplist1     : oplist1 ',' identifier *)
(*             | identifier *)
(*             ; *)


topdecls    : topdecls1
            | (* empty *)
            ;

topdecls1   : topdecls1 topdecl semis1
            | topdecl semis1
            (* TODO: keep 'error recovery' ? *)
            (* error recovery *)
            (* | topdecls1 error semis1 *)
            (* | error semis1                                    { yyerror(&@1,scanner,"skipped top-level declaration");  } *)
            ;

topdecl     : puredecl                             { printDecl("value",$2); }
            (* TODO: keep aliases? *)
            (* | aliasdecl                            { printDecl("alias",$2); } *)
            | typedecl                             { printDecl("type",$2); }
            ;




(* ---------------------------------------------------------
-- Type declarations
----------------------------------------------------------*)
(* aliasdecl   : ALIAS typeid typeparams kannot '=' type_     { $$ = $2; } *)
(*             ; *)

typedecl    :
            (* | typemod TYPE typeid typeparams kannot typebody      { $$ = $3; } *)
            (* | structmod STRUCT typeid typeparams kannot conparams { $$ = $3; } *)
            | EFFECT varid typeparams kannot opdecls          { $$ = $3; }
            | EFFECT typeparams kannot operation              { $$ = "<operation>"; }
            ;

(* typemod     : structmod *)
(*             ; *)

(* structmod   : *)
(*             (\* | ID_VALUE *\) *)
(*             (\* | ID_REFERENCE *\) *)
(*             | (\* empty *\) *)
(*             ; *)

(* typebody    : '{' semis constructors '}' *)
(*             | (\* empty *\) *)
(*             ; *)

(* typeid      : '(' commas ')'      { $$ = "(,)"; }       (\* tuples *\) *)
(*             | '[' ']'             { $$ = "[]"; }        (\* lists *\) *)
(*             | '<' '>'             { $$ = "<>"; }        (\* total effect *\) *)
(*             | '<' '|' '>'         { $$ = "<|>"; }       (\* effect extension *\) *)
(*             | varid               { $$ = $1; } *)
(*             ; *)

(* TODO: rewrite commas to use +/* *)
commas      : commas1
            | (* empty *)
            ;

commas1     : commas ','
            ;


(* constructors: constructors1 semis1 *)
(*             | (\* empty *\) *)
(*             ; *)

(* constructors1: constructors1 semis1 constructor *)
(*             | constructor *)
(*             ; *)

(* constructor : con conid typeparams conparams *)
(*             | con STRING typeparams conparams *)
(*             ; *)

(* con         : CON *)
(*             | (\* empty *\) *)
(*             ; *)

(* conparams   : '(' parameters1 ')'          (\* deprecated *\) *)
(*             | '{' semis sconparams '}' *)
(*             | (\* empty *\) *)
(*             ; *)

(* sconparams  : sconparams parameter semis1 *)
(*             | (\* empty *\) *)
(*             ; *)


(* ---------------------------------------------------------
-- Effect declarations
----------------------------------------------------------*)


opdecls     : '{' semis operations '}'
            ;

operations  : operations operation semis1
            | (* empty *)
            ;

operation   : VAL identifier typeparams ':' tatomic
            | FUN identifier typeparams '(' parameters ')' ':' tatomic
            | EXCEPT identifier typeparams '(' parameters ')' ':' tatomic
            | CONTROL identifier typeparams '(' parameters ')' ':' tatomic
            ;


(* ---------------------------------------------------------
-- Pure (top-level) Declarations
----------------------------------------------------------*)
puredecl    : VAL binder '=' blockexpr      { $$ = $3; }
            | FUN funid funbody             { $$ = $3; }
            ;

(* TODO: update puredecl to include this? *)
fundecl     : funid funbody                { $$ = $1; }
            ;

binder      : identifier                    { $$ = $1; }
            | identifier ':' type_           { $$ = $1; }
            ;

funid       : identifier         { $$ = $1; }
            (* TODO: how can a function name be [,,,]? *)
            (* | '[' commas ']'     { $$ = "[]"; } *)
            (* TODO: are literals as function names needed? *)
            (* | STRING             { $$ = $1; } *)
            ;

(* TODO: why does one call bodyexpr, and the other call block? *)
funbody     : typeparams '(' pparameters ')' bodyexpr
            | typeparams '(' pparameters ')' ':' tresult qualifier block
            ;

(* annotres    : ':' tresult *)
(*             | (\* empty *\) *)
(*             ; *)


(* ---------------------------------------------------------
-- Statements
----------------------------------------------------------*)

block       : '{' semis statements1 '}'    (* must end with an expression statement (and not a declaration) *)
            ;

(* TODO: rewrite to use */+ *)
statements1 : statements1 statement semis1
            | statement semis1
            (* TODO: errors? *)
            (* | error semis1 *)
            ;

statement   : decl
            | withstat
            | withstat IN blockexpr
            | returnexpr
            | basicexpr
            ;

decl        : FUN fundecl
            | VAL apattern '=' blockexpr    (* local value declaration can use a pattern binding *)
            (* TODO: keep := in ? *)
            (* | VAR binder ASSIGN blockexpr   (\* local variable declaration *\) *)
            ;


(* ---------------------------------------------------------
-- Expressions
----------------------------------------------------------*)
bodyexpr    : blockexpr
            (* | RARROW blockexpr  (\* deprecated *\) *)
            ;

blockexpr   : expr              (* a `block` is not interpreted as an anonymous function but as statement grouping *)
            ;

expr        :
            (* | withexpr *)
            | block             (* interpreted as an anonymous function (except if coming from `blockexpr`) *)
            | returnexpr
            | valexpr
            | basicexpr
            ;

basicexpr   : ifexpr
            | matchexpr
            | handlerexpr
            | fnexpr
            | opexpr             %prec RARROW
            ;


(* keyword expressions *)

(* TODO: not yet supporting match? *)
matchexpr   : MATCH ntlexpr '{' semis matchrules '}'
            ;

fnexpr      : FN funbody                     (* anonymous function *)
            ;

returnexpr  : RETURN expr
            ;

ifexpr      : IF ntlexpr THEN expr elifs
            | IF ntlexpr THEN expr
            | IF ntlexpr RETURN expr
            ;

elifs       : ELIF ntlexpr THEN expr elifs
            | ELSE expr
            ;

valexpr     : VAL apattern '=' blockexpr IN expr
            ;


(* operator expression *)
(* TODO: what am I doing about operators? *)

opexpr      : opexpr operator prefixexpr
            | prefixexpr
            ;

prefixexpr  : '!' prefixexpr
            | '~' prefixexpr
            | appexpr               %prec RARROW
            ;

appexpr     : appexpr '(' arguments ')'             (* application *)
            (* | appexpr '[' arguments ']'             (\* index expression *\) *)
            | appexpr '.' atom                      (* dot application *)
            | appexpr block                         (* trailing function application *)
            | appexpr fnexpr                        (* trailing function application *)
            | atom
            ;


(* non-trailing-lambda expression *)
ntlexpr     : ntlopexpr
            ;

ntlopexpr   : ntlopexpr operator ntlprefixexpr
            | ntlprefixexpr
            ;

ntlprefixexpr: '!' ntlprefixexpr
            | '~' ntlprefixexpr
            | ntlappexpr
            ;

ntlappexpr  : ntlappexpr '(' arguments ')'             (* application *)
            (* | ntlappexpr '[' arguments ']'             (\* index expression *\) *)
            | ntlappexpr '.' atom                      (* dot application *)
            | atom
            ;

(* atomic expressions *)

atom        : identifier
            | constructor
            | literal
            | mask
            | '(' aexprs ')'             (* unit, parenthesized (possibly annotated) expression, tuple expression *)
            | '[' cexprs ']'             (* list expression (elements may be terminated with comma instead of separated) *)
            ;

literal     : NAT | FLOAT | CHAR | STRING
            ;

(* mask        : MASK behind '<' tbasic '>' *)
(*             ; *)

(* behind      : ID_BEHIND *)
(*             | (\* empty *\) *)
(*             ; *)

(* arguments: separated by comma *)


(* TODO: rewrite to use separated_list *)
arguments   : arguments1
            | (* empty *)
            ;

arguments1  : arguments1 ',' argument
            | argument
            ;

argument    : expr
            (* | identifier '=' expr                  (\* named arguments *\) *)
            ;

(* parameters: separated by comma, must have a type *)

(* TODO: rewrite to use separated_list *)
parameters  : parameters1
            | (* empty *)
            ;

parameters1 : parameters1 ',' parameter
            | parameter
            ;

parameter   : paramid ':' paramtype
            (* | paramid ':' paramtype '=' expr *)
            ;

paramid     : identifier
            | WILDCARD
            ;

paramtype   : type_
            (* TODO: is this just for optional parameters? *)
            (* | '?' type_ *)
            ;


(* pattern matching parameters: separated by comma *)

(* TODO: rewrite to use separated_list *)
pparameters : pparameters1
            | (* empty *)
            ;

pparameters1: pparameters1 ',' pparameter
            | pparameter
            ;

pparameter  : pattern
            | pattern ':' paramtype
            (* TODO: pattern=expr - strange form of pattern matching
               seems to be for struct members, or e.g. (fst=1, snd=2) *)
            (* | pattern ':' paramtype '=' expr *)
            (* | pattern '=' expr *)
            ;


(* annotated expressions: separated or terminated by comma *)

(* TODO: rewrite to use lists! *)
aexprs      : aexprs1                               (* separated by comma *)
            | (* empty *)
            ;

aexprs1     : aexprs1 ',' aexpr
            | aexpr
            ;

(* TODO: what? - surely these are different cases? *)
cexprs      : cexprs0                              (* terminated or separated by comma *)
            | cexprs0 aexpr
            ;

cexprs0     : cexprs0 aexpr ','
            | (* empty *)
            ;

aexpr       : expr annot
            ;

annot       : ':' typescheme
            | (* empty *)
            ;



(* ---------------------------------------------------------
-- Identifiers and operators
----------------------------------------------------------*)

operator    : op
            ;

identifier  : varid
            | IDOP
            ;


varid       : ID
            (* allow reserved words to be used as identifiers
               in unambiguous contexts *)
            | ID_C            { $$ = "c"; }
            | ID_CS           { $$ = "cs"; }
            | ID_JS           { $$ = "js"; }
            | ID_FILE         { $$ = "file"; }
            | ID_INLINE       { $$ = "inline"; }
            | ID_NOINLINE     { $$ = "noinline"; }
            | ID_OPEN         { $$ = "open"; }
            | ID_EXTEND       { $$ = "extend"; }
            | ID_LINEAR       { $$ = "linear"; }
            | ID_BEHIND       { $$ = "behind"; }
            | ID_VALUE        { $$ = "value"; }
            | ID_REFERENCE    { $$ = "reference"; }
            | ID_SCOPED       { $$ = "scoped"; }
            | ID_INITIALLY    { $$ = "initially"; }
            | ID_FINALLY      { $$ = "finally"; }
            | ID_REC          { $$ = "rec"; }
            | ID_CO           { $$ = "co"; }
            (* note: commented out in original spec *)
            (* | ID_NAMED        { $$ = "named"; } *)
            ;

constructor : conid
            ;


conid       : CONID  { $$ = $1; }
            ;

(* TODO: decide whether operators should be special cases, or done like funcitons *)
op          : OP
            | '>'       { $$ = ">";  }
            | '<'       { $$ = "<";  }
            | '|'       { $$ = "|";  }
            (* | ASSIGN    { $$ = ":="; } *)
            ;


(* ---------------------------------------------------------
-- Matching
----------------------------------------------------------*)
(* TODO: pattern matching removed for now *)

matchrules  : matchrules1 semis1
            | (* empty *)
            ;

matchrules1 : matchrules1 semis1 matchrule
            | matchrule
            ;

matchrule   : patterns1 '|' expr RARROW blockexpr
            | patterns1 RARROW blockexpr
            ;

(* TODO: are the commas to form a tuple, or is this
  just syntax for multiple pattern matching? *)
patterns1   : patterns1 ',' pattern
            | pattern
            ;

apatterns   : apatterns1
            | (* empty *)
            ;

apatterns1  : apatterns1 ',' apattern
            | apattern
            ;

apattern    : pattern annot                    (* annotated pattern *)
            ;

pattern     : identifier
            (* | identifier AS pattern              (\* named pattern *\) *)
            (* | conid *)
            (* | conid '(' patargs ')' *)
            (* | '(' apatterns ')'                  (\* unit, parenthesized, and tuple pattern *\) *)
            (* | '[' apatterns ']'                  (\* list pattern *\) *)
            (* | literal *)
            | WILDCARD
            ;

(* patargs     : patargs1 *)
(*             | (\* empty *\) *)
(*             ; *)

(* patargs1    : patargs ',' patarg *)
(*             | patarg *)
(*             ; *)

(* patarg      : identifier '=' apattern            (\* named argument *\) *)
(*             | apattern *)
(*             ; *)


(* ---------------------------------------------------------
-- Handlers
----------------------------------------------------------*)
handlerexpr : HANDLER witheff opclauses
            | HANDLE witheff ntlexpr opclauses
            ;

(* TODO: what does an annotation on a handler mean? *)
witheff     : '<' anntype '>'
            | (* empty *)
            ;

withstat    : WITH basicexpr
            | WITH witheff opclauses    (* shorthand for handler *)
            | WITH binder LARROW basicexpr
            (* TODO: support the old syntax? *)
            (* deprecated: *)
            | WITH binder '=' basicexpr
            ;

(* withexpr    : withstat IN blockexpr *)
(*             (\* note: already commented out in spec *\) *)
(*             (\* | withstat *\) *)
(*             ; *)

(* TODO: rewrite to use lists *)
opclauses   : opclause
            | '{' semis opclauses1 semis1 '}'
            | '{' semis '}'
            ;

opclauses1  : opclauses1 semis1 opclausex
            | opclausex
            ;

opclausex   :
            (* | ID_FINALLY bodyexpr *)
            (* | ID_INITIALLY bodyexpr *)
            | opclause
            ;

opclause    : VAL identifier '=' blockexpr
            | VAL identifier ':' type_ '=' blockexpr
            | FUN identifier opparams bodyexpr
            | EXCEPT identifier opparams bodyexpr
            | CONTROL identifier opparams bodyexpr
            (* | RCONTROL identifier opparams bodyexpr *)
            | RETURN '(' opparam ')' bodyexpr
            (* | RETURN paramid bodyexpr               (\* deprecated *\) *)
            ;

opparams    : '(' opparams0 ')'
            ;

opparams0   : opparams1
            | (* empty *)
            ;

opparams1   : opparams1 ',' opparam
            | opparam
            ;

opparam     : paramid
            | paramid ':' type_
            ;


(* ---------------------------------------------------------
-- Types
----------------------------------------------------------*)
(* TODO: rewrite to use lists *)
tbinders    : tbinders1
            | (* empty *)
            ;

tbinders1   : tbinders1 ',' tbinder
            | tbinder
            ;

tbinder     : varid kannot
            ;


(* full type *)
typescheme  : someforalls tarrow qualifier        (* used for type annotations *)
            ;

type_       : FORALL typeparams1 tarrow qualifier
            | tarrow qualifier
            ;

someforalls :
            | FORALL typeparams1
            | (* empty *)
            ;

typeparams  : typeparams1
            | (* empty *)
            ;

typeparams1 : '<' tbinders '>'
            ;

(* mono types *)
tarrow      : tatomic RARROW tresult
            | tatomic
            ;

tresult     : tatomic tbasic                 (* effect and result type *)
            | tatomic                        (* just a result type (with a default total effect) *)
            ;

tatomic     : tbasic
            | '<' targuments1 '|' tatomic '>' (* extensible effect type *)
            | '<' targuments '>'             (* fixed effect type *)
            ;

tbasic      : typeapp
            | '(' tparams ')'                (* unit, parenthesis, tuple, named parameters *)
            (* TODO: [] as the list type doesn't seem to actually be supported *)
            (* | '[' anntype ']'                (\* list type *\) *)
            ;

typeapp     : typecon
            | typecon '<' targuments '>'
            ;

typecon     : varid | qvarid                 (* type name *)
            | WILDCARD                       (* wildcard type variable *)
            (* TODO: I think the (,,,)<a,b,c> form isn't needed *)
            (* | '(' commas1 ')'                (\* tuple constructor *\) *)
            (* | '[' ']'                        (\* list constructor *\) *)
            | '(' RARROW ')'                 (* function constructor *)
            ;


tparams     : tparams1
            | (* empty *)
            ;

tparams1    : tparams1 ',' tparam
            | tparam
            ;

tparam      : identifier ':' anntype              (* named parameter *)
            | anntype
            ;


targuments  : targuments1
            | (* empty *)
            ;

targuments1 : targuments1 ',' anntype
            | anntype
            ;

anntype     : type_ kannot
            ;


(* ---------------------------------------------------------
-- Kinds
----------------------------------------------------------*)
kannot      : DCOLON kind
            | (* empty *)
            ;

kind        : '(' kinds1 ')' RARROW katom
            | katom RARROW kind
            | katom
            ;

kinds1      : kinds1 ',' kind
            | kind
            ;

katom       : conid
            ;

%%

