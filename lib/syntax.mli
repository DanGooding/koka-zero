(* TODO: rewrite to use modules? *)

type puredecl

type typedecl =
  | Effect of  * params * kind * opdecls
(* TODO: desugar now or as a separate stage? *)


type topdecl =
  | Puredecl of puredecl
  | Typedecl of typedecl

type program = topdecl list


