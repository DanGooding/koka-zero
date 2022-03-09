open Import
open Evidence_passing_syntax

(** a lambda's free variables *)
val free_in_lambda : Expr.lambda -> Variable.Set.t

(** a [fix_lambda]'s free varaibles *)
val free_in_fix_lambda : Expr.fix_lambda -> Variable.Set.t
