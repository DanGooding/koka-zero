open Monadic_syntax

module Names : sig
  val compose_unary : Variable.t
  val bind : Variable.t
  val pure : Variable.t
  val prompt : Variable.t
  val handler : Variable.t
  val perform : Variable.t
end

(* TODO: or should these be lambda/fix_lambda types? *)
val compose_unary : Expr.fix_lambda Generation.t
val bind : Expr.fix_lambda Generation.t
val compose_kleisli : Expr.fix_lambda Generation.t
val pure : Expr.fix_lambda Generation.t
val prompt : Expr.fix_lambda Generation.t
val handler : Expr.fix_lambda Generation.t

(* val Hnd : ? *)

(* TODO: or a label-indexed family? *)
val select : Expr.fix_lambda Generation.t
val perform : Expr.fix_lambda Generation.t
