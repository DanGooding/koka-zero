open Evidence_passing_syntax
open Import

module Names : sig
  val compose_unary : Variable.t
  val kleisli_compose_unary : Variable.t
  val bind : Variable.t
  val pure : Variable.t
  val prompt : Variable.t
  val handler : Variable.t
  val perform : Variable.t
end

(** declarations of the 'primitive' functions *)
val prelude : Program.Fun_decl.t list Generation.t
