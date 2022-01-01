open Koka_zero_util

val infer_type
  :  Minimal_syntax.Program.t
  -> (Type.Mono.t * Effect.t) Or_static_error.t
