val parse_channel
  :  ?filename:string
  -> in_channel
  -> Koka_zero_inference.Minimal_syntax.Program.t
     Koka_zero_util.Or_static_error.t

val parse_string
  :  ?filename:string
  -> string
  -> Koka_zero_inference.Minimal_syntax.Program.t
     Koka_zero_util.Or_static_error.t
