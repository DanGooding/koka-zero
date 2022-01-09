val ctl_of_value : Value.t -> Value.ctl Interpreter.t
val closure_of_value : Value.t -> Value.closure Interpreter.t
val marker_of_value : Value.t -> Value.Marker.t Interpreter.t
val bool_of_value : Value.t -> bool Interpreter.t
val int_of_value : Value.t -> int Interpreter.t

val zip_arguments
  :  params:'a list
  -> args:'b list
  -> ('a * 'b) list Interpreter.t
