open Import

val ctl_of_value : Value.t -> Value.ctl Interpreter.t
val closure_of_value : Value.t -> Value.closure Interpreter.t
val marker_of_value : Value.t -> Value.Marker.t Interpreter.t
val effect_label_of_value : Value.t -> Effect_label.t Interpreter.t
val op_of_value : Value.t -> Value.op Interpreter.t
val hnd_of_value : Value.t -> Value.hnd Interpreter.t
val evidence_vector_of_value : Value.t -> Value.evidence_vector Interpreter.t
val evidence_of_value : Value.t -> Value.evidence Interpreter.t
val bool_of_value : Value.t -> bool Interpreter.t
val int_of_value : Value.t -> int Interpreter.t
val list_of_value : Value.t -> Value.t list Interpreter.t
val tuple_of_value : Value.t -> Value.t list Interpreter.t

val zip_arguments
  :  params:'a list
  -> args:'b list
  -> ('a * 'b) list Interpreter.t
