module Runtime_error = Runtime_error
module Value = Value

let interpret_program program = Interpreter.run (Eval.eval_program program)
