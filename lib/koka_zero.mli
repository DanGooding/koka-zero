(** Reimplementatin of core Koka, compiling to LLVM-IR, for my dissertation *)

(** Returns a greeting message.

    {4 Examples}

    {[ print_endline @@ greet "Jane" ]} *)
val greet : string -> string
