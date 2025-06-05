open! Core
open! Import

let resume = Variable.of_user "resume"
let main = Variable.of_user "main"
let entry_point = Variable.of_language_internal "main"
let console_effect = Effect.Label.of_string "console"
let println = Variable.of_user "println"
let println_int = Variable.of_user "println-int"
let print_int = Variable.of_user "print-int"
let read_int = Variable.of_user "read-int"
