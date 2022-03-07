import std/os/readline

effect my-exn {
  control raise() : a
}

fun fib(n) {
  if n < 0
  then raise()
  else if n == 0
  then 0
  else if n == 1
  then 1
  else (fib(n - 1) + fib(n - 2) : int)
}

fun main() {
  with handler control raise() {
    println(-1)
  }
  readline().parse-int().unjust().fib().println()
}
