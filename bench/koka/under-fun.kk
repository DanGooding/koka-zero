import std/os/readline

effect ctl scramble(x : int) : int

// make `n` recursive calls, then call `action`
fun under-n(n, action) {
  fun worker(m) {
    if m == 0
    then {
      action()
      0
    } else 1 + worker(m - 1) // ensure not tail recursive
  }
  val _ = worker(n)
  ()
}

fun iterate(n, x, f) {
  if n == 0
  then x
  else iterate(n - 1, f(x), f)
}

fun main() {
  val repeats = readline().parse-int().unjust
  // tail-resumptive: don't have to bubble through whole of under-n
  with fun scramble(x) x + 1
  with under-n(1000)
  // scramble `repeats` times
  iterate(repeats, 0, scramble).println()
}
