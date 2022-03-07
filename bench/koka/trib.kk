import std/os/readline

fun trib(n) : div int {
  if n == 1
  then 1
  elif n == 2
  then 1
  elif n == 3
  then 2
  else trib(n - 1) + trib(n - 2) + trib(n - 3)
}

fun main() {
  readline().parse-int().unjust().trib().println()
}
