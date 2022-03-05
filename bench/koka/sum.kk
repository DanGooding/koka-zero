
import std/os/readline

fun sum(n) {
  if n <= 0
  then 0
  else 1 + sum(n - 1);
}

fun main() {
  readline().parse-int().unjust().sum().println()
}
