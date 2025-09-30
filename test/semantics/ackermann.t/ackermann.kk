fun ackermann(m, n) {
  if m == 0
  then n + 1
  elif n == 0
  then ackermann(m - 1, 1)
  else ackermann(m - 1, ackermann(m, n - 1))
}

fun main() {
  val m = read-int(());
  val n = read-int(());
  ackermann(m, n).println-int();
}
