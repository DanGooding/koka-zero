fun loop(n) {
  if n == 0
  then ()
  else loop(n - 1)
}

fun main() {
  loop(read-int(()))
}
