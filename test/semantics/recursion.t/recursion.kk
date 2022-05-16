
fun fib(n) {
  if n == 0
  then 0
  elif n == 1
  then 1
  else fib(n - 1) + fib(n - 2);
}

fun main() {
  println-int(fib(12));
}
