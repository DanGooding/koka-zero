
fun fib(n) {
  if n == 0
  then 0
  else if n == 1
  then 1
  else fib(n - 1) + fib(n - 2);
}

fun for(start, stop, body) {
  if start >= stop
  then ()
  else {
    body(start);
    for(start + 1, stop, body);
    // currently no tail recursion optimisation!
  };
}

fun main() {
  for(0, 25) fn(i) {
    // this is a very inefficient way to list fibonacci numbers!
    print-int(fib(i));
  };
}
