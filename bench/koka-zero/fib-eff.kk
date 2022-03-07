
effect exn {
  control throw(_ : ()) : ();
};

fun fib(n) {
  if n < 0
  then { throw(()); 0 }
  else if n == 0
  then 0
  else if n == 1
  then 1
  else fib(n - 1) + fib(n - 2);
};

fun main() {
  with handler control throw(_) {
    print-int(-1);
  };
  read-int(()).fib().print-int();
};

