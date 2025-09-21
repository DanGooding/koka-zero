
fun print-list(xs, print-element) {
  match xs {
    Cons(x, ys) -> {
      print-element(x);
      print-list(ys, print-element)
    };
    _ -> println(())
  }
}

fun print-bool(b) {
  val n = match b {
    False -> 0;
    True -> 1;
  };
  print-int(n);
}

fun main() {
  print-list([1,2,3], print-int);
  print-list([True, False, True], print-bool);
}