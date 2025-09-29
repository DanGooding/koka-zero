
fun print-list2(xs, print-element) {
  match xs {
    Cons(x, ys) -> {
      print-element(x);
      print-list(ys, print-element)
    };
    _ -> println(())
  }
}

fun print-bool2(b) {
  val n = match b {
    False -> 0;
    True -> 1;
  };
  print-int(n);
}

fun main() {
  print-list2([1,2,3], print-int);
  print-list2([True, False, True], print-bool2);
}