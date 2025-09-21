
fun print-list(xs, print-element) {
  match xs {
    Nil -> println(());
    Cons(x, ys) -> {
      print-element(x);
      print-list(ys, print-element)
    }
  }
}

fun print-bool(b) {
  match b {
    True -> print-int(1);
    False -> print-int(0);
  }
}

fun main() {
  print-list([1,2,3], print-int);
  print-list([True, False, True], print-bool);
}