
fun print-list(xs, print-element) {
  match xs {
    Nil -> println();
    Cons(x, ys) -> {
      print-element(x);
      print-list(ys, print-element)
    }
  }
}

fun main() {
  print-list([1,2,3], print-int);
}