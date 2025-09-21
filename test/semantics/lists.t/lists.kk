
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

fun map(xs, f) {
  match xs {
    Nil -> Nil;
    Cons(x, xx) -> Cons(f(x), xx.map(f));
  }
}

fun filter(xs, f) {
  match xs {
    Nil -> Nil;
    Cons(x, xx) -> {
      if f(x)
      then Cons(x, xx.filter(f))
      else xx.filter(f)
    }
  }
}

fun main() {
  [1,2,3,4,5,6,7,8,9].filter(fn(i) i % 2 == 0).print-list(print-int)
}