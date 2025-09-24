effect fun sqrt(n : int) : option<int>;

fun filter-map(xs, f) {
  match xs {
    Nil -> Nil;
    Cons(x, xx) -> {
      match f(x) {
        None -> xx.filter-map(f);
        Some(y) -> Cons(y, xx.filter-map(f))
      }
    }
  }
}

fun square-root(n) {
  fun go(i) {
    if i * i == n
    then Some(i)
    elif i * i > n
    then None
    else go(i + 1)
  };
  if n < 0
  then None
  else go(0)
}

fun range(start, stop) {
  if start >= stop
  then []
  else Cons(start, range(start + 1, stop))
}

fun for-each(xs, f) {
  match xs {
    Nil -> ();
    Cons(x, xx) -> {
      f(x);
      xx.for-each(f)
    }
  }
}

fun main() {
  val n-and-root = range(1, 100).filter-map fn(n) {
    match square-root(n) {
      None -> None;
      Some(i) -> Some((n, i));
    }
  };

  n-and-root.for-each fn((n, i)) {
    print-int(i);
    println-int(n);
  }
}
