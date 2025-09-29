fun reverse(xs) {
  fun reverse-tail(xs, acc) {
    match xs {
      Nil -> acc;
      Cons(x, xx) -> reverse-tail(xx, Cons(x, acc));
    }
  };
  reverse-tail(xs, Nil)
}

fun concat(xs, ys) {
  fun concat-rev(rev-xs, acc) {
    match rev-xs {
      Nil -> acc;
      Cons(x, xx) -> concat-rev(xx, Cons(x, acc));
    }
  };
  concat-rev(reverse(xs), ys)
}

fun flat-map(xs, f) {
  match xs {
    Nil -> Nil;
    Cons(y, ys) -> concat(f(y), ys.flat-map(f));
  }
}

fun map(xs, f) {
  fun map-tail(xs, acc, f) {
    match xs {
      Nil -> reverse(acc);
      Cons(y, ys) -> map-tail(ys, Cons(f(y), acc), f);
    }
  };
  map-tail(xs, Nil, f);
}

fun exists(xs, p) {
  match xs {
    Nil -> False;
    Cons(y, ys) -> {
      if p(y)
      then True
      else ys.exists(p)
    }
  }
}

