fun map(xs, f) {
  fun map-tail(xs, acc, f) {
    match xs {
      Nil -> reverse(acc);
      Cons(y, ys) -> map-tail(ys, Cons(f(y), acc), f);
    }
  };
  map-tail(xs, Nil, f);
}
