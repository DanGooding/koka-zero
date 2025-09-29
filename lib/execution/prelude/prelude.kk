// int

fun max(x, y) {
  if x >= y then x else y
}

// lists

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

fun for-all(xs, p) {
  match xs {
    Nil -> True;
    Cons(y, ys) -> {
      if p(y)
      then ys.for-all(p)
      else False
    }
  }
}

fun list-equal(xs, ys) {
  match xs {
    Nil -> {
      match ys {
        Nil -> True;
        Cons(_, _) -> False;
      }
    };
    Cons(x, xx) -> {
      match ys {
        Nil -> False;
        Cons(y, yy) -> list-equal(xx, yy);
      }
    }
  }
}

fun filter(xs, p) {
  fun filter-tail(xs, acc) {
    match xs {
      Nil -> reverse(acc);
      Cons(y, ys) -> {
        if p(y)
        then filter-tail(ys, Cons(y, acc))
        else filter-tail(ys, acc)
      }
    }
  };
  filter-tail(xs, [])
}

fun filter-map(xs, f) {
  fun filter-map-tail(xs, acc) {
    match xs {
      Nil -> reverse(acc);
      Cons(y, ys) -> {
        match f(y) {
          None    -> filter-map-tail(ys, acc);
          Some(z) -> filter-map-tail(ys, Cons(z, acc));
        }
      }
    }
  };
  filter-map-tail(xs, [])
}

fun flat-map(xs, f) {
  match xs {
    Nil -> Nil;
    Cons(y, ys) -> concat(f(y), ys.flat-map(f));
  }
}

fun fold(xs, init, f) {
  match xs {
    Nil -> init;
    Cons(x, ys) -> fold(ys, f(init, x), f);
  }
}

fun reduce(xs, combine) {
  match xs {
    Nil -> None;
    Cons(x, ys) -> fold(ys, x, combine);
  }
}

fun for-each(xs, f) {
  match xs {
    Nil -> ();
    Cons(y, ys) -> {
      f(y);
      ys.for-each(f)
    }
  }
}

fun range(start, stop) {
  if start >= stop
  then []
  else Cons(start, range(start + 1, stop))
}

fun print-list(xs, print-element) {
  xs.for-each(print-element);
  println(());
}

fun head(xs) {
  match xs {
    Nil -> None;
    Cons(x, _) -> Some(x);
  }
}

// option

fun some-if(cond, value) {
  if cond
  then Some(value)
  else None
}

fun option-map(x, f) {
  match x {
    None -> None;
    Some(y) -> Some(f(y));
  }
}

fun option-flat-map(x, f) {
  match x {
    None -> None;
    Some(y) -> f(y);
  }
}
