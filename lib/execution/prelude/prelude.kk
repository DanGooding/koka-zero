// int

fun max(x, y) {
  if x >= y then x else y
}

fun min(x, y) {
  if x <= y then x else y
}

fun compare-int(a, b) {
  a - b
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

fun concat-rev(rev-xs, acc) {
  match rev-xs {
    Nil -> acc;
    Cons(x, xx) -> concat-rev(xx, Cons(x, acc));
  }
}

fun concat(xs, ys) {
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

fun list-equal(xs, ys, element-equal) {
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
        Cons(y, yy) -> element-equal(x, y) && list-equal(xx, yy, element-equal);
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

fun fold-right(xs, init, f) {
  fold(reverse(xs), init, fn(a, b) f(b, a))
}

fun concat-all(lists) {
  fold-right(lists, [], concat)
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

fun compare-reversed(compare) {
  fn(a, b) compare(b, a)
}

fun sort(xs, compare) {
  fun merge(xs, ys, acc) {
    match xs {
      Nil -> concat-rev(acc, ys);
      Cons(x, xx) -> {
        match ys {
          Nil -> concat-rev(acc, xs);
          Cons(y, yy) -> {
            if compare(x, y) <= 0
            then merge(xx, ys, Cons(x, acc))
            else merge(xs, yy, Cons(y, acc))
          }
        }
      }
    }
  };

  fun split(xs, acc1, acc2) {
    match xs {
      Nil -> (acc1, acc2);
      Cons(x, ys) -> {
        match ys {
          Nil -> (Cons(x, acc1), acc2);
          Cons(y, zs) -> split(zs, Cons(x, acc1), Cons(y, acc2));
        }
      }
    }
  };

  fun mergesort(xs) {
    match xs {
      Nil -> xs;
      Cons(_, xx) -> {
        match xx {
          Nil -> xs;
          Cons(_, _) -> {
            // list of two or more items

            val (a, b) = split(xs, [], []);
            val a = mergesort(a);
            val b = mergesort(b);
            
            merge(a, b, [])
          }
        } 
      }
    }
  };

  mergesort(xs)
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

// tuple

fun tuple2-equal((a1, a2), (b1, b2), equal1, equal2) {
  equal1(a1, b1) && equal2(a2, b2)
}

fun tuple2-compare((a1, a2), (b1, b2), compare1, compare2) {
  val cmp1 = compare1(a1, b1);
  if cmp1 == 0
  then compare2(a2, b2)
  else cmp1
}
