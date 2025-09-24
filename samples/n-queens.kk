// the chess board is (0,0) to (n-1, n-1)

// are these coordinates on the same row, column, or diagonal?
fun is-attacked-by((x, y), (qx, qy)) {
  x == qx || y == qy || x + y == qx + qy || x - y == qx - qy
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

fun is-attacked-by-any(cell, queens) {
  queens.exists fn(queen) cell.is-attacked-by(queen)
}

fun range(start, stop) {
  if start >= stop
  then []
  else Cons(start, range(start + 1, stop))
}

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

fun place-next-queen(x, queens, n) {
  if x >= n
  then [queens]
  else {
    val ys = range(0, n);
    ys.flat-map fn(y) {
      val cell = (x, y);
      if cell.is-attacked-by-any(queens)
      then []
      else place-next-queen(x + 1, Cons(cell, queens), n)
    }
  }
}

fun max(x, y) {
  if x >= y then x else y
}

fun list-max(xs) {
  match xs {
    Nil -> None;
    Cons(y, ys) -> {
      match list-max(ys) {
        None -> Some(y);
        Some(z) -> Some(max(y, z));
      }
    }
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


fun n-queen-arrangements(n) {
  place-next-queen(0, [], n);
}

fun positions-to-board-array(n, positions) {
  range(0, n).map fn(y) {
    range(0, n).map fn(x) {
      val occupied = positions.exists fn((px, py)) px == x && py == y;
      if occupied then 1 else 0
    }
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

fun print-board-array(board) {
  board.for-each fn(row) {
    row.for-each fn(content) print-int(content);
    println(());
  };
  println(());
}

fun main() {
  val n = read-int(());
  val arrangements = n-queen-arrangements(n);
  val board-arrays = arrangements.map fn(positions) positions-to-board-array(n, positions);
  board-arrays.for-each(print-board-array);
}
