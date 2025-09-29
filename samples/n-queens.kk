// the chess board is (0,0) to (n-1, n-1)

// are these coordinates on the same row, column, or diagonal?
fun is-attacked-by((x, y), (qx, qy)) {
  x == qx || y == qy || x + y == qx + qy || x - y == qx - qy
}

fun is-attacked-by-any(cell, queens) {
  queens.exists fn(queen) cell.is-attacked-by(queen)
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
