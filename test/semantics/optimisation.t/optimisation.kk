effect control choose(options : list<int>) : int;

fun choose-all(action) {
  with handler {
    control choose(options) {
      options.flat-map(resume)
    };
  };
  val result = action();
  [result]
}

fun sequence(digits, length) {
  range(0, length).map fn(_) choose(digits)
}

fun all-sequences() {
  with choose-all;
  sequence(range(1, 4), 3)
}

fun main() {
  all-sequences().for-each fn(xs) xs.print-list(print-int)
}
