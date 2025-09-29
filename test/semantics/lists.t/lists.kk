fun print-bool(b) {
  match b {
    True -> print-int(1);
    False -> print-int(0);
  }
}

fun main() {
  [1,2,3,4,5,6,7,8,9].filter(fn(i) i % 2 == 0).print-list(print-int)
}