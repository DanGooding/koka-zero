fun four-times(f, x) {
  fun twice(g) {
    fn (y) g(g(y))
  };
  // use twice polymorphically
  twice(fn(z) z)(True);
  twice(twice(f))(x)
}

fun main() {
  four-times(fn (x) x * 10, 3).println-int
}
