fun foo(f, g, h, x) {
  val z = if (f(g(x)) > 0) 
  then h(0)(1)
  else h(1)(0);
  h(z)(z)
}

fun main() {
  foo(fn(y) { y * y }, fn(x) { x + x }, fn(m) { fn (n) { m - n }}, 3).println-int
}