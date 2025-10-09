
// request an integer in [1,n)
effect control choose-up-to(n : int) : int;

fun all-solutions(action) {
  with control choose-up-to(m) {
    range(1, m).flat-map fn(i) {
      resume(i)
    };
  };
  match action() {
    None -> [];
    Some(result) -> [result];
  }
}

fun is-triple(a, b, c) {
  a * a + b * b == c * c;
}

fun find-triple(n) {
  val c = choose-up-to(n);
  val b = choose-up-to(c);
  val a = choose-up-to(b);
  if is-triple(a, b, c)
  then Some((a, b, c))
  else None
}

fun print-triple((a, b, c)) {
  print-int(a); print-int(b); println-int(c);
}

fun main() {
  val limit = read-int(());
  val triples = all-solutions fn() find-triple(limit);
  triples.for-each(print-triple);
}

