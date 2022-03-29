// request an integer in [1,n)
effect control choose-up-to(n : int) : int;

fun is-triple(a, b, c) {
  a * a + b * b == c * c;
};

fun find-triple-below(limit) {
  val c = choose-up-to(limit);
  val b = choose-up-to(c);
  val a = choose-up-to(b);
  is-triple(a, b, c);
};

fun sum-over(start, stop, f) {
  if start >= stop
  then 0
  else f(start) + sum-over(start + 1, stop, f);
};

fun count-triples-below(limit) {
  with handler {
  control choose-up-to(m) {
      sum-over(0, m, resume);
    };
  };
  val is-triple = find-triple-below(limit);
  if is-triple then 1 else 0; // count
};

fun main() {
  read-int(()).count-triples-below.print-int;
};
