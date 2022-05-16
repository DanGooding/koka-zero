
// request an integer in [1,n)
effect control choose-up-to(n : int) : int;

fun choose-const(n, action) {
  with handler {
    control choose-up-to(_) {
      resume(n);
    };
  };
  action();
}

fun choose-user(action) {
  with handler {
    control choose-up-to(m) {
      println-int(m);
      resume(read-int(()));
    };
  };
  action();
}

fun for(start, stop, action) {
  if start >= stop
  then ()
  else {
    action(start);
    for(start + 1, stop, action)
  };
}

fun choose-any(action) {
  with handler {
    control choose-up-to(m) {
      for(1, m) fn(i) {
        resume(i);
      };
    };
  };
  action();
}

fun is-triple(a, b, c) {
  a * a + b * b == c * c;
}

fun find-triple(n) {
  val c = choose-up-to(n);
  val b = choose-up-to(c);
  val a = choose-up-to(b);
  if is-triple(a, b, c)
  then { print-int(a); print-int(b); println-int(c); }
  else ();
}

fun main() {
  val limit = 20;
  with choose-any();
  find-triple(limit);
}

