effect state-int {
  control get(_ : ()) : int;
  control set(i : int) : ();
}

fun state(init, action) {
  fun thread-through-state() {
    with handler {
      control get(_) { fn(s) { resume(s)(s) } };
      control set(s) { fn(_) { resume(())(s) } };
    };
    val x = action();
    fn(s) { x };
  };
  thread-through-state()(init);
}

// apply f to state
fun modify(f) {
  get(()).f().set();
}

// state += 1
fun increment() {
  modify fn(x) { x + 1 };
}

// state -= 1
fun decrement() {
  modify fn(x) { x - 1 };
}

fun triangular_worker() {
  if (get(()) == 0)
  then 0
  else {
    val current = get(());
    decrement();
    current + triangular_worker()
  }
}

fun triangular(n) {
  with state(n);
  triangular_worker();
}

fun main() {
  triangular(10).println-int
}


