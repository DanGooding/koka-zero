
effect control yield-int(x : int) : ();

// yield n, n+1, n+2, ...
fun naturals-from(n) {
  yield-int(n);
  naturals-from(n + 1);
};

// yield 0, 1, 2, ...
fun naturals() {
  naturals-from(0);
};

// yield elements satisfying `p` only
fun filter(p, generate) {
  with control yield-int(x) {
    if p(x) then { yield-int(x); };
    resume(());
  };
  generate();
};


effect state-int {
  control get(_ : ()) : int;
  control set(i : int) : ();
};

fun state(init, action) {
  fun thread-state() {
    with handler {
      control get(_) { fn(s) { resume(s)(s) } };
      control set(s) { fn(_) { resume(())(s) } };
    };
    val x = action();
    fn(s) { x };
  };
  thread-state()(init);
};

// apply f to state
fun modify(f) {
  get(()).f().set();
};

fun increment() {
  modify fn(x) { x + 1 };
};

fun decrement() {
  modify fn(x) { x - 1 };
};

// yield the first `n` elements only
fun take(n, generate) {
  with state(n);  // remaining to yield
  with control yield-int(x) {
    val remaining = get(());
    if remaining > 0
    then {
      set(remaining - 1);
      yield-int(x);  // propagate
      resume(());
    }else {
       (); // don't yield any further
    }
  };
  generate();
};

fun print-yielded(generate) {
  with handler {
    control yield-int(x) {
      print-int(x);
      resume(());
    };
  };
  generate();
};



fun main() {
  with print-yielded;
  with take(20);
  with filter(fn(x) { x % 2 != 0 });
  naturals();
};


