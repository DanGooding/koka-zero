
effect control yield-int(x : int) : ();

// yield n, n+1, n+2, ...
fun integers-from(n) {
  yield-int(n);
  integers-from(n + 1);
}

// yield 0, 1, 2, ...
fun naturals() {
  integers-from(0);
}

// yield elements satisfying `p` only
fun filter(p, generate) {
  with control yield-int(x) {
    if p(x) then { yield-int(x); };
    resume(());
  };
  generate();
}


effect state-int {
  control get(_ : ()) : int;
  control set(i : int) : ();
}

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
}

// apply f to state
fun modify(f) {
  get(()).f().set();
}

fun increment() {
  modify fn(x) { x + 1 };
}

fun decrement() {
  modify fn(x) { x - 1 };
}

fun map(f, generate) {
  with fun yield-int(x) {
    yield-int(f(x));
  };
  generate();
}

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
}

fun take-while(p, generate) {
  with control yield-int(x){
    if p(x) then {
      yield-int(x);
      resume(());
    }else {
      ();
    };
  };
  generate();
}

fun fold(x0, f, generate) {
  with control yield-int(x) {
     // TODO: try the state trick for tail-recursion
     f(x0, resume(()));
  };
  generate();
}

// is `p` true for every element?
fun all(p, generate) {
  with control yield-int(x) {
    if p(x)
    then resume(())
    else False
  };
  generate();
  True;
}

// is `p` true for any element?
fun any(p, generate) {
  !all(fn(x) { !p(x); }, generate);
}

fun is-prime(n) {
  if n <= 1 then False
  else {
    with all(fn(d) { n % d != 0 });
    with take-while(fn(d) { d < n });
    integers-from(2);
  };
}

fun print-yielded(generate) {
  with control yield-int(x) {
      print-int(x);
      resume(());
    };
  generate();
}

fun main() {
  with print-yielded;
  with take-while(fn(x) { x < 30 });
  with filter(is-prime);
  naturals();
}


