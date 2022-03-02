
effect read {
  control ask(_ : ()) : int;
};

fun const(x, action) {
  with handler {
    control ask(_) {
      resume(x)
    };
  };
  action();
};

fun increment(action) {
  with handler {
    control ask(_) {
      resume(ask(()) + 1)
    };
  };
  action()
};

fun main() {
  with const(0);
  with increment;
  with increment;
  with increment;
  ask(()).print-int();
};
