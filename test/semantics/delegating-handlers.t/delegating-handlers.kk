
effect read {
  control ask(x : ()) : int;
};

fun const(x, action) {
  with handler {
    control ask(unit) {
      resume(x)
    };
  };
  action();
};

fun increment(action) {
  with handler {
    control ask(unit) {
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
