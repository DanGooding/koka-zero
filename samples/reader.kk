
effect read {
  control ask(x : ()) : int;
};

fun foo() {
  ask(()) + ask(());  // 21 + 21 = 42
};

fun main() {
  with handler {
    control ask(unit) {
      resume(21);
    };
  };
  print-int(foo());
};
