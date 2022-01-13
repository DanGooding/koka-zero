
effect read {
  control ask(x : ()) : int;
};

fun foo() {
  ask(()) + ask(());
};

fun main() {
  with handler {
    control ask(unit) {
      resume(1);
    };
  };
  print-int(foo());
};
