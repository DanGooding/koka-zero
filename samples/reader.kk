
effect read {
  control ask(_ : ()) : int;
}

fun add-twice() {
  ask(()) + ask(());  // 21 + 21 = 42
}

fun main() {
  with handler {
    control ask(_) {
      resume(21);
    };
  };
  print-int(add-twice());
}
