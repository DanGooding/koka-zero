
effect read {
  fun ask(_ : ()) : int;
}

fun add-twice() {
  ask(()) + ask(());  // 21 + 21 = 42
}

fun main() {
  with handler {
    fun ask(_) { 21 };
  };
  print-int(add-twice());
}
