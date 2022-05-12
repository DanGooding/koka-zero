
fun const(x) {
  fn(_) {
    x
  }
}

effect read {
  control ask(_ : ()) : int;
}

fun main() {
  with handler {
    control ask(_) {
      resume(1)
    };
  };
  val unit = const(())(3);
  ask(()) + ask(unit)
}
