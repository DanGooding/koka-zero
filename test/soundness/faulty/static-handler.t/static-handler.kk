
effect control exn(_ : ()) : ();

fun f() {
  with handler {
    control exn(_) {
      fn() { () }
    };
  };
  fn() {
    // defined under a handler
    exn(())
  }
};

fun main() {
  val g = f();
  // not executed under a handler
  g()
};

