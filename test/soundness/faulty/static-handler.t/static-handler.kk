
effect control exn(x : ()) : ();

fun f() {
  with handler {
    control exn(x) {
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

