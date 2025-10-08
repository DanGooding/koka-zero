effect control exn(_ : ()) : ();

fun call-both() {
  fun call-both-inner(f, g) {
    f();
    g();
  };
  call-both-inner(
    fn() exn(()),
    fn() println-int(3)
  )
}

fun main() {
  with handler {
    control exn(()) {
      resume(())
    };
  };
  call-both()
}
