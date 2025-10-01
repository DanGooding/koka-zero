effect control exn(_ : ()) : ();

fun call-both(f, g) {
  fun call-both-inner(f, g) {
    f();
    g();
  };
  call-both-inner(f, g)
}

fun main() {
  with handler {
    control exn(()) {
      resume(())
    };
  };
  call-both(
    fn() exn(()),
    fn() println-int(5)
  )
}
