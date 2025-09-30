// Cooperative multithreading - yield allows another task to run
effect control yield(_ : ()) : ();

// Prints a list of values, yielding after each
fun print-each(xs) {
  match xs {
    Nil -> ();
    Cons(x, xx) -> {
      println-int(x);
      yield(());
      print-each(xx)
    }
  }
}

// Turns a yielding function: () -> <yield> ()
// into a function that returns new 'next-step' function each time it yields.
// This has the recursive type: rec<step, () -> option<step> >
fun to-steppable(task) {
  fn() {
    with handler {
      control yield(()) {
        Some(fn() resume(()))
      };
    };
    task();
    None
  }
}

fun run-to-completion(task) {
  match task() {
    None -> ();
    Some(next) -> run-to-completion(next);
  }
}

// Alternate two steppable tasks
fun interleave-steppable(task, other) {
  match task() {
    None -> run-to-completion(other);
    Some(next) -> interleave-steppable(other, next);
  }
}

// Alternate two yielding functions
fun interleave(task, other) {
  val steppable = to-steppable(task);
  val steppable-other = to-steppable(other);
  interleave-steppable(steppable, steppable-other)
}

fun main() {
  interleave(
    fn() print-each([1,2,3]),
    fn() print-each([10,20,30,40])
  );
}
