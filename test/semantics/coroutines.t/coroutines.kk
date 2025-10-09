// Cooperative multithreading - yield allows another task to run.
effect control yield(_ : ()) : ();

// Prints a list of values, yielding after each.
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
// This has the recursive type: rec<step, () -> option<step>>.
// Which expands to () -> option<() -> option<() -> ...>>.
fun to-steppable(task) {
  fn() {
    with control yield(()) {
      // If the task yields, the resumption is the next step.
      Some(fn() resume(()))
    };
    task();
    None // Once the task completes, there is no next step.
  }
}

fun run-to-completion(task) {
  match task() {
    None -> ();
    Some(next) -> run-to-completion(next);
  }
}

// Round-robin a list of steppable tasks
fun interleave-steppable(tasks) {
  match tasks {
    Nil -> ();
    Cons(task, rest) -> {
      match task() {
        None -> interleave-steppable(rest);
        Some(next) -> interleave-steppable(concat(rest, [next]));
      }
    }
  }
}

// Round-robin a list of yielding functions
fun interleave(tasks) {
  val steppable = tasks.map(to-steppable);
  interleave-steppable(steppable)
}

fun main() {
  val ones = fn() print-each([1,2,3]);
  val tens = fn() print-each([10,20,30,40]);
  interleave([ones, tens]);
}
