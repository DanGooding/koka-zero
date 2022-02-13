
fun read-bool() {
  read-int(()) != 0;
};

effect control yield-int(x : int) : ();

// yield the fibonacci numbers in sequence
fun fibs() {
  fun fibs-from(current, next) {
    yield-int(current);
    fibs-from(next, current + next);
  };
  fibs-from(0, 1);
};

fun main() {
  with handler {
    control yield-int(x) {
      print-int(x);
      if read-bool()  // continue?
      then resume(())
      else ();
    };
  };
  fibs();
};

