
effect control yield-int(x : int) : ();

// yield the fibonacci numbers
fun fibs() {
  fun fibs-from(current, next) {
    yield-int(current);
    fibs-from(next, current + next);
  };
  fibs-from(0, 1);
}

fun main() {
  val limit = read-int(());
  with control yield-int(n) {
    if n <= limit
    then {
      println-int(n);
      resume(());
    }else ();
  };
  fibs();
}

