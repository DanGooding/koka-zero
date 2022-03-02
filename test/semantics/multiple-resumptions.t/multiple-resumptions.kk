
fun int-of-bool(b) {
  if b then 1 else 0;
};

effect control flip(_ : ()) : bool;

fun flip-either(action) {
  with handler {
    control flip(_) {
      resume(False) || resume(True);
    };
  };
  action();
};

fun three-digit() {
  val x = flip(());
  val y = flip(());
  val z = flip(());

  100 * int-of-bool(x) + 10 * int-of-bool(y) + int-of-bool(z);
};

fun find-satisfying(pred) {
  with flip-either;
  val n = three-digit();
  print-int(n);
  pred(n);
};

fun main() {
  find-satisfying(fn(x) x == 101).int-of-bool.print-int();
};
