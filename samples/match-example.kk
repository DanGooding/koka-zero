fun print-bool(b) {
  val n = match b {
    False -> 0;
    True -> 1;
  };
  print-int(n)
}

fun main() {
  print-bool(True);
  println(());
}