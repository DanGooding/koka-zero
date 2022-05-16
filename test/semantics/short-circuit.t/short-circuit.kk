
fun print-and-return(n, b) {
  println-int(n);
  b;
}

fun test-and() {
  // expect: 1
  print-and-return(1, False) && print-and-return(2, False);
}

fun test-or() {
  // expect: 3
  print-and-return(3, True) || print-and-return(4, True);
}

fun main() {
  test-and();
  test-or();
}

