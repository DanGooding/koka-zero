
fun even(n) {
  if n == 0
  then True
  else odd(n - 1);
};


fun odd(n) {
  if n == 0
  then False
  else even(n - 1);
};

fun main() { () };
