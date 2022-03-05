
fun sum(n) {
  if n <= 0
  then 0
  else 1 + sum(n - 1);
};

fun main() {
  read-int(()).sum().print-int();
};
