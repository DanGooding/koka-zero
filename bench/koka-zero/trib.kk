
fun trib(n) {
  if n == 1
  then 1
  elif n == 2
  then 1
  elif n == 3
  then 2
  else trib(n - 1) + trib(n - 2) + trib(n - 3);
};

fun main() {
  read-int(()).trib().print-int();
};
