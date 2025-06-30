fun triangle(n) {
  fun tri(i, acc) {
    if i == 0
    then acc
    else tri(i - 1, acc + i)
  };

  tri(n, 0)
}

fun main() {
  read-int(()).triangle().println-int()
}
