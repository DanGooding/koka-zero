
fun main() {
  val length = read-int(());
  val input = range(0, length).map(fn(_) read-int(()));
  println(());

  input.sort(compare-int).print-list(print-int)
}
