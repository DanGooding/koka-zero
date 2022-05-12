
fun main() {
  // val bindings are NOT polymorphic
  val id = fn(x) x;
  id(3);
  id(True);
  id(());
}

