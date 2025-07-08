
fun main() {
  // val bindings are NOT polymorphic
  val id = fn(x) x;
  // this function doesn't get type forall A. A -> A
  // instead it's: A -> A for some specific A >= Union[Int,Bool,Unit]
  // it's fine to call id, but we can't use it's result as any specific type.
  id(3) + 3;
  id(True) || True;
  id(())
}
