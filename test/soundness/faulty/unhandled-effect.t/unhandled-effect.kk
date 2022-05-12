
effect read {
  control ask(_ : ()) : int;
}

fun main() {
  ask(()); // missing handler for read effect
}
