
effect read {
  control ask(x : ()) : int;
};

fun main() {
  ask(()); // missing handler for read effect
};
