
fun id(x) { x; };

fun main() {
  // demonstrating `id` gets a polymorphic type
  if id(True) then {
    id(id)(3);
  }else {
    id(());
    id(id)(4);
  };
}

