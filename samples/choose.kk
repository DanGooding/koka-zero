
effect control choice(x : ()) : bool;

fun choice-const(b, action) {
  with control choice(x) {
    resume(b);
  };
  action();
};

fun choice-any(action) {
  with control choice(x) {
    resume(False) || resume(True);
  };
  action();
};

fun pick01() {
  if choice(()) then 1 else 0;
};

fun pick-number() {
   val x = pick01() * 100;
   val y = pick01() * 10;
   val z = pick01();
  x + y + z;
};

fun main() {
  with choice-any();
  pick-number() == 101;
};

