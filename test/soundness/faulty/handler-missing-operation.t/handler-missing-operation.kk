
effect state {
  control get(x : ()) : int;
  control set(x : int) : ();
};

fun main() {
  val h = handler {
    control get(x) {
      resume(3)
    };
  };
  ()
};
