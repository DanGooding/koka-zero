
effect state {
  control get(_ : ()) : int;
  control set(x : int) : ();
};

fun main() {
  val h = handler {
    control get(_) {
      resume(3)
    };
  };
  ()
};
