effect ask {
  fun ask(x : ()) : int;
}

effect fail {
  control fail(x : ()) : ();
}

fun main() {
  with handler {
    control fail(_) { // fail must be handled here
      println-int(1)
    };
  };
  with handler {
    fun ask(_) { // runs in-place at the perform-site of ask(())
      fail(()); -1;
     };
  };
  with handler {
    control fail(_) { // fail must not be handled here
      println-int(0);
    };
  };
  // perform ask(())
  ask(());
  ();
}
