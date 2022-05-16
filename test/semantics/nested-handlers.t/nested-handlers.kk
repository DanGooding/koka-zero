
effect read {
  control ask(_ : ()) : int;
}

effect control scramble(x : int) : int;

effect control throw(_ : ()) : ();

fun main() {
  with handler {
    control scramble(x) {
      resume(x * x + x + 1)
    };
  };
  with handler {
    control throw(_) {
      ()
    };
  };
  with handler {
    control ask(_) {
      resume(3)
    };
  };
  if ask(()) < 0
  then throw(())
  else scramble(ask(())).println-int();
}
