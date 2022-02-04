
effect read {
  control ask(x : ()) : int;
};

effect control scramble(x : int) : int;

effect control throw(x : ()) : ();

fun main() {
  with handler {
    control scramble(x) {
      resume(x * x + x + 1)
    };
  };
  with handler {
    control throw(x) {
      ()
    };
  };
  with handler {
    control ask(x) {
      resume(3)
    };
  };
  if ask(()) < 0
  then throw(())
  else scramble(ask(())).print-int();
};
