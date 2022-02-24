
effect read-A {
  fun ask-A(_ : ()) : int;
};

fun answer-A-const(x, action) {
  with handler {
    fun ask-A(_) {
      x;
    };
  };
  action();
};

effect read-B {
  fun ask-B(_ : ()) : int;
};

fun answer-B-double-A(x, action) {
  with handler {
    fun ask-B(_) {
      ask-A(()) * 2;
    };
  };
  action();
};

fun main() {
  with answer-A-const(5);
  with answer-B-double-A;
  with answer-A-const(7);
  print-int(ask-B(())); // 2 * 5 = 10
};


