effect control pick(choices : (int, int)) : int;

// effects can't yet be polymorphic, so we can't express a general exn type
effect control exn(_ : ()) : int;

fun pick-from-list(xs) {
  match xs {
    Nil -> exn(());
    Cons(a, bs) -> match bs {
      Nil -> a;
      Cons (b, cs) -> {
        val x = pick((a, b));
        with control exn(_) {
          // if pick-from-list(cs) fails because cs is empty, then just return x
          x
        };
        val y = pick-from-list(cs);
        pick((x, y))
      }
    };
  }
}

fun pick-max(action) {
  with control pick(pair) {
    val (a, b) = pair;
    resume(if a > b then a else b)
  };
  action()
}

fun max((a,b)) {
  a + b
}

fun main() {
  with control exn(_) {
    ()
  };
  with pick-max;
  pick-from-list([1,10,2,9,3,8,4,7,5,6]).println-int()
}