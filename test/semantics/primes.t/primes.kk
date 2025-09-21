fun range(start, stop) {
  if start >= stop
  then []
  else Cons(start, range(start + 1, stop))
}

fun remove-multiples(prime, next-multiple, candidates) {
  match candidates {
    Nil -> Nil;
    Cons(x, xs) -> {
      if x == next-multiple
      then remove-multiples(prime, next-multiple + prime, xs)
      else if x > next-multiple
      then remove-multiples(prime, next-multiple + prime, candidates)
      else Cons(x, remove-multiples(prime, next-multiple, xs))
    }
  }
}

fun sieve(candidates) {
  match candidates {
    Nil -> Nil;
    Cons(prime, rest) -> {
      val rest-except-multiples = remove-multiples(prime, prime, rest);
      Cons(prime, sieve(rest-except-multiples))
    }
  }
}

fun print-list(xs, print-element) {
  match xs {
    Nil -> println(());
    Cons(x, xx) -> {
      print-element(x);
      print-list(xx, print-element);
    }
  }
}

fun main() {
  val limit = read-int(());
  range(2, limit).sieve.print-list(print-int);
}
