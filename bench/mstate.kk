/*
   Adapted from the koka benchmark suite
   https://github.com/koka-lang/koka/blob/master/test/bench/koka/mstate-int.kk
   the license of which is reproduced below
*/
/* Copyright 2012-2021, Microsoft Research, Daan Leijen
   This is free software; you can redistribute it and/or modify it under the
   terms of the Apache License, Version 2.0.
*/

effect st {
  control get(_ : ()) : int;
  control set(i : int) : ();
};

fun state(init, action) {
  fun thread-state() {
    with handler {
      control get(_) { fn(s) { resume(s)(s) } };
      control set(s) { fn(_) { resume(())(s) } };
    };
    val x = action();
    fn(s) { x };
  };
  thread-state()(init);
};

fun counter(c) {
  val i = get(());
  if i == 0
  then c
  else {
    set(i - 1);
    counter(c + 1);
  };
};

fun main() {
  val n = read-int(());
  with state(n);
  counter(0).print-int();
};
