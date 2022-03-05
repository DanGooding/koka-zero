/* Adapted from the koka benchmark suite
   https://github.com/koka-lang/koka/blob/master/test/bench/koka/mstate-int.kk
   the license of which is reproduced below
*/
/* Copyright 2012-2021, Microsoft Research, Daan Leijen
   This is free software; you can redistribute it and/or modify it under the
   terms of the Apache License, Version 2.0.
*/

import std/os/readline

effect st
  ctl get() : int
  ctl set(i : int)  : ()


fun state( init : int, action : () -> <st|e> a ) : e a
  with return(f) f(init)
  handle action
    return(x)      (fn(s) x)
    raw ctl get()  (fn(s) rcontext.resume(s)(s))
    raw ctl set(s) (fn(_) rcontext.resume(())(s))

fun counter( c : int ) : _ int
  val i = get()
  if (i==0) then c else
    set(i.dec)
    counter(c.inc)


pub fun main()
  val n = readline().parse-int().unjust()
  with state(n)
  counter(0).println
