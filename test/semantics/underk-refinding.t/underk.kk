effect evil {
  control evil(x : ()) : ();
};

// not relevant that `read` is tail-resumptive
effect read {
  fun ask(x : ()) : int;
};

// relevant that `tl` is tail resumptive!
effect tl {
  fun tl(x : ()) : int;
};

// run a resumption under a different handler context
fun f(k) {
  with handler { // h_read2
    fun ask(_) { 2 };
  };
  k(()); // 5: resume `evil` now with a different `ask` handler
};

fun g() {
  with handler { // h_read1
    fun ask(_) { 1 };
  };
  with handler { // h_evil
    control evil(_) {
      // 4: leak resumption to later resume under changed handler context
      resume; // FIXME: leaking like this fails the occurs check :/
    };
  };
  with handler { // h_tl
    fun tl(_) {
      ask(()).print-int(); // 2: receive value 1
      evil(()); // 3: perform `evil` within tail-resumptive operation clause
      ask(()).print-int(); // 6: recieve value 2
    };
  };
  tl(); // 1: perform `tl`
};

fun main() {
  f(g());
};
