# Koka Zero

Reimplementatin of core [Koka](https://koka-lang.github.io), compiling to
[LLVM-IR](https://llvm.org/docs/LangRef.html), for my third-year dissertation.

This uses Algebraic Subtyping rather than Koka's Hindley-Milner inference with effect rows, since the former can give a precise effect to each subexpression of a program, which enables` some optimisations. 

## Example (a generator producing the Fibonacci sequence)
```koka
effect control yield-int(x : int) : ();

// yield the fibonacci numbers
fun fibs() {
  fun fibs-from(current, next) {
    yield-int(current);
    fibs-from(next, current + next);
  };
  fibs-from(0, 1);
}

fun main() {
  with control yield-int(n) {
    // print the fibonacci numbers below 100
    if n <= 100
    then {
      print-int(n);
      resume(());  // keep generating
    }else { 
      ();  // stop generating
    };
  };
  fibs();
}
// outputs: 0 1 1 2 3 5 8 13 21 34 55 89
```

## Setup
Requirements:
- [`opam`](https://opam.ocaml.org/) the OCaml package manager
- [LLVM toolchain including `clang`](https://releases.llvm.org/)
- optional: `libgc` - the [Boehm Garbage Collector](https://hboehm.info/gc/)

```sh
make install-deps  # creates a fresh opam switch to install ocaml & the project's dependencies
make build
```

`koka-zero-config.sexp` points to the compilation dependencies which aren't captured by opam.
These should be updated to point to the current install of `clang`, 
the `runtime.c` file within this project, and `libgc`.
The latter is [Boehm Garbage Collector](https://hboehm.info/gc/), linked against
for memory management. To compile without garbage collection, set `(gc_path ())`.


### Testing
```sh
make test
```

## Usage

The compiler binary is in `bin/main.exe` - `compile.sh` is a wrapper that calls it with the 
`koka-zero-config.sexp`.

```sh
./compile.sh samples/reader.kk
./reader
```

Or run the interpreter with:
```sh
make start -- interpret samples/fib.kk
```
