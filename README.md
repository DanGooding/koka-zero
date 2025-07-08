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
- [opam](https://opam.ocaml.org/)
- [LLVM and clang](https://releases.llvm.org/)
- optional: [Boehm Garbage Collector](https://hboehm.info/gc/)

```sh
# optional:
make create_switch  # create a new opam switch
eval $(opam env)
```

```sh
make deps  # install development dependencies (dune)
make  # build the project
# which may prompt to run:
dune external-lib-deps --missing --root . @install  # install dependencies
```
The language runtime uses the [Boehm Garbage Collector](https://hboehm.info/gc/)
for memory management. `$GC` in `compile.sh` should be updated to
point to your system's install (e.g. `/home/xyz/gc`). To compile without garbage
collection, set `$DISABLE_GC` (i.e. `DISABLE_GC=1 ./compile.sh ...`).


### Testing
```sh
make test
```



## Usage

Compile: first to textual llvm-ir, then via `clang` to an executable
```sh
./compile.sh samples/reader.kk
./reader
```

Or interpret a koka program with:
```sh
make start -- interpret samples/fib.kk
```

## Development

Debug the compiler using OCaml debugger with:
```sh
make debug
set arguments "compile" "samples/hello.kk" "-o" "hello.ll"
run
```
