# Koka Zero

Reimplementatin of core [Koka](https://koka-lang.github.io), compiling to 
LLVM-IR, for my third year dissertation

## Setup
```sh
# optional:
make create_switch  # create a new opam switch
make deps  # install dev dependencies
eval $(opam env)

make
# which prompts to run
dune external-lib-deps --missing --root . @install
```
The language runtime uses the [Boehm GC](https://hboehm.info/gc/) for memory 
management. `GC_INCLUDE` and `GC_LIB` in `compile.sh` should be updated to 
point to your system's install.


## Usage
Interpret a koka program with:
```sh
make start -- interpret samples/fib.kk
```

Or compile to textual llvm ir with:
```sh
./compile.sh samples/reader.kk
./reader  # execute
```

---

Debug the compiler using OCaml debugger with:
```sh
make debug
set arguments "compile" "samples/hello.kk" "-o" "hello.ll"
run
```
