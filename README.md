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
# $ dune external-lib-deps --missing --root . @install
```

## Usage
Interpret a koka program with:
```sh
make start -- interpret samples/fib.kk
```

Or compile to textual llvm ir with:
```sh
make start -- compile samples/reader.kk -o reader.ll
```

Debug using OCaml debugger with:
```sh
make debug
set arguments "compile" "samples/hello.kk" "-o" "hello.ll"
run
```
