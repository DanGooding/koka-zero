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
