FROM alpine:3.20 AS build-koka-compiler
WORKDIR /build

RUN apk add make
RUN apk add bash
# required to build the ocaml compiler & llvm package:
RUN apk add musl-dev
RUN apk add binutils
RUN apk add clang

RUN apk add opam
# don't need the safety bubblewrap layer since we're already in a container
RUN \
  --mount=type=cache,target=~/.opam/download-cache \
  opam init \
  --disable-sandboxing \ 
  --compiler=ocaml-base-compiler.5.3.0

# the llvm ocaml package doesn't know how to install this
RUN apk add llvm18-static

RUN \
  --mount=type=bind,source=Makefile,target=Makefile \
  --mount=type=bind,source=koka-zero.opam,target=koka-zero.opam \
  --mount=type=cache,target=~/.opam/download-cache \
  make install-deps

# build the compiler exe
RUN \
  --mount=type=bind,source=Makefile,target=Makefile \
  --mount=type=bind,source=lib,target=lib \
  --mount=type=bind,source=bin,target=bin \
  --mount=type=bind,source=dune,target=dune \
  --mount=type=bind,source=dune-project,target=dune-project \
  make build

FROM alpine:3.22 AS run-koka-compiler
WORKDIR /app
# this container is suitable for running the compiler

COPY --from=build-koka-compiler \
  /build/_build/default/bin/main.exe \
  koka-zero

COPY \
  ./lib/execution/runtime/ \
  runtime/

RUN apk add clang

# TODO: get libgc headers + objects
# and include -gc in the config

RUN /app/koka-zero create-config \
  -clang /usr/bin/clang \
  -runtime runtime/runtime.c \
  -o koka-zero-config.sexp
