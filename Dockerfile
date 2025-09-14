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
  make release-build

FROM alpine:3.22 AS build-libgc
WORKDIR /build

RUN apk add git

RUN git clone https://github.com/bdwgc/libatomic_ops.git --branch v7.8.2 --depth 1
RUN git clone https://github.com/bdwgc/bdwgc.git --branch v8.2.8 --depth 1

RUN ln -s /build/libatomic_ops /build/bdwgc/libatomic_ops
WORKDIR /build/bdwgc

RUN apk add autoconf automake libtool make
RUN apk add clang

RUN autoreconf -vif
RUN automake --add-missing

RUN ./configure
RUN make
RUN make install


FROM alpine:3.22 AS run-koka-compiler
WORKDIR /app
# this container is suitable for running the compiler
RUN apk add clang

COPY --from=build-koka-compiler \
  /build/_build/default/bin/main.exe \
  koka-zero

COPY \
  ./lib/execution/runtime/ \
  runtime/

ARG GC_SRC_PATH="/usr/local"
ARG GC_DEST_PATH="/usr/local"

COPY --from=build-libgc \
  ${GC_SRC_PATH}/lib/libgc.so* \
  ${GC_DEST_PATH}/lib

COPY --from=build-libgc \
  ${GC_SRC_PATH}/include \
  ${GC_DEST_PATH}/include

RUN /app/koka-zero create-config \
  -clang /usr/bin/clang \
  -runtime /app/runtime/runtime.c \
  -gc ${GC_DEST_PATH} \
  -o koka-zero-config.sexp
