#!/bin/bash

PROJECT_ROOT=${PROJECT_ROOT:-.}

declare -a BENCHMARKS=(fib.kk mstate.kk)

GC=/home/dan/boehm/gc $PROJECT_ROOT/bench/koka-zero/version.sh

for BENCHMARK in "${BENCHMARKS[@]}"; do
    BENCHMARK=$PROJECT_ROOT/bench/koka-zero/$BENCHMARK
    OPT_LEVEL=3 $PROJECT_ROOT/compile.sh $BENCHMARK

    EXE=${BENCHMARK%.kk}

    perf stat -r 10 -d $EXE
    # /usr/bin/time $EXE
done
