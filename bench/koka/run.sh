#!/bin/bash

PROJECT_ROOT=${PROJECT_ROOT:-.}

declare -a BENCHMARKS=(fib.kk mstate.kk)

for BENCHMARK in "${BENCHMARKS[@]}"; do
    BENCHMARK=$PROJECT_ROOT/bench/koka/$BENCHMARK

    EXE=${BENCHMARK%.kk}
    koka $BENCHMARK -O2 -o $EXE
    chmod +x $EXE

    sudo perf stat -r 10 -d $EXE
    # /usr/bin/time $EXE
done

