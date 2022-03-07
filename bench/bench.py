from collections import namedtuple
from datetime import datetime
import json
import os
import re
import statistics
import subprocess

Benchmark = namedtuple('Benchmark', 'name inputs')

# TODO: track resident set size?
# TODO: real vs. user vs. system??
# a Datapoint stores the data collected from a single benchmark run
Datapoint = namedtuple('Datapoint', 'time_real_seconds')

# a command such as Command('ls', ['-al', '~'])
Command = namedtuple('Command', 'name args')

def set_executable(path):
    ''' add execute permissions for those who have read permissions.
        taken from https://stackoverflow.com/a/30463972
        CC BY-SA 3.0 licensed
    '''
    mode = os.stat(path).st_mode
    mode |= (mode & 0o444) >> 2  # copy R bits to X
    os.chmod(path, mode)


def get_koka_version_info():
    koka_version_result = subprocess.run(
        ['koka', '--version'],
        check=True, capture_output=True)

    koka_version = koka_version_result.stdout.decode('utf-8')

    gcc_version_result = subprocess.run(
        ['gcc', '--version'],
        check=True, capture_output=True)
    gcc_version = gcc_version_result.stdout.decode('ascii')

    return {
        'koka': koka_version,
        'gcc': gcc_version
    }

def get_koka_zero_version_info(gc_location=None):
    codebase_version_result = subprocess.run(
        ['git', 'rev-parse', 'HEAD'],
        check=True, capture_output=True)

    commit = codebase_version_result.stdout.decode('ascii').strip()

    clang_version_result = subprocess.run(
        ['clang', '--version'],
        check=True, capture_output=True)
    clang_version = clang_version_result.stdout.decode('ascii')


    gc_version = '<unknown>'
    if gc_location is not None:
        gc_readme_path = f'{gc_location}/README.md'

        gc_version_re = re.compile(r'version +\d+\.\d+\.\d+')
        with open(gc_readme_path) as f:
            for line in f:
                match = re.search(gc_version_re, line)
                if match:
                    gc_version = match[0]
                    break

    return {
        'koka-zero commit': commit,
        'clang': clang_version,
        'Boehm GC': gc_version
    }

def compile_koka_benchmark(name, project_root='.'):
    path_prefix = f'{project_root}/bench/koka/{name}'
    path = f'{path_prefix}.kk'
    exe_path = f'{path_prefix}'
    _compile_result = subprocess.run(
        ['koka', path, '-O2', '-o', exe_path],
        check=True)

    set_executable(exe_path)
    return Command(name=exe_path, args=[])

def make_koka_zero():
    subprocess.run(['make'], check=True)

def compile_koka_zero_benchmark(name, project_root='.'):
    path_prefix = f'{project_root}/bench/koka-zero/{name}'
    path = f'{path_prefix}.kk'

    # preserve $PATH etc
    env = os.environ.copy()
    env['OPT_LEVEL'] = '3'
    env['PROJECT_ROOT'] = project_root

    compiler_path = f'{project_root}/compile.sh'
    _compile_result = subprocess.run(
        [compiler_path, path],
        check=True,
        env=env)

    exe_path = f'{path_prefix}'

    return Command(name=exe_path, args=[])

def make_koka_zero_interpreter():
    subprocess.run(['make'], check=True)

def setup_koka_zero_interpreter_benchmark(name, project_root='.'):
    path_prefix = f'{project_root}/bench/koka-zero/{name}'
    path = f'{path_prefix}.kk'
    command = f'{project_root}/_build/default/bin/main.exe'
    args = 'interpret', path
    return Command(name=command, args=args)

def run_benchmark(command, input_data, repeats=1):
    """ given an executable to benchmark,
        and input to pass it as a string,
        return a list of datapoints for the given number of runs
    """
    datapoints = []
    for _ in range(repeats):
        benchmark_result = subprocess.run(
            ['/usr/bin/time', '-f%e', command.name] + list(command.args),
            check=True,
            input=input_data.encode('ascii'),
            capture_output=True)

        output = benchmark_result.stderr.decode('ascii').strip()
        time_real_seconds = float(output)

        datapoints.append(Datapoint(time_real_seconds=time_real_seconds))

    return datapoints

def summarise(datapoints):
    """ collapse many datapoints into one for human reading """
    return Datapoint(time_real_seconds=
        statistics.mean(d.time_real_seconds for d in datapoints))

def transpose(datapoints):
    """ turn a list of datapoints into a datapoint of lists """
    time_real_seconds = []
    for d in datapoints:
        time_real_seconds.append(d.time_real_seconds)
    return Datapoint(time_real_seconds=time_real_seconds)


def run_benchmarks(benchmarks, repeats=1, project_root='.', include_interpreter=False):
    """ run the given benchmarks, writing the results to a log file """
    koka_version = get_koka_version_info()
    koka_zero_version = get_koka_zero_version_info(
        gc_location='/home/dan/boehm/gc')

    make_koka_zero()
    if include_interpreter:
        make_koka_zero_interpreter()

    results = {}

    for bench in benchmarks:
        print(bench.name)
        # TODO: capture compiler output (to silence)
        koka_bench_command = compile_koka_benchmark(
            bench.name, project_root=project_root)
        koka_zero_bench_command = compile_koka_zero_benchmark(
            bench.name, project_root=project_root)
        if include_interpreter:
            koka_zero_interpreter_bench_command = \
                setup_koka_zero_interpreter_benchmark(
                    bench.name, project_root=project_root)

        bench_koka_results = {}
        bench_koka_zero_results = {}
        if include_interpreter:
            bench_koka_zero_interpreter_results = {}
        for input_ in bench.inputs:
            input_data = str(input_)
            print(input_)

            # TODO: abstract BenchmarkSubject class which koka/koka-zero inherit from
            koka_results = run_benchmark(
                koka_bench_command, input_data, repeats=repeats)
            print('koka                 :', summarise(koka_results))
            koka_zero_results = run_benchmark(
                koka_zero_bench_command, input_data, repeats=repeats)
            print('koka-zero            :', summarise(koka_zero_results))
            if include_interpreter:
                koka_zero_interpreter_results = run_benchmark(
                    koka_zero_interpreter_bench_command, input_data, repeats=repeats)
                print('koka-zero interpreter:', summarise(koka_zero_interpreter_results))
            # TODO: discard maximum?
            # TODO: catch & log segfaults

            bench_koka_results[input_] = transpose(koka_results)
            bench_koka_zero_results[input_] = transpose(koka_zero_results)
            if include_interpreter:
                bench_koka_zero_interpreter_results[input_] = \
                    transpose(koka_zero_interpreter_results)

        bench_results = {
            'koka': bench_koka_results,
            'koka-zero': bench_koka_zero_results,
            }
        if include_interpreter:
            bench_results['koka-zero interpreter'] = bench_koka_zero_interpreter_results
        results[bench.name] = bench_results

    now = datetime.now()
    log = {
        'date': str(now),
        'version': {
            'koka': koka_version,
            'koka-zero': koka_zero_version
        },
        'results': results
    }
    timestamp = now.strftime('%Y%m%d-%H%M%S')
    log_filename = f'{project_root}/bench/log/log-{timestamp}.json'

    os.makedirs(os.path.dirname(log_filename), exist_ok=True)
    with open(log_filename, 'w') as f:
        json.dump(log, f, ensure_ascii=False, indent=2)

    print(f'results written to {log_filename}')


def main():
    benchmarks = [
        Benchmark(name='sum', inputs=[1_000, 10_000, 100_000]),
        Benchmark(name='trib', inputs=range(10, 25)),
        Benchmark(name='fib', inputs=[23, 24, 25, 26, 27, 28, 29, 30, 31]),
        # Benchmark(name='fib-eff', inputs=[23, 24, 25, 26, 27, 28, 29, 30, 31]),
        # TODO: mstate-int32 for koka?
        Benchmark(name='mstate', inputs=[1, 10, 100, 1_000, 10_000])
        # mstate segfaults (stack overflows) at 100_000 in koka
        # TODO: fun vs ctl
    ]
    repeats = 5
    project_root = '.'
    run_benchmarks(benchmarks, repeats=repeats, project_root=project_root)


if __name__ == '__main__':
    main()
