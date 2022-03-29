from abc import ABC, abstractmethod
from collections import namedtuple
from datetime import datetime
import json
from math import inf
import os
import re
import statistics
import subprocess

Benchmark = namedtuple('Benchmark', 'name inputs input_limits')

# range of inputs a subject is run on
InputLimit = namedtuple('InputLimit', 'min max')
def input_limit(min=-inf, max=inf):
    return InputLimit(min=min, max=max)
def input_no_limit():
    return InputLimit(min=-inf, max=inf)

# TODO: track resident set size?
# TODO: real vs. user vs. system??
# a Datapoint stores the data collected from a single benchmark run
Datapoint = namedtuple('Datapoint', 'time_real_seconds')

# a command such as Command('ls', ['-al', '~'])
Command = namedtuple('Command', 'name args')

class Subject(ABC):
    """ a benchmark subject is a compiler/interpreter which can
        compile and run given benchmarks
    """

    def __init__(self, project_root='.'):
        self.project_root = project_root

    @abstractmethod
    def name(self):
        """ the name of this subject as a string """
        ...

    @abstractmethod
    def make(self):
        """ build the compiler itself (if applicable)"""
        ...

    @abstractmethod
    def get_version_info(self):
        """ return a dictionary containing versions of this subject
            and any applicable dependencies
        """
        ...

    @abstractmethod
    def compile_benchmark(self, name):
        """ compile the named benchmark to an binary (if applicable)
            and return a Command which executes it
        """
        ...

class KokaSubject(Subject):
    """ the established Koka compiler """

    def __init__(self, project_root='.'):
        super().__init__(project_root=project_root)

    def name(self):
        return 'koka'

    def make(self):
        pass

    def get_version_info(self):
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

    def compile_benchmark(self, name):
        path_prefix = f'{self.project_root}/bench/koka/{name}'
        path = f'{path_prefix}.kk'
        exe_path = f'{path_prefix}'
        _compile_result = subprocess.run(
            ['koka', path, '-O2', '-o', exe_path],
            check=True)

        set_executable(exe_path)
        return Command(name=exe_path, args=[])


class KokaZeroSubject(Subject):
    """ this, the koka zero compiler """

    def __init__(self, gc_location=None, project_root='.'):
        super().__init__(project_root=project_root)
        self.gc_location = gc_location

    def name(self):
        return 'koka-zero'

    def get_version_info(self):
        codebase_version_result = subprocess.run(
            ['git', 'rev-parse', 'HEAD'],
            check=True, capture_output=True)

        commit = codebase_version_result.stdout.decode('ascii').strip()

        clang_version_result = subprocess.run(
            ['clang', '--version'],
            check=True, capture_output=True)
        clang_version = clang_version_result.stdout.decode('ascii')

        gc_version = '<unknown>'
        if self.gc_location is not None:
            gc_readme_path = f'{self.gc_location}/README.md'

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

    def make(self):
        subprocess.run(['make'], check=True, cwd=self.project_root)

    def compile_benchmark(self, name):
        path_prefix = f'{self.project_root}/bench/koka-zero/{name}'
        path = f'{path_prefix}.kk'

        # preserve $PATH etc
        env = os.environ.copy()
        env['OPT_LEVEL'] = '3'
        env['PROJECT_ROOT'] = self.project_root

        compiler_path = f'{self.project_root}/compile.sh'
        _compile_result = subprocess.run(
            [compiler_path, path],
            check=True,
            env=env)

        exe_path = f'{path_prefix}'

        return Command(name=exe_path, args=[])

class KokaZeroInterpreterSubject(Subject):
    def __init__(self, project_root='.'):
        super().__init__(project_root=project_root)

    def name(self):
        return 'koka-zero-interpreter'

    def make(self):
        subprocess.run(['make'], check=True, cwd=self.project_root)

    def get_version_info(self):
        codebase_version_result = subprocess.run(
            ['git', 'rev-parse', 'HEAD'],
            check=True, capture_output=True)

        commit = codebase_version_result.stdout.decode('ascii').strip()

        return {
            'koka-zero commit': commit
        }

    def compile_benchmark(self, name):
        """ no actual work done when compiling, just prepare
            the command to evaluate
        """
        path_prefix = f'{self.project_root}/bench/koka-zero/{name}'
        path = f'{path_prefix}.kk'
        command = f'{self.project_root}/_build/default/bin/main.exe'
        return Command(name=command, args=['interpret', path])


def set_executable(path):
    """ add execute permissions for those who have read permissions.
        taken from https://stackoverflow.com/a/30463972
        CC BY-SA 3.0 licensed
    """
    mode = os.stat(path).st_mode
    mode |= (mode & 0o444) >> 2  # copy R bits to X
    os.chmod(path, mode)

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


def run_benchmarks(subjects, benchmarks, repeats=1, project_root='.'):
    """ run the given benchmarks, writing the results to a log file """

    versions = {}
    for subject in subjects:
        versions[subject.name()] = subject.get_version_info()

    results = {}

    for bench in benchmarks:
        print(bench.name)

        commands = {}
        bench_results = {}
        for subject in subjects:
            # TODO: capture compiler output (to silence)
            commands[subject.name()] = subject.compile_benchmark(bench.name)

        for input_data in bench.inputs:
            print(input_data)
            input_str = str(input_data)
            bench_results[input_data] = {}

            for subject in subjects:
                if subject.name() in bench.input_limits:
                    limits = bench.input_limits[subject.name()]
                    if input_data < limits.min or input_data > limits.max:
                        continue

                res = run_benchmark(
                    commands[subject.name()],
                    input_str,
                    repeats=repeats)
                # TODO: discard maximum?
                # TODO: catch & log segfaults

                print(f'{subject.name():<25}:', summarise(res))

                layout = 'input,subject'
                bench_results[input_data][subject.name()] = transpose(res)

        results[bench.name] = bench_results

    now = datetime.now()
    log = {
        'date': str(now),
        'version': versions,
        'layout': layout,
        'results': results
    }
    timestamp = now.strftime('%Y%m%d-%H%M%S')
    log_filename = f'{project_root}/bench/log/log-{timestamp}.json'

    os.makedirs(os.path.dirname(log_filename), exist_ok=True)
    with open(log_filename, 'w') as f:
        json.dump(log, f, ensure_ascii=False, indent=2)

    print(f'results written to {log_filename}')


def main():
    project_root = '.'
    subjects = [
        KokaSubject(project_root=project_root),
        KokaZeroSubject(project_root=project_root, gc_location='/home/dan/boehm/gc'),
        # KokaZeroInterpreterSubject(project_root=project_root)
    ]
    benchmarks = [
        # Benchmark(name='trib', inputs=range(10, 25)),
        Benchmark(name='fib', inputs=range(23, 45),
                  input_limits={
                      'koka-zero': input_limit(max=31),
                      'koka': input_limit(min=38)}),
        # Benchmark(name='fib-eff', inputs=[23, 24, 25, 26, 27, 28, 29, 30, 31]),
        Benchmark(name='mstate', inputs=range(10_000, 100_000, 10_000),
                  input_limits={'koka-zero': input_no_limit(),
                                'koka': input_limit(max=75_000)}),
        # mstate segfaults (stack overflows) at 80_000 in koka
        # TODO: want same number of samples for each
        Benchmark(name='triples', inputs=range(100, 1000, 50),
                  input_limits={'koka-zero': input_limit(max=251),
                                'koka': input_limit(max=951)}),
        Benchmark(name='under-control', inputs=range(10_000, 100_000, 10_000),
                  input_limits={
                      'koka': input_limit(max=25_000)}),
        # TODO: koka takes ~0s for all of under-fun
        Benchmark(name='under-fun', inputs=range(10_000, 100_000, 10_000),
                  input_limits={})
    ]
    repeats = 10
    run_benchmarks(subjects, benchmarks, repeats=repeats, project_root=project_root)


if __name__ == '__main__':
    main()
