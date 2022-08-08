from collections import OrderedDict
from enum import Enum
from glob import glob
from matplotlib import pyplot as plt
from re import compile


class Algorithm(Enum):
    Betts = 0
    Tarjan = 1
    Cooper = 2


time_regex = compile(r'\d+.\d+')
prefix = 'results.'
suffix = '.txt'
result_files = sorted(glob('{}[0-9]*{}'.format(prefix, suffix)), key=lambda name: int(name[len(prefix):-len(suffix)]))
lines = {alg: [] for alg in Algorithm}
x_values = []

largest_time = 0
largest_size = 0
subprograms = 0
for a_file in result_files:
    alg_times = {alg: [] for alg in Algorithm}
    state = Algorithm.Betts
    with open(a_file, 'r') as in_file:
        for line in in_file:
            execution_time_match = time_regex.match(line)
            if execution_time_match:
                execution_time = execution_time_match[0]
                alg_times[state].append(float(execution_time))
                state = Algorithm((state.value + 1) % len(Algorithm))

    if sum(len(x) for x in alg_times.values()):
        lexemes = a_file.split('.')
        x_values.append(lexemes[1])
        largest_size = max(largest_size, int(lexemes[1]))

        for alg in Algorithm:
            subprograms = max(subprograms, len(alg_times[alg]))
            total_time = sum(alg_times[alg])
            lines[alg].append(total_time)
            largest_time = max(largest_time, total_time)

fig, ax = plt.subplots(1, 1, constrained_layout=True)
ax.plot(x_values, lines[Algorithm.Betts], label=Algorithm.Betts.name, c='black', marker='^', lw=0.5, ms=3)
ax.plot(x_values, lines[Algorithm.Tarjan], label=Algorithm.Tarjan.name, c='dodgerblue', marker='o', lw=0.5, ms=3)
ax.plot(x_values, lines[Algorithm.Cooper], label=Algorithm.Cooper.name, c='salmon', marker='*', lw=0.5, ms=3)

ax.get_xaxis().set_major_locator(plt.MaxNLocator())
ax.get_yaxis().set_major_locator(plt.MaxNLocator())

ax.set_xlabel('Size of a CFG')
ax.set_ylabel('Average time (seconds) to process {} CFGs'.format(subprograms))

ax.yaxis.grid(True)

ax.legend(loc='upper left')
plt.show()
