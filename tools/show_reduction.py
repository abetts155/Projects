from collections import OrderedDict
from enum import Enum
from glob import glob
from matplotlib import pyplot as plt
from re import compile


class Algorithm(Enum):
    Betts = 0
    Tarjan = 1
    Cooper = 2


time_line = compile(r'\d+.\d+')
prefix = 'results.'
suffix = '.txt'
result_files = sorted(glob('{}[0-9]*{}'.format(prefix, suffix)), key=lambda name: int(name[len(prefix):-len(suffix)]))
lines = {alg: [] for alg in Algorithm}
x_values = []

for a_file in result_files:
    total = {alg:0 for alg in Algorithm}
    state = Algorithm.Betts
    with open(a_file, 'r') as in_file:
        for line in in_file:
            times = time_line.match(line)
            if times:
                total[state] += float(times[0])
                state = Algorithm((state.value + 1) % len(Algorithm))

    if sum(total.values()):
        lexemes = a_file.split('.')
        x_values.append(lexemes[1])

        for alg in Algorithm:
            lines[alg].append(total[alg])

fig, ax = plt.subplots(1, 1, constrained_layout=True)
ax.plot(x_values, lines[Algorithm.Betts], label=Algorithm.Betts.name, color='black')
ax.plot(x_values, lines[Algorithm.Tarjan], label=Algorithm.Tarjan.name, color='dodgerblue')
ax.plot(x_values, lines[Algorithm.Cooper], label=Algorithm.Cooper.name, color='salmon')
ax.legend()
plt.show()
