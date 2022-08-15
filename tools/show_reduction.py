from enum import Enum
from glob import glob
from matplotlib import pyplot as plt
from re import compile


def set_matplotlib_defaults():
    plt.style.use('seaborn-deep')
    plt.rcParams['font.serif'] = 'Ubuntu'
    plt.rcParams['font.monospace'] = 'Ubuntu Mono'
    plt.rcParams['font.size'] = 10
    plt.rcParams['axes.labelsize'] = 10
    plt.rcParams['axes.labelweight'] = 'bold'
    plt.rcParams['axes.titlesize'] = 10
    plt.rcParams['xtick.labelsize'] = 8
    plt.rcParams['ytick.labelsize'] = 8
    plt.rcParams['legend.fontsize'] = 10
    plt.rcParams['figure.titlesize'] = 12


def main():
    info_regex = compile(r'(\w+)\s(\d+.\d+)')
    prefix = 'results.'
    suffix = '.txt'
    result_files = sorted(glob('{}[0-9]*{}'.format(prefix, suffix)), key=lambda name: int(name[len(prefix):-len(suffix)]))
    lines = {}
    names = set()
    x_values = []

    largest_time = 0
    largest_size = 0
    subprograms = 0
    for a_file in result_files:
        alg_times = {}
        with open(a_file, 'r') as in_file:
            for line in in_file:
                match = info_regex.match(line)
                if match:
                    name = match.group(1)
                    execution_time = match.group(2)
                    alg_times.setdefault(name, []).append(float(execution_time))
                    names.add(name)

        if sum(len(x) for x in alg_times.values()):
            lexemes = a_file.split('.')
            x_values.append(lexemes[1])
            largest_size = max(largest_size, int(lexemes[1]))

            for name in alg_times.keys():
                subprograms = max(subprograms, len(alg_times[name]))
                total_time = sum(alg_times[name])
                lines.setdefault(name, []).append(total_time)
                largest_time = max(largest_time, total_time)

    fig, ax = plt.subplots(1, 1, constrained_layout=True)
    colors = ['black', 'dodgerblue', 'salmon']
    markers = ['^', 'o', '*']
    names = sorted(names)
    for i, name in enumerate(names):
        ax.plot(x_values, lines[name], label=name, c=colors[i], marker=markers[i], lw=0.5, ms=3)

    ax.get_xaxis().set_major_locator(plt.MaxNLocator())
    ax.get_yaxis().set_major_locator(plt.MaxNLocator())

    ax.set_xlabel('Size of a CFG')
    ax.set_ylabel('Average time (seconds) to process {} CFGs'.format(subprograms))

    ax.yaxis.grid(True)

    ax.legend(loc='upper left')
    plt.show()


if __name__ == '__main__':
    set_matplotlib_defaults()
    main()
