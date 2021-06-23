import argparse
import enum
import sys
import threading
import time
import typing

from graphs import graphs
from system import programs
from utils import messages


def do_verification(cfg: graphs.ControlFlowGraph, candidate_tree, reference_tree):
    messages.debug_message("Verifying...")
    differences = set()
    for v in cfg:
        if v != cfg.entry:
            (candidate,) = candidate_tree.predecessors(v)
            (reference,) = reference_tree.predecessors(v)

            if candidate.predecessor() != reference.predecessor():
                differences.add((v, candidate.predecessor(), reference.predecessor()))

    message = "Differences ({} vs {}):\n{}".format(candidate_tree.__class__.__name__,
                                                   reference_tree.__class__.__name__,
                                                   '\n'.join('{}=>{} {}=>{}'.format(candidate, v, reference, v) for v, candidate, reference in differences))

    if differences:
        messages.error_message(message)


class Algorithms(enum.Enum):
    Betts = 'Betts'
    Cooper = 'Cooper'
    Tarjan = 'Tarjan'


class Time:
    __slots__ = ['name', 'time']

    def __init__(self, name: Algorithms, time):
        self.name = name
        self.time = time


def main(program_filename: str,
         repeat:           int,
         subprogram_names: typing.List[str],
         verify:           bool):
    the_program = programs.IO.read(program_filename)
    the_program.cleanup()

    messages.verbose_message("Verification is {}".format("ON" if verify else "OFF"))

    analysable_subprograms = [subprogram for subprogram in the_program
                              if not subprogram_names or (subprogram_names and subprogram.name in subprogram_names)]

    results = []
    speedups = []
    slowdowns = []
    for subprogram in analysable_subprograms:
        messages.verbose_message("Analysing", subprogram.name)
        subprogram.cfg.dotify()
        betts_times = []
        tarjan_times = []
        cooper_times = []

        for i in range(0, repeat):
            subprogram.cfg.shuffle_edges()

            start = time.time()
            betts_tree = graphs.Betts(subprogram.cfg)
            betts_times.append(time.time() - start)

            start = time.time()
            tarjan_tree = graphs.LengauerTarjan(subprogram.cfg, subprogram.cfg.entry)
            tarjan_times.append(time.time() - start)

            start = time.time()
            cooper_tree = graphs.Cooper(subprogram.cfg)
            cooper_times.append(time.time() - start)

            if verify:
                messages.debug_message("Betts versus Tarjan")
                do_verification(subprogram.cfg, betts_tree, tarjan_tree)
                messages.debug_message("Cooper versus Tarjan")
                do_verification(subprogram.cfg, cooper_tree, tarjan_tree)
                messages.debug_message("Betts versus Cooper")
                do_verification(subprogram.cfg, betts_tree, cooper_tree)

        betts = Time(Algorithms.Betts, sum(betts_times) / repeat)
        cooper = Time(Algorithms.Cooper, sum(cooper_times) / repeat)
        tarjan = Time(Algorithms.Tarjan, sum(tarjan_times) / repeat)
        times = [betts, cooper, tarjan]
        times.sort(key=lambda t: t.time)
        results.append(times)

        messages.verbose_message('{} vertices={} edges={} branches={} merges={}'.format(subprogram.name,
                                                                                        subprogram.cfg.number_of_vertices(),
                                                                                        subprogram.cfg.number_of_edges(),
                                                                                        subprogram.cfg.number_of_branches(),
                                                                                        subprogram.cfg.number_of_merges()))
        messages.verbose_message('{}:: {:.4f}'.format(times[0].name.value,
                                                      times[0].time))
        messages.verbose_message('{}:: {:.4f} {:.1f}X faster'.format(times[1].name.value,
                                                                     times[1].time,
                                                                     times[1].time/times[0].time))
        messages.verbose_message('{}:: {:.4f} {:.1f}X faster'.format(times[2].name.value,
                                                                     times[2].time,
                                                                     times[2].time/times[0].time))

    messages.verbose_message("======> Summary")
    for name in Algorithms:
        first = [r for r in results if r[0].name == name]
        if first:
            messages.verbose_message('{} came first {} times'.format(name.value, len(first)))

    for name in Algorithms:
        second = [r for r in results if r[1].name == name]
        if second:
            messages.verbose_message('{} came second {} times'.format(name.value, len(second)))

    for name in Algorithms:
        third = [r for r in results if r[2].name == name]
        if third:
            messages.verbose_message('{} came third {} times'.format(name.value, len(third)))


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Compare dominator algorithms')

    parser.add_argument('--program',
                        help='read the program from this file',
                        required=True)

    parser.add_argument('--repeat',
                        type=int,
                        help='repeat the dominator tree creation this many times',
                        default=1,
                        metavar='<INT>')

    parser.add_argument('-s',
                        '--subprogram',
                        nargs='+',
                        help='only do the calculation for these subprograms',
                        metavar='<NAME>')

    parser.add_argument('--verify',
                        action='store_true',
                        help='verify the dominator trees against each other',
                        default=False)

    return parser.parse_args()


if __name__ == '__main__':
    threading.stack_size(2 ** 26)
    sys.setrecursionlimit(2 ** 30)
    kwargs = vars(parse_the_command_line())
    main(kwargs['program'], kwargs['repeat'], kwargs['subprogram'], kwargs['verify'])

