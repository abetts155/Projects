import argparse
import random
import sys

from graphs import graphs
from system import traces, program
from utils import messages


def generate_trace(ppg: graphs.ProgramPointGraph, **kwargs):
    trace = traces.Trace()
    stack = [ppg.entry]
    time = 0
    while stack:
        v = stack.pop()
        trace.append(traces.TraceElement(v, time))

        if v != ppg.exit:
            index = random.randint(0, len(ppg.successors(v)) - 1)
            next_edge = ppg.successors(v)[index]
            stack.append(next_edge.successor())
            time += random.randint(1, kwargs['time_increment'])
    return trace


def main(**kwargs):
    kwargs['number_of_runs'] = max(kwargs['number_of_runs'], 1)
    the_program = program.IO.read(kwargs['filename'])
    for subprogram in the_program:
        messages.debug_message('Creating traces for {}'.format(subprogram.name))
        ppg = graphs.ProgramPointGraph.create_from_control_flow_graph(subprogram.cfg)
        ppg.dotify()

        all_traces = traces.Traces()
        for trace_number in range(1, kwargs['number_of_runs'] + 1):
            trace = generate_trace(ppg, **kwargs)
            all_traces.append(trace)
        all_traces.write(ppg.trace_filename())


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Create a set of traces for a program')

    parser.add_argument('--filename',
                        help='read the program from this file',
                        required=True)

    parser.add_argument('--time-increment',
                        type=int,
                        help='choose maximum increment between timestamps',
                        metavar='<INT>',
                        default=20)

    parser.add_argument('--number-of-runs',
                        type=int,
                        help='select number of program runs',
                        metavar='<INT>',
                        default=1)

    return parser.parse_args()


if __name__ == '__main__':
    assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'
    main(**vars(parse_the_command_line()))
