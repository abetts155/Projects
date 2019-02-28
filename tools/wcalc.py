import argparse
import os
import shutil
import sys
import threading

from graphs import graphs, instrumentation, vertices
from system import program
from utils import messages


def parse_trace(the_program: program.Program, root: str, trace: str):
    execution_times = {}
    parse_stack = []
    loop_stack = []
    with open(trace, 'r') as rd:
        for line in rd:
            lexemes = line.split()
            number = int(lexemes[0])
            time = int(lexemes[1])

            if len(parse_stack) == 0:
                subprogram = the_program[root]
                parse_stack.append((subprogram.ipg, subprogram.ipg.entry))
                loop_stack.append((subprogram.lnt, subprogram.lnt.entry))
                predecessor_time = time
            else:
                ipg, vertex = parse_stack.pop()
                old_size = len(parse_stack)
                for edge in ipg.successors(vertex):
                    successor = edge.successor()
                    if isinstance(successor.program_point, instrumentation.Instrumentation):
                        if edge.successor().program_point.number == number:
                            execution_times.setdefault(edge, set()).add(time - predecessor_time)
                            # Transition from instrumentation to instrumentation.
                            parse_stack.append((ipg, successor))
                    elif isinstance(successor.program_point, vertices.CallVertex):
                        subprogram = the_program[successor.program_point.callee]
                        if subprogram.ipg.entry.program_point.number == number:
                            execution_times.setdefault(edge, set()).add(time - predecessor_time)
                            # Transition from instrumentation to instrumentation in another subprogram.
                            # Go to the call vertex, in readiness for the return.
                            parse_stack.append((ipg, successor))
                            # Go to the start start of the callee.
                            parse_stack.append((subprogram.ipg, subprogram.ipg.entry))
                            loop_stack.append((subprogram.lnt, subprogram.lnt.entry))

                predecessor_time = time

                # Check we have progressed.
                assert len(parse_stack) > old_size

            ipg, vertex = parse_stack[-1]
            if len(ipg.successors(vertex)) == 0:
                # Subprogram is done, so unwind the exit instrumentation point.
                parse_stack.pop()
                # Check we are not done in the root subprogram.
                if parse_stack:
                    # Unwind to the call vertex.
                    ipg, vertex = parse_stack.pop()
                    # There should be a single transition, to a ghost instrumentation point.
                    (successor_edge,) = ipg.successors(vertex)
                    assert instrumentation.is_ghost(successor_edge.successor().program_point)
                    parse_stack.append((ipg, successor_edge.successor()))

    for edge, times in execution_times.items():
        print(edge, max(times))


def create_instrumentation_point_graphs(the_program):
    for subprogram in the_program:
        subprogram.cfg.dotify()
        ppg = graphs.ProgramPointGraph.create_from_control_flow_graph(subprogram.cfg)
        ppg.dotify()
        lnt = graphs.LoopNests(ppg)
        lnt.dotify()
        subprogram.ipg = graphs.InstrumentationPointGraph.create(ppg, lnt)
        subprogram.ipg.dotify()


def main(**kwargs):
    the_program = program.IO.read(kwargs['program'])
    the_program.cleanup()
    the_program.call_graph.dotify()

    if not kwargs['root'] in the_program:
        messages.error_message("Given root subprogram '{}' does not belong to the program".format(kwargs['root']))

    create_instrumentation_point_graphs(the_program)
    parse_trace(the_program, kwargs['root'], kwargs['trace'])


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Do WCET calculation using instrumentation point graphs')

    parser.add_argument('--program',
                        metavar='<FILE>',
                        help='read the program from this file',
                        required=True)

    parser.add_argument('--trace',
                        metavar='<FILE>',
                        help='extract timing data from this set of traces',
                        required=True)

    parser.add_argument('-r',
                        '--root',
                        metavar='<SUBPROGRAM>',
                        help='use this subprogram as the root of the analysis',
                        required=True)

    return parser.parse_args()


if __name__ == '__main__':
    assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'
    assert shutil.which('lp_solve', mode=os.X_OK), 'Script requires lp_solve to be in your path'
    threading.stack_size(2 ** 26)
    sys.setrecursionlimit(2 ** 20)
    main(**vars(parse_the_command_line()))
