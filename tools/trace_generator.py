#!/usr/bin/env python3

import sys
import os
import random

from argparse import Action, ArgumentError, ArgumentParser

import lib.utils
from lib.system import analysis
from lib.system.environment import create_program_from_input_file
from lib.system.directed_graphs import InstrumentationPointGraph
from lib.system.vertices import is_basic_block

assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'


def generate_trace(program : analysis.Program):
    # Maintain the call stack of instrumentation point graphs
    call_stack = []
    call_stack.append(InstrumentationPointGraph.instrument_as_per_user_instruction(program[program.call_graph.root_function.name]))
    # Start execution from the entry point of the program
    vertex = call_stack[-1].entry_vertex
    while call_stack:
        # Ignore program points added for analysis purposes
        if not vertex.abstract:
            print(vertex.program_point)

        call_or_return = False
        if is_basic_block(vertex.program_point):
            call_vertex = program.call_graph.get_vertex_with_name(call_stack[-1].name)
            for succ_edge in call_vertex.successor_edge_iterator():
                if vertex.program_point in succ_edge.call_sites:
                    call_or_return = True
                    succ_vertex = program.call_graph.get_vertex(succ_edge.vertex_id)
                    call_stack.append(InstrumentationPointGraph.instrument_as_per_user_instruction(program[succ_vertex.name]))
                    break
        else:
            if vertex == call_stack[-1].exit_vertex:
                call_or_return = True
                call_stack.pop()

        if not call_or_return:
            edge_idx = random.randint(0, vertex.number_of_successors()-1)
            succ_edge = vertex.get_ith_successor_edge(edge_idx)
            vertex = call_stack[-1].get_vertex(succ_edge.vertex_id)
    print()


def parse_the_command_line():
    class CheckForPositiveValue(Action):
        def __call__(self, parser, namespace, value, option_string=None):
            if value <= 0:
                raise ArgumentError('Argument {} requires a positive integer'. \
                                    format(option_string))
            setattr(namespace, self.dest, value)

    parser = ArgumentParser(description='Generate a random trace')

    parser.add_argument('program_file',
                        help='a file containing program information'
                             ' (with .txt extension)')

    parser.add_argument('--number-of-runs',
                        action=CheckForPositiveValue,
                        type=int,
                        help='select number of program runs',
                        metavar='<INT>',
                        required=True)

    lib.utils.globals.add_common_command_line_arguments(parser)
    lib.utils.globals.args = vars(parser.parse_args())
    lib.utils.globals.set_filename_prefix(lib.utils.globals.args['program_file'])
    base, _ = os.path.splitext(os.path.abspath(lib.utils.globals.args['program_file']))
    lib.utils.globals.args['trace_file'] = base + '.trace.txt'


if __name__ == '__main__':
    parse_the_command_line()
    program = create_program_from_input_file()
    analysis.build_instrumentation_point_graphs(program)
    for _ in range(1,lib.utils.globals.args['number_of_runs']+1):
        pass#generate_trace(program)

