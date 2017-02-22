#!/usr/bin/env python3

import sys
import os
import random
import typing
import collections

from argparse import Action, ArgumentError, ArgumentParser

from lib.utils import globals
from lib.system import analysis
from lib.system.environment import create_program_from_input_file
from lib.system.directed_graphs import InstrumentationPointGraph
from lib.system.vertices import is_basic_block

assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'


State = collections.namedtuple('State', ['ipg', 'vertex'])


def generate_trace(program : analysis.Program,
                   instrumentation_point_graphs : typing.Dict[str, InstrumentationPointGraph]):

    def find_successor_program_point(function, vertex, program_point):
        for succ_edge in vertex.successor_edge_iterator():
            succ_vertex = function.get_vertex(succ_edge.vertex_id)
            if succ_vertex.program_point == program_point:
                return succ_vertex
        assert False

    def inch_forward(function, vertex):
        edge_idx = random.randint(0, vertex.number_of_successors()-1)
        succ_edge = vertex.get_ith_successor_edge(edge_idx)
        return function.get_vertex(succ_edge.vertex_id)

    # Start execution from the entry point of the program
    function = instrumentation_point_graphs[program.call_graph.root_function.name]
    vertex = function.entry_vertex
    # Maintain a call stack
    call_stack = [State(function, vertex)]
    time = 1
    trace = []
    while call_stack:
        if not vertex.abstract:
            trace.append('{} {}'.format(vertex.program_point, time))
            time += random.randint(1, random.randint(1, globals.args['max_time_increment']))

        # Transition between instrumentation points
        if vertex.abstract:
            # Call detected
            call_stack.append(State(function, vertex))
            callee_vertex = None
            for control_flow_graph in program:
                if control_flow_graph.has_vertex_for_program_point(vertex.program_point):
                    callee_vertex = program.call_graph.get_vertex_with_name(control_flow_graph.name)
                    break
            assert callee_vertex
            function = instrumentation_point_graphs[callee_vertex.name]
            if vertex.program_point == function.entry_vertex.program_point:
                vertex = function.entry_vertex
            else:
                vertex = find_successor_program_point(function,
                                                      function.entry_vertex,
                                                      vertex.program_point)
        elif vertex == function.exit_vertex:
            # Return detected
            state = call_stack.pop()
            # Only transition back to a caller if it exists
            if call_stack:
                # Find the inlined exit sign post
                vertex = find_successor_program_point(state.ipg,
                                                      state.vertex,
                                                      vertex.program_point)
                function = state.ipg
                vertex = inch_forward(function, vertex)
        else:
            # Intraprocedural move
            vertex = inch_forward(function, vertex)
    return trace


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

    parser.add_argument('--max-time-increment',
                        action=CheckForPositiveValue,
                        type=int,
                        help='choose maximum increment between timestamps',
                        metavar='<INT>',
                        default=1000)

    globals.add_common_command_line_arguments(parser)
    globals.args = vars(parser.parse_args())
    globals.set_filename_prefix(globals.args['program_file'])
    base, _ = os.path.splitext(os.path.abspath(globals.args['program_file']))
    globals.args['trace_file'] = base + '.trace.txt'


if __name__ == '__main__':
    parse_the_command_line()
    program = create_program_from_input_file()
    analysis.instrument_branches(program)
    instrumentation_point_graphs = analysis.build_instrumentation_point_graphs(program)
    with open(globals.args['trace_file'], 'w') as out_file:
        for _ in range(1, globals.args['number_of_runs']+1):
            for t in generate_trace(program, instrumentation_point_graphs):
                out_file.write(t)
                out_file.write('\n')
            out_file.write('\n')


