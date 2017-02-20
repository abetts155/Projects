#!/usr/bin/env python3

import sys
import os
import random
import typing
import collections

from argparse import Action, ArgumentError, ArgumentParser

import lib.utils
from lib.system import analysis
from lib.system.environment import create_program_from_input_file
from lib.system.directed_graphs import InstrumentationPointGraph, CallGraph
from lib.system.vertices import is_basic_block

assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'


State = collections.namedtuple('State', ['ipg', 'vertex'])


def generate_trace(out_file, call_graph : CallGraph, instrumentation_point_graphs : typing.Dict[str, InstrumentationPointGraph]):
    # Start execution from the entry point of the program
    function = instrumentation_point_graphs[call_graph.root_function.name]
    vertex = function.entry_vertex
    # Maintain a call stack
    call_stack = [State(function, vertex)]
    time = 1
    while call_stack:
        # A trace event only happens if the program point has not been added
        # for analysis purposes
        if not vertex.abstract:
            out_file.write('{} {}\n'.format(vertex.program_point, time))
            time += random.randint(1, 200)

        call_or_return = False
        if is_basic_block(vertex.program_point):
            call_vertex = program.call_graph.get_vertex_with_name(function.name)
            call_edges = [succ_edge for succ_edge in call_vertex.successor_edge_iterator()
                          if vertex.program_point in succ_edge.call_sites]
            assert len(call_edges) <= 1
            if call_edges:
                call_or_return = True
                call_stack.append(State(function, vertex))
                callee_vertex = program.call_graph.get_vertex(call_edges.pop().vertex_id)
                function = instrumentation_point_graphs[callee_vertex.name]
                vertex = function.entry_vertex

        if vertex == function.exit_vertex:
            call_or_return = True
            state = call_stack.pop()
            if function != instrumentation_point_graphs[call_graph.root_function.name]:
                # We have returned to the state just before the function call.
                # Advance to the function return program point that was
                # inlined from the callee
                function = state.ipg
                next_state = function.get_vertex(state.vertex.get_ith_successor_edge(0).vertex_id)
                next_state = function.get_vertex(next_state.get_ith_successor_edge(0).vertex_id)
                idx = 0
                found = False
                while idx < next_state.number_of_successors() and not found:
                    succ_edge = next_state.get_ith_successor_edge(idx)
                    succ_vertex = function.get_vertex(succ_edge.vertex_id)
                    found = succ_vertex.program_point == vertex.program_point
                    if found:
                        vertex = succ_vertex
                    idx += 1
                assert found

        if not call_or_return:
            edge_idx = random.randint(0, vertex.number_of_successors()-1)
            succ_edge = vertex.get_ith_successor_edge(edge_idx)
            vertex = function.get_vertex(succ_edge.vertex_id)


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
    instrumentation_point_graphs = analysis.build_instrumentation_point_graphs(program)
    with open(lib.utils.globals.args['trace_file'], 'w') as out_file:
        for _ in range(1,lib.utils.globals.args['number_of_runs']+1):
            generate_trace(out_file, program.call_graph, instrumentation_point_graphs)
            out_file.write('\n')

