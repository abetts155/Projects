#!/usr/bin/env python3

import sys
import argparse
import typing
import re
import collections

assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'

from lib.utils import globals
from lib.system import environment
from lib.system import analysis
from lib.system.directed_graphs import InstrumentationPointGraph, CallGraph, DepthFirstSearch
from lib.system import calculations


State = collections.namedtuple('State', ['ipg', 'vertex'])


def parse_trace(call_graph: CallGraph,
                instrumentation_point_graphs : typing.Dict[str, InstrumentationPointGraph]):
    def parse_trace_event(line):
        line = line.replace('(', '').replace(')', '').replace(',', '').strip()
        lexemes = line.split(' ')
        try:
            assert 2 <= len(lexemes) <= 3
            time = lexemes[-1]
            if len(lexemes) == 3:
                program_point = (int(lexemes[0]), int(lexemes[1]))
            else:
                program_point = int(lexemes[0])
            return program_point, int(lexemes[-1])
        except ValueError:
            print("Unable to convert string into integer on line '{}'".format(line))
            sys.exit(1)

    def transition(function, vertex, program_point):
        for idx in range(0, vertex.number_of_successors()):
            succ_edge = vertex.get_ith_successor_edge(idx)
            succ_vertex = function.get_vertex(succ_edge.vertex_id)
            if succ_vertex.program_point == program_point:
                return succ_vertex
        assert False, ('Unable to transition from {} to {} in function {}'.
                       format(vertex.program_point, program_point, function.name))

    end_to_end_execution_times = []
    transition_execution_times = {}
    root_function = instrumentation_point_graphs[call_graph.root_function.name]
    with open(globals.args['trace_file'], 'r') as in_file:
        for line in in_file:
            if re.match(r'\S', line):
                program_point, time = parse_trace_event(line)
                if program_point == root_function.entry_vertex.program_point:
                    # New trace: reset everything
                    function = root_function
                    vertex = root_function.entry_vertex
                    call_stack = []
                    start_time = time
                    pred_time = time
                else:
                    # Inch forward one transition
                    pred_vertex = vertex
                    vertex = transition(function, vertex, program_point)
                    transition_execution_times.setdefault((pred_vertex, vertex), []).append(time-pred_time)
                    pred_time = time

                    if vertex.abstract:
                        # Moved to a vertex that has been inlined from a callee
                        # Find where the call is going
                        call_vertex = program.call_graph.get_vertex_with_name(function.name)
                        call_edges = [succ_edge for succ_edge in call_vertex.successor_edge_iterator()
                                      if pred_vertex.program_point in succ_edge.call_sites]
                        assert len(call_edges) == 1
                        call_stack.append(State(function, vertex))
                        callee_vertex = call_graph.get_vertex(call_edges.pop().vertex_id)
                        function = instrumentation_point_graphs[callee_vertex.name]
                        vertex = transition(function, function.entry_vertex, program_point)

                    if vertex == function.exit_vertex:
                        if function.name != call_graph.root_function.name:
                            state = call_stack.pop()
                            function = state.ipg
                            pred_vertex = function.get_vertex(state.vertex.get_ith_successor_edge(0).vertex_id)
                            vertex = transition(function, pred_vertex, vertex.program_point)
                        else:
                            end_to_end_execution_times.append(time-start_time)
    return transition_execution_times, end_to_end_execution_times


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Do WCET calculation using instrumentation point graphs')

    parser.add_argument('program_file',
                        help='a file containing program information'
                             ' (with .txt extension)')

    parser.add_argument('trace_file',
                        help='a file containing timestamped execution traces'
                             ' (with .txt extension)')

    parser.add_argument('--repeat',
                        type=int,
                        help='repeat the calculation this many times',
                        default=1,
                        metavar='<INT>')

    globals.add_common_command_line_arguments(parser)
    globals.args = vars(parser.parse_args())
    globals.set_filename_prefix(globals.args['program_file'])


def do_wcet_calculation(program : analysis.Program,
                        instrumentation_point_graphs,
                        transition_execution_times):
    depth_first_search_tree = DepthFirstSearch(program.call_graph,
                                               program.call_graph.root_function,
                                               False)
    call_execution_times = {}
    # Traverse call graph in reverse topological order to process callees before callers
    for call_vertex in depth_first_search_tree.post_order:
        instrumentation_point_graph = instrumentation_point_graphs[call_vertex.name]
        wcet = calculations.do_wcet_calculation_for_instrumentation_point_graph(instrumentation_point_graph,
                                                                                transition_execution_times,
                                                                                call_execution_times)
        call_execution_times[call_vertex.name] = wcet
        if call_vertex == depth_first_search_tree.post_order[-1]:
            return wcet


if __name__ == '__main__':
    parse_the_command_line()
    program = environment.create_program_from_input_file()
    analysis.instrument_branches(program)
    instrumentation_point_graphs = analysis.build_instrumentation_point_graphs(program)
    transition_execution_times, end_to_end_execution_times = parse_trace(program.call_graph, instrumentation_point_graphs)
    wcet = do_wcet_calculation(program, instrumentation_point_graphs, transition_execution_times)
    hwmt = max(end_to_end_execution_times)
    assert wcet >= hwmt
    print('WCET = {}'.format(wcet))
    print('HWMT = {}'.format(hwmt))



