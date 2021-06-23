import argparse
import sys

from graphs import (edges, graphs, vertices)
from system import (traces, programs)


class BoundSequence(list):
    def maximum(self, loop_transition):
        loop_exit_edges = [edge.successor() for edge in loop_transition]
        lists = []
        sublist = []
        for value in self:
            if isinstance(value, vertices.ProgramPointVertex):
                if value in loop_exit_edges:
                    if sublist:
                        lists.append(sublist)
                    sublist = []
            else:
                sublist.append(value)

        flattened_lists = []
        for sublist in lists:
            flattened_lists.append(sum(sublist))
        return max(flattened_lists, default=0)


def update_loop_signature(lnt: graphs.LoopNests, bounds, loop_stack, vertex: vertices.ProgramPointVertex):
    if lnt.is_header(vertex):
        successor_loop = lnt.find_loop(vertex)
        if successor_loop != loop_stack[-1]:
            loop_stack.append(successor_loop)
            bounds[successor_loop].append(1)
        else:
            bounds[successor_loop][-1] += 1
    elif lnt.is_loop_exit(vertex):
        outer_loop = loop_stack[-2]
        inner_loop = loop_stack[-1]
        loop_stack.pop()

        if outer_loop == lnt.exit:
            for nested_loop in lnt:
                if outer_loop != nested_loop:
                    bounds[nested_loop].append(vertex)
        else:
            bounds[inner_loop].append(vertex)


def parse(ppg: graphs.ProgramPointGraph, lnt, trace):
    parse_table = {}
    execution_times = {}
    for vertex in ppg:
        parse_table[vertex] = {}
        for successor_edge in ppg.successors(vertex):
            parse_table[vertex][successor_edge.successor()] = successor_edge
            execution_times[successor_edge] = []

    bounds = {loop: BoundSequence() for loop in lnt if not lnt.is_outermost_loop(loop)}

    with open(trace, 'r') as rd:
        for line in rd:
            elements = traces.TraceElement.parse(line)
            if elements:
                vertex = ppg[elements[0]]

                if vertex == ppg.entry:
                    predecessor_time = elements[1]
                    predecessor = vertex
                    loop_stack = [lnt.entry]
                else:
                    successor_edge = parse_table[predecessor][vertex]
                    time = elements[1] - predecessor_time
                    execution_times[successor_edge].append(time)

                    for element in successor_edge:
                        update_loop_signature(lnt, bounds, loop_stack, element)

                    update_loop_signature(lnt, bounds, loop_stack, vertex)

                    predecessor_time = elements[1]
                    predecessor = vertex

        for vertex in ppg:
            for successor_edge in ppg.successors(vertex):
                print('WCET {} => {}\t{}'.format(vertex,
                                                 successor_edge.successor(),
                                                 max(execution_times[successor_edge], default=0)))

        for loop in lnt:
            if not lnt.is_outermost_loop(loop):
                sequence = bounds[loop]
                (loop_transition,) = [successor_edge for successor_edge in lnt.successors(loop)
                                      if successor_edge.direction == edges.LoopTransition.Direction.EXIT]
                print('Local  {}\t\t{}'.format(loop.header, sequence.maximum(loop_transition)))
                if loop_transition.successor() == lnt.exit:
                    stack = [loop]
                    while stack:
                        inner_loop = stack.pop()
                        inner_sequence = bounds[inner_loop]
                        print('Global {}\t\t{}'.format(inner_loop.header, inner_sequence.maximum(loop_transition)))
                        stack.extend([successor_edge.successor() for successor_edge in lnt.successors(inner_loop)
                                      if successor_edge.direction == edges.LoopTransition.Direction.ENTRY])


def main(**kwargs):
    the_program = programs.IO.read(kwargs['program'])

    name = traces.TraceFile.extract_subprogram(the_program, kwargs['trace'])
    subprogram = the_program[name]
    graph_type = traces.TraceFile.extract_type(the_program, kwargs['trace'])

    ppg = graphs.ProgramPointGraph.create_from_control_flow_graph(subprogram.cfg)
    ppg.dotify()

    lnt = graphs.LoopNests(ppg)
    lnt.dotify()

    if graph_type is graphs.ProgramPointGraph:
        parse(ppg, lnt, kwargs['trace'])
    else:
        ipg = graphs.InstrumentationPointGraph.create(ppg, lnt)
        ipg.dotify()
        parse(ipg, lnt, kwargs['trace'])


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Filter a set of traces to instrumented program points')

    parser.add_argument('--program',
                        help='read the program from this file',
                        required=True)

    parser.add_argument('--trace',
                        help='file containing trace to parse',
                        required=True)

    return parser.parse_args()


if __name__ == '__main__':
    assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'
    main(**vars(parse_the_command_line()))
