import argparse
import sys

from system import programs
from graphs import (vertices, edges, graphs)


def main(**kwargs):
    the_program = programs.IO.read(kwargs['program'])
    the_program.call_graph.dotify()

    if kwargs['manual']:
        programs.IO.read_properties(the_program, kwargs['manual'])
    else:
        instrumentation_id = 0
        for subprogram in the_program:
            ppg = graphs.ProgramPointGraph.create_from_control_flow_graph(subprogram.cfg)
            ppg.dotify()
            ppg.choose_instrumentation()

            old_to_new = {v:v for v in subprogram.cfg}
            for v in ppg:
                if isinstance(v.program_point, vertices.Vertex):
                    instrumentation_id += 1
                    instrumentation = vertices.InstrumentationVertex(vertices.Vertex.get_vertex_id(),
                                                                     ppg.name,
                                                                     instrumentation_id)
                    subprogram.cfg.add_vertex(instrumentation)
                    old_to_new[v.program_point] = instrumentation

                    if subprogram.cfg.entry == v.program_point:
                        subprogram.cfg.entry = instrumentation
                    if subprogram.cfg.exit == v.program_point:
                        subprogram.cfg.exit = instrumentation

                    for edge in subprogram.cfg.successors(v.program_point):
                        subprogram.cfg.add_edge(edges.ControlFlowEdge(instrumentation, edge.successor()))
                    for edge in subprogram.cfg.predecessors(v.program_point):
                        subprogram.cfg.add_edge(edges.ControlFlowEdge(edge.predecessor(), instrumentation))

                    subprogram.cfg.remove_vertex(v.program_point)

            for v in ppg:
                if isinstance(v.program_point, edges.Edge):
                    instrumentation_id += 1
                    predecessor = old_to_new[v.program_point.predecessor()]
                    successor = old_to_new[v.program_point.successor()]
                    subprogram.cfg.remove_edge(edges.ControlFlowEdge(predecessor, successor))
                    subprogram.cfg.add_edge(edges.InstrumentationEdge(predecessor, successor, instrumentation_id))

    programs.IO.write(the_program, kwargs['output'])


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Pick instrumentation points')

    parser.add_argument('--program',
                        help='read the program from this file',
                        required=True)

    parser.add_argument('--output',
                        help='write the instrumented program to this file',
                        required=True)

    parser.add_argument('--manual',
                        help='use instrumentation points selected in the property file')

    return parser.parse_args()


if __name__ == '__main__':
    assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'
    main(**vars(parse_the_command_line()))
