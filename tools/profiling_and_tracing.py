#!/usr/bin/env python3

import sys
import argparse
import random

from lib.utils import messages
from lib.utils import dot
from lib.system import program
from lib.graphs import graph


def do_super_block_instrumentation(control_flow_graph,
                                   trace,
                                   actual_execution_counts):
    instrumentation_points = set()
    for _, abstract_vertices in control_flow_graph.get_loop_nesting_tree().\
                                level_by_level_iterator\
                                (abstract_vertices_only=True):
        for abstract_vertex in abstract_vertices:
            if is_basic_block(abstract_vertex.program_point):
                subgraph = control_flow_graph.get_super_block_subgraph(abstract_vertex)
                subgraph.choose_instrumentation_points_for_profiling(instrumentation_points)
    
    filtered_trace = [program_point for program_point in trace 
                      if program_point in instrumentation_points]
    inferred_execution_counts = Counter(filtered_trace)
    
    for _, abstract_vertices in control_flow_graph.get_loop_nesting_tree().\
                                level_by_level_iterator\
                                (abstract_vertices_only=True):
        for abstract_vertex in abstract_vertices:
            if is_basic_block(abstract_vertex.program_point):
                subgraph = control_flow_graph.get_super_block_subgraph(abstract_vertex,
                                                                       redo=True)
                
                # Put execution counts on super blocks.
                for super_vertex in subgraph:
                    super_vertex.instrumented = False
                    super_vertex.count = 0
                    for induced_vertex in super_vertex.vertices:
                        if induced_vertex.program_point in instrumentation_points:
                            super_vertex.count = inferred_execution_counts[induced_vertex.program_point]
                            super_vertex.instrumented = True
                
                # Now compute all super block execution counts.
                depth_first_search = DepthFirstSearch(subgraph, 
                                                      subgraph.root_vertex,
                                                      False)
                for super_vertex in depth_first_search.post_order:
                    if super_vertex.number_of_predecessors() > 1:
                        pred_execution_count = 0
                        uninstrumented_pred_super_vertex = None
                        for pred_edge in super_vertex.predecessor_edge_iterator():
                            pred_super_vertex = subgraph.get_vertex(pred_edge.vertex_id)
                            if pred_super_vertex.instrumented:
                                pred_execution_count += pred_super_vertex.count
                            else:
                                assert uninstrumented_pred_super_vertex is None
                                uninstrumented_pred_super_vertex = pred_super_vertex
                        uninstrumented_pred_super_vertex.count = super_vertex.count - pred_execution_count
                    
                    if super_vertex.number_of_successors() > 1:
                        for _, successor_edges in super_vertex.successor_edge_partition_iterator():
                            for succ_edge in successor_edges:
                                succ_super_vertex = subgraph.get_vertex(succ_edge.vertex_id)
                                super_vertex.count += succ_super_vertex.count
                            if super_vertex.count > 0:
                                break
                
                for super_vertex in subgraph:
                    for induced_vertex in super_vertex.vertices:
                        inferred_execution_counts[induced_vertex.program_point] = super_vertex.count

    for key in actual_execution_counts.keys():
        if inferred_execution_counts[key] != actual_execution_counts[key]:
            print(key, 
                  actual_execution_counts[key], 
                  inferred_execution_counts[key])


def generate_trace(ppg: graph.ProgramPointGraph):
    trace = []
    v = ppg.entry
    while v != ppg.exit:
        trace.append(v)
        (e,) = random.sample(ppg.successors(v), 1)
        v = e.successor()
    trace.append(ppg.exit)
    return trace


def intra_procedural_analysis(prog, cfg, instrumentation_policy, traces):
    messages.debug_message('Analysing subprogram {}'.format(cfg.name))
    dot.visualise_control_flow_graph(prog, cfg)
    ppg = graph.ProgramPointGraph(cfg)
    dot.visualise_flow_graph(prog, ppg, '.ppg')
    ipg = graph.InstrumentationPointGraph.create_from_policy(ppg, instrumentation_policy)
    dot.visualise_instrumentation_point_graph(prog, ipg)

    for i in range(traces+1):
        messages.debug_message('Trace #{}'.format(i))
        trace = generate_trace(ppg)
        messages.debug_message(' '.join(str(v.program_point) for v in trace))
        true_count = {v: 0 for v in ppg.vertices}
        for v in trace:
            true_count[v] += 1

        filtered_trace = [v for v in trace if v in ipg.vertices]
        ipg_count = {v: 0 for v in ppg.vertices}
        state = ipg.entry
        for v in filtered_trace + [ipg.exit]:
            if v != ipg.exit:
                ipg_count[v] += 1

            for e in ipg.successors(state):
                if e.successor().program_point == v.program_point:
                    for p in e.path:
                        ipg_count[p] += 1
                    state = e.successor()
                    break

        for v in ppg.vertices:
            assert true_count[v] == ipg_count[v]


def main(**kwargs):
    prog = program.Program.IO.read(kwargs['filename'])

    for cfg in prog:
        intra_procedural_analysis(prog, cfg, kwargs['instrument'], kwargs['traces'])


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Instrument program to collect execution profiles at run time')

    parser.add_argument('filename',
                        help='a file containing program information')

    parser.add_argument('--instrument',
                        type=graph.InstrumentationPointGraph.Policy,
                        choices=list(graph.InstrumentationPointGraph.Policy),
                        required=True,
                        help='instrument vertices, edges or both')

    parser.add_argument('--traces',
                        type=int,
                        help='number of traces to generate',
                        default=1,
                        metavar='<INT>')

    return parser.parse_args()


if __name__ == '__main__':
    assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'
    main(**vars(parse_the_command_line()))
