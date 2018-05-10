#!/usr/bin/env python3

import argparse
import random
import sys

from lib.utils import dot

from graphs import graph
from system import program
from utils import messages


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
        v = e.successor
    trace.append(ppg.exit)
    return trace


def check_traces(ppg: graph.ProgramPointGraph, ipg: graph.InstrumentationPointGraph, traces):
    for j in range(traces):
        trace = generate_trace(ppg)
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
                if e.successor.program_point == v.program_point:
                    for p in e.path:
                        ipg_count[p] += 1
                    state = e.successor
                    break

        for v in ppg.vertices:
            assert true_count[v] == ipg_count[v]


def inter_procedural_analysis(prog: program.Program, policy, reinstrument, traces):
    dfs = graph.DepthFirstSearch(prog.call_graph, prog.call_graph.root)
    for call_v in dfs.post_order():
        cfg = prog[call_v.name]
        messages.debug_message('Analysing subprogram {}'.format(cfg.name))
        dot.visualise_control_flow_graph(prog, cfg)
        ppg = graph.ProgramPointGraph(cfg)
        dot.visualise_flow_graph(prog, ppg, '.ppg')
        ipg = graph.InstrumentationPointGraph.create_from_policy(ppg, policy)
        ipg.reduce()
        dot.visualise_instrumentation_point_graph(prog, ipg)
        prog.add_ipg(ipg)
        for call_e in prog.call_graph.successors(call_v):
            for site in call_e.call_sites:
                print(call_v.name, call_e.successor.name, site)


def intra_procedural_analysis(prog, policy, reinstrument, traces):
    generated_ipgs = {cfg.name: [] for cfg in prog}
    for cfg in prog:
        messages.verbose_message('Analysing subprogram {}'.format(cfg.name))
        dot.visualise_control_flow_graph(prog, cfg)
        ppg = graph.ProgramPointGraph(cfg)
        dot.visualise_flow_graph(prog, ppg, '.ppg')

        for i in range(reinstrument):
            ipg = graph.InstrumentationPointGraph.create_from_policy(ppg, policy)
            ipg.name = '{}.{}'.format(ipg.name, len(generated_ipgs[cfg.name]))
            generated_ipgs[cfg.name].append(ipg)
            filters = [lambda v: ipg.predecessors(v) and
                                 ipg.successors(v) and
                                 len(ipg.predecessors(v)) == 1 and
                                 len(ipg.successors(v)) == 1,
                       lambda v: ipg.predecessors(v) and
                                 ipg.successors(v)]
            ipg.reduce(filters)
            #dot.visualise_instrumentation_point_graph(prog, ipg)

        for i in range(reinstrument):
            ipg = graph.InstrumentationPointGraph.create_from_policy(ppg, policy)
            ipg.name = '{}.{}'.format(ipg.name, len(generated_ipgs[cfg.name]))
            generated_ipgs[cfg.name].append(ipg)
            filters = [lambda v: True]
            ipg.reduce(filters)
            # dot.visualise_instrumentation_point_graph(prog, ipg)

    for name, ipgs in generated_ipgs.items():
        ipgs.sort(key=lambda ipg: ipg.number_of_vertices())
        ipgs.sort(key=lambda ipg: ipg.number_of_edges())
        print("===========================> {} candidates: {}".format(name, len(ipgs)))
        basic_ipg = graph.InstrumentationPointGraph.create_from_policy(ppg, policy)
        print('n={} m={}'.format(basic_ipg.number_of_vertices(), basic_ipg.number_of_edges()))
        smallest_instrumentation = min(ipgs, key=lambda ipg: ipg.number_of_vertices())
        print('min(|V|): {} n={} m={}'.format(smallest_instrumentation.name,
                                              smallest_instrumentation.number_of_vertices(),
                                              smallest_instrumentation.number_of_edges()))
        largest_instrumentation = max(ipgs, key=lambda ipg: ipg.number_of_vertices())
        print('max(|V|): {} n={} m={}'.format(largest_instrumentation.name,
                                              largest_instrumentation.number_of_vertices(),
                                              largest_instrumentation.number_of_edges()))

        smallest_footprint = min(ipgs, key=lambda ipg: ipg.number_of_edges())
        print('min(|E|): {} n={} m={}'.format(smallest_footprint.name,
                                              smallest_footprint.number_of_vertices(),
                                              smallest_footprint.number_of_edges()))

        largest_footprint = max(ipgs, key=lambda ipg: ipg.number_of_edges())
        print('max(|E|): {} n={} m={}'.format(largest_footprint.name,
                                              largest_footprint.number_of_vertices(),
                                              largest_footprint.number_of_edges()))

def main(**kwargs):
    prog = program.Program.IO.read(kwargs['filename'])
    if kwargs['interprocedural']:
        inter_procedural_analysis(prog,
                                  kwargs['policy'],
                                  kwargs['reinstrument'],
                                  kwargs['traces'])
    else:
        intra_procedural_analysis(prog,
                                  kwargs['policy'],
                                  kwargs['reinstrument'],
                                  kwargs['traces'])


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Instrument program to collect execution profiles at run time')

    parser.add_argument('filename',
                        help='a file containing program information')

    parser.add_argument('--policy',
                        type=graph.InstrumentationPointGraph.Policy,
                        choices=list(graph.InstrumentationPointGraph.Policy),
                        required=True,
                        help='instrument vertices, edges or both')

    parser.add_argument('--reinstrument',
                        type=int,
                        help='number of times to reinstrument a subprogram',
                        default=1,
                        metavar='<INT>')

    parser.add_argument('--traces',
                        type=int,
                        help='number of traces to generate',
                        default=1,
                        metavar='<INT>')

    parser.add_argument('--interprocedural',
                        action='store_true',
                        help='do interprocedural analysis',
                        default=False)

    return parser.parse_args()


if __name__ == '__main__':
    assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'
    main(**vars(parse_the_command_line()))
