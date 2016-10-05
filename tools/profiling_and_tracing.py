#!/usr/bin/env python3

import sys
assert sys.version_info >= (3,0), 'Script requires Python 3.0 or greater to run'

import argparse

from collections import Counter

from lib.utils import debug
from lib.utils import globals
from lib.system import environment
from lib.system.directed_graphs import InstrumentationPointGraph
from lib.system.directed_graphs import DepthFirstSearch
from lib.system.vertices import is_basic_block


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
        


def do_instrumentation_point_graph_instrumentation(control_flow_graph,
                                                   trace,
                                                   actual_execution_counts):    
    instrumentation_point_graph = InstrumentationPointGraph.\
                                    instrument_but_maintain_path_reconstructibility\
                                        (control_flow_graph)
    instrumentation_points = set({vertex.program_point 
                                  for vertex in instrumentation_point_graph})
    
    # Add a sentinel value to the end of the trace so that we transition to a final
    # state and count correctly.
    filtered_trace = [program_point for program_point in trace 
                      if program_point in instrumentation_points] +\
                      [None]
    debug.debug_message('Post-filter: {}'.format(filtered_trace), 
                        __name__)
    inferred_execution_counts = {}
    
    vertex = instrumentation_point_graph.get_entry_vertex()
    for program_point in filtered_trace:
        inferred_execution_counts[program_point] = 1 + inferred_execution_counts.\
                                                        setdefault(program_point, 0)
        for succ_edge in vertex.successor_edge_iterator():
            succ_vertex = instrumentation_point_graph.get_vertex(succ_edge.vertex_id)
            if succ_vertex.program_point == program_point:
                for tree_succ_edge in succ_edge.path_expression.root_vertex.successor_edge_iterator():
                    program_point_vertex = succ_edge.path_expression.get_vertex(tree_succ_edge.vertex_id)
                    inferred_execution_counts[program_point_vertex.
                                              program_point] = 1 + inferred_execution_counts.\
                                                                setdefault(program_point_vertex.program_point, 0)
                vertex = succ_vertex
                break
    
    for key in actual_execution_counts.keys():
        if inferred_execution_counts[key] != actual_execution_counts[key]:
            print('MISMATCH:',
                  key, 
                  actual_execution_counts[key], 
                  inferred_execution_counts[key])


def parse_the_command_line(): 
    parser = argparse.ArgumentParser(description=
                                     'Instrument program to collect execution '
                                     'profiles at run time')
    
    parser.add_argument('program_file',
                        help='a file containing program information'
                        ' (with .txt extension)')
    
    parser.add_argument('--instrument',
                        choices=['vertices', 'edges', 'mixed'],
                        required=True,
                        help='instrument vertices, edges, or both')
    
    parser.add_argument('--repeat',
                        type=int,
                        help='repeat the calculation this many times',
                        default=1,
                        metavar='<INT>')
    
    parser.add_argument('--functions',
                        nargs='*',
                        help='analyse these functions only')
    
    parser.add_argument('--super-blocks',
                        action='store_true',
                        help='use super blocks to work out where to place ' 
                        'instrumentaton',
                        default=False)
    
    parser.add_argument('--ipg',
                        action='store_true',
                        help='use instrumentation point graph to work out '
                        'where to place instrumentaton',
                        default=False)
    
    globals.add_common_command_line_arguments(parser)
    globals.args = vars(parser.parse_args())
    globals.set_filename_prefix(globals.args['program_file'])


if __name__ == '__main__': 
    parse_the_command_line()
    program = environment.create_program_from_input_file()

    for control_flow_graph in program:
        debug.debug_message('======> function {}'.format(control_flow_graph.name),
                            __name__)
        if globals.args['instrument'] == 'vertices':
            control_flow_graph.instrument_all_basic_blocks()
        elif globals.args['instrument'] == 'edges':
            control_flow_graph.instrument_all_control_flow_edges()
        elif globals.args['instrument'] == 'mixed':
            control_flow_graph.instrument_all_basic_blocks()
            control_flow_graph.instrument_all_control_flow_edges()
        else:
            assert False
    
        for repetition in range(1, globals.args['repeat']+1):
            trace = control_flow_graph.generate_trace(executions=10)
            debug.debug_message('Pre-filter: {}'.format(trace), 
                                __name__)
            actual_execution_counts = Counter(trace)
            if globals.args['super_blocks']:
                do_super_block_instrumentation(control_flow_graph, 
                                               trace,
                                               actual_execution_counts)
            if globals.args['ipg']:
                do_instrumentation_point_graph_instrumentation(control_flow_graph, 
                                                               trace,
                                                               actual_execution_counts)
                

        
    