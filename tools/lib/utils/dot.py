"""
This modules outputs graph data structures of the program to DOT format and 
then immediately converts the file to PNG format for viewing.
"""

import os
import subprocess

from tools.lib.utils import debug
from tools.lib.utils import config
from tools.lib.system import directed_graphs
from tools.lib.system import vertices



def make_file(graph, graph_name):
    if config.Arguments.dot:
        dot_filename = '%s%s%s.%s.%s' % (config.Arguments.basepath,
                                         os.sep, 
                                         config.Arguments.basename, 
                                         graph_name, 
                                         "dot")
        with open(dot_filename, 'w') as dot_file:
            dot_file.write('digraph {\n')
            if isinstance(graph, directed_graphs.StateTransitionGraph):
                write_state_transition_graph(dot_file, graph)
            elif isinstance(graph, directed_graphs.LoopNestingHierarchy):
                write_loop_nesting_tree(dot_file, graph)
            elif isinstance(graph, directed_graphs.CallGraph):
                write_call_graph(dot_file, graph)
            elif isinstance(graph, directed_graphs.ControlFlowGraph):
                write_flow_graph(dot_file, graph)
            elif isinstance(graph,directed_graphs.Dominators):
                write_dominator_tree(dot_file, graph)
            dot_file.write('}\n')
        # Create PNG file
        output_to_png_file(dot_filename)
        if not config.Arguments.debug:
            # We only need the DOT file for debugging purposes
            os.remove(dot_filename)
        

def write_state_transition_graph(dot_file, state_transition_graph):
    for state in state_transition_graph:
        for succ_edge in state.successor_edge_iterator():
            dot_file.write('%d -> %d [label ="%s"];\n' % (state.vertex_id,
                                                          succ_edge.vertex_id,
                                                          succ_edge.path_expression))
        dot_file.write('\n')
        
        
def write_loop_nesting_tree(dot_file, loop_nesting_tree):
    for vertex in loop_nesting_tree:
        if isinstance(vertex, vertices.LoopHeaderVertex):
            dot_file.write('%d [shape=triangle, style=filled, fillcolor=red,'
                           ' label="%d"];\n' % (vertex.vertex_id, 
                                                vertex.header_id))
        elif isinstance(vertex, vertices.TransitionVertex):
            dot_file.write('%d [label="%s"];\n' % (vertex.vertex_id, 
                                                   vertex.transition))
    for vertex in loop_nesting_tree:
        for succ_edge in vertex.successor_edge_iterator():
            succ_vertex = loop_nesting_tree.get_vertex(succ_edge.vertex_id)
            if isinstance(succ_vertex, vertices.LoopHeaderVertex)\
            or isinstance(succ_vertex, vertices.TransitionVertex):
                dot_file.write('%d -> %d;\n' % (vertex.vertex_id,
                                               succ_edge.vertex_id))
        

def write_call_graph(dot_file, call_graph):
    for caller in call_graph:
        for succ_edge in caller.successor_edge_iterator():
            callee = call_graph.get_vertex(succ_edge.vertex_id)
            dot_file.write('%s -> %s  [label ="%s"];\n' % (caller.name,
                                                           callee.name,
                                                           succ_edge.call_sites))
            
            
def write_flow_graph(dot_file, flow_graph):
    for vertex in flow_graph:
        dot_file.write('%d -> {' % vertex.vertex_id)
        for succ_edge in vertex.successor_edge_iterator():
            dot_file.write('%d; ' % succ_edge.vertex_id)
        dot_file.write('}\n')
        

def write_dominator_tree(dot_file, dominator_tree):
    for vertex in dominator_tree:
        if isinstance(vertex, vertices.TransitionVertex):
            dot_file.write('%d [label="%s"];\n' % (vertex.vertex_id, 
                                                   vertex.transition))
        else:
            dot_file.write('%d [shape=point];\n' % vertex.vertex_id)
    for vertex in dominator_tree:
        dot_file.write('%d -> {' % vertex.vertex_id)
        for succ_edge in vertex.successor_edge_iterator():
            dot_file.write('%d; ' % succ_edge.vertex_id)
        dot_file.write('}\n')
        

def output_to_png_file(dot_filename):
    png_filename = os.path.splitext(dot_filename)[0] + '.png'
    with open(png_filename, 'w') as png_file:
        cmd  = 'dot -Tpng %s' % dot_filename 
        proc = subprocess.Popen(cmd,
                                shell=True,
                                stdout=png_file,
                                stderr=subprocess.PIPE)
        _, _ = proc.communicate()
        if proc.returncode != 0:
            debug.exit_message('Running "%s" failed' % cmd)
            