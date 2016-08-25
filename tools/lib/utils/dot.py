"""
This modules outputs graph data structures of the program to DOT format and 
then immediately converts the file to PNG format for viewing.
"""

import os
import subprocess

from tools.lib.utils import debug
from tools.lib.utils import config
from tools.lib.system import directed_graphs



def make_file(graph, graph_name):
    if config.Arguments.dot:
        dot_filename = '%s%s%s.%s.%s' % (config.Arguments.basepath,
                                         os.sep, 
                                         config.Arguments.basename, 
                                         graph_name, 
                                         "dot")
        with open(dot_filename, 'w') as dot_file:
            dot_file.write('digraph {\n')
            if isinstance(graph, directed_graphs.ControlFlowGraph):
                write_control_flow_graph(dot_file, graph)
            elif isinstance(graph, directed_graphs.StateTransitionGraph):
                write_state_transition_graph(dot_file, graph)
            elif isinstance(graph, directed_graphs.CallGraph):
                write_call_graph(dot_file, graph)
            dot_file.write('}\n')
        
        output_to_png_file(dot_filename)
            
            
def write_control_flow_graph(dot_file, control_flow_graph):
    for basic_block in control_flow_graph:
        dot_file.write('%d -> {' % basic_block.vertex_id)
        for succ_edge in basic_block.successor_edge_iterator():
            dot_file.write('%d; ' % succ_edge.vertex_id)
        dot_file.write('}\n')
        

def write_state_transition_graph(dot_file, state_transition_graph):
    dot_file.write('node [shape=point]\n')
    for state in state_transition_graph:
        for succ_edge in state.successor_edge_iterator():
            if len(succ_edge.program_point) == 1:
                program_point_string = str(succ_edge.program_point[0])
            else:
                program_point_string = '%d, %d' % (succ_edge.program_point[0],
                                                   succ_edge.program_point[1])
            dot_file.write('%d -> %d [label ="%s"];\n' % (state.vertex_id,
                                                          succ_edge.vertex_id,
                                                          program_point_string))
        dot_file.write('\n')
        

def write_call_graph(dot_file, call_graph):
    for caller in call_graph:
        for succ_edge in caller.successor_edge_iterator():
            callee = call_graph.get_vertex(succ_edge.vertex_id)
            dot_file.write('%s -> %s  [label ="%s"];\n' % (caller.name,
                                                           callee.name,
                                                           succ_edge.call_sites))
        

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
            