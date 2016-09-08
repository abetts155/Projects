"""
This modules outputs graph data structures of the program to DOT format and 
then immediately converts the file to PNG format for viewing.
"""

import os
import subprocess

from lib.system import directed_graphs
from lib.system import vertices
from lib.utils import config
from lib.utils import debug


def make_file(graph):
    if config.Arguments.graphviz:
        dot_filename = graph.dot_filename() + '.dot'
        with open(dot_filename, 'w') as dot_file:
            dot_file.write('digraph {\n')
            dot_file.write('ranksep=0.3;\n')
            dot_file.write('nodesep=0.25;\n')
            if isinstance(graph, directed_graphs.LoopNestingHierarchy):
                write_loop_nesting_tree(dot_file, graph)
            elif isinstance(graph, directed_graphs.SuperBlockGraph):
                write_super_block_graph(dot_file, graph)
            elif isinstance(graph, directed_graphs.CallGraph):
                write_call_graph(dot_file, graph)
            elif isinstance(graph, directed_graphs.ControlFlowGraph):
                write_control_flow_graph(dot_file, graph)
            elif isinstance(graph, directed_graphs.Dominators):
                write_dominator_tree(dot_file, graph)
            elif isinstance(graph, directed_graphs.PathExpression):
                write_pass_expression(dot_file, graph)
            dot_file.write('}\n')
        # Create PNG file
        output_to_png_file(dot_filename)
        if not config.Arguments.debug:
            # We only need the DOT file for debugging purposes
            os.remove(dot_filename)

        
def write_loop_nesting_tree(dot_file, loop_nesting_tree):
    for vertex in loop_nesting_tree:
        if vertex.abstract:
            color = 'orange' if isinstance(vertex.program_point, int) else 'cornsilk'
            dot_file.write('%d [shape=triangle, style=filled, fillcolor=%s,'
                           ' label="%r"];\n' % (vertex.vertex_id,
                                                color,
                                                vertex.program_point))
        else:
            dot_file.write('%d [label="%s"];\n' % (vertex.vertex_id,
                                                   vertex.program_point))
            
        for succ_edge in vertex.successor_edge_iterator():
            dot_file.write('%d -> %d;\n' % (vertex.vertex_id,
                                            succ_edge.vertex_id))
        

def write_call_graph(dot_file, call_graph):
    for caller in call_graph:
        dot_file.write('{} [label ="{}"];\n'.format(caller.vertex_id,
                                                    caller.name))
        for succ_edge in caller.successor_edge_iterator():
            callee = call_graph.get_vertex(succ_edge.vertex_id)
            dot_file.write('%d -> %d [label ="%s"];\n' % (caller.vertex_id,
                                                          callee.vertex_id,
                                                          succ_edge.call_sites))
            
            
def write_control_flow_graph(dot_file, control_flow_graph):
    
    def write_vertex(vertex):
        color = 'white'
        if vertex.instrumented:
            color = 'yellow'
        dot_file.write('%d [label="%r", style=filled, fillcolor=%s];\n' 
                       % (vertex.vertex_id,
                          vertex.program_point,
                          color)) 
        
        for succ_edge in vertex.successor_edge_iterator():
            dot_file.write('%d -> %d [label ="%s"];\n' % (vertex.vertex_id,
                                                          succ_edge.vertex_id,
                                                          succ_edge.path_expression))
    
    
    if control_flow_graph.entry_vertex is not None:
        write_vertex(control_flow_graph.entry_vertex)
    for vertex in control_flow_graph:
        if vertex != control_flow_graph.entry_vertex:
            write_vertex(vertex)
        
        

def write_dominator_tree(dot_file, dominator_tree):
    
    def write_write(vertex):
        dot_file.write('%d [label="%r"];\n' % (vertex.vertex_id,
                                               vertex.program_point))
        
        for succ_edge in vertex.successor_edge_iterator():
            dot_file.write('%d -> %d;\n' % (vertex.vertex_id,
                                            succ_edge.vertex_id))
         
    
    write_write(dominator_tree.root_vertex)
    for vertex in dominator_tree:
        if vertex != dominator_tree.root_vertex:
            write_write(vertex)
        

def write_pass_expression(dot_file, path_expression):
    
    def write_vertex(vertex):
        if isinstance(vertex, vertices.RegularExpressionVertex):
            if vertex.operator == vertices.RegularExpressionVertex.SEQUENCE:
                label = 'SEQ'
                color = 'yellow'
            elif vertex.operator == vertices.RegularExpressionVertex.ALTERNATIVE:
                label = 'ALT'
                color = 'lightblue'
            else:
                label = 'LOOP'
                color = 'red'
            dot_file.write('{} [label={}, shape=triangle, fillcolor={}, style=filled];\n'.
                           format(vertex.vertex_id, label, color))
        else:            
            dot_file.write('%d [label="%r"];\n' % (vertex.vertex_id,
                                                   vertex.program_point))
            
        for succ_edge in vertex.successor_edge_iterator():
            dot_file.write('%d -> %d;\n' % (vertex.vertex_id,
                                            succ_edge.vertex_id)) 
    
    depth_first_search = directed_graphs.DepthFirstSearch(path_expression,
                                                          path_expression._root_vertex,
                                                          False)
    for vertex in depth_first_search.post_order:
        write_vertex(vertex)
        
        
def write_super_block_graph(dot_file, super_block_graph): 
    
    def write_vertex(vertex):
        vertex_label = ''
        for induced_vertex in vertex.vertices:
            vertex_label += str(induced_vertex.program_point)
            if induced_vertex == vertex.representative:
                vertex_label += ' *' 
            if induced_vertex.abstract:
                vertex_label += ' (A)'
            vertex_label += '\n'
        color = 'white'
        if vertex.is_loop_exit_edge:
            color = 'yellow'
        dot_file.write('%d [label="%s", style=filled, fillcolor=%s];\n' 
                       % (vertex.vertex_id, vertex_label, color))
        
        for succ_edge in vertex.successor_edge_iterator():
            dot_file.write('%d -> %d;\n' % (vertex.vertex_id,
                                            succ_edge.vertex_id))
    
    write_vertex(super_block_graph.root_vertex)
    for vertex in super_block_graph:
        if vertex != super_block_graph.root_vertex:
            write_vertex(vertex)
        

def output_to_png_file(dot_filename):
    png_filename = os.path.splitext(dot_filename)[0] + '.png'
    with open(png_filename, 'w') as png_file:
        cmd = 'dot -Tpng %s' % dot_filename 
        proc = subprocess.Popen(cmd,
                                shell=True,
                                stdout=png_file,
                                stderr=subprocess.PIPE)
        _, _ = proc.communicate()
        if proc.returncode != 0:
            debug.exit_message('Running "%s" failed' % cmd)
            
