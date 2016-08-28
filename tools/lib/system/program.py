import collections
import re

from tools.lib.system import directed_graphs
from tools.lib.system import vertices
from tools.lib.utils import dot



def read_program_information_from_file(file_name):
    edges_in__control_flow_graphs = collections.OrderedDict()
    edges_in__call_graph          = []
    current_function             = None
    with open(file_name) as the_file:
        for line in the_file:
            if re.match(r'\S', line):
                # Transform line into lower case and then strip all whitespace
                line = ''.join(line.lower().split())
                if re.match(r'\w+', line):
                    current_function = line
                    edges_in__control_flow_graphs[current_function] = []
                else:
                    line = line[1:len(line)-1]
                    source, destination = line.split(',')
                    if re.match(r'\d+', destination):
                        edges_in__control_flow_graphs[current_function].\
                            append((source, destination))
                    else:
                        edges_in__call_graph.append((source, 
                                                    current_function, 
                                                    destination))
    program = Program()
    program.create_from_parsed_input(edges_in__control_flow_graphs, 
                                     edges_in__call_graph)
    return program



class Program:
    
    """
    Models a program as a set of graphs.  These graphs always include the
    control flow graphs of each function and a call graph.  Other graphs,
    like a call-context graph, may be included as well, but it depends on
    the purpose of the analysis.
    """
    
    def __init__(self):
        self._call_graph          = directed_graphs.CallGraph()
        self._control_flow_graphs = collections.OrderedDict()
        
    
    def create_from_parsed_input(self, 
                                 edges_in__control_flow_graphs, 
                                 edges_in__call_graph):
        def get_vertex_id(the_input):
            try:
                return int(the_input)
            except ValueError:
                raise ValueError('Unable to convert %r into a vertex id' % the_input)
        
        for function_name, edge_list in edges_in__control_flow_graphs.items():
            # Create new call graph vertex
            self._call_graph.add_vertex(vertices.SubprogramVertex
                                       (self._call_graph.get_new_vertex_id(),
                                        function_name))
            
            # Create new control flow graph and add vertices to it
            control_flow_graph = directed_graphs.ControlFlowGraph(function_name)
            for an_edge in edge_list:
                pred_id = get_vertex_id(an_edge[0])
                succ_id = get_vertex_id(an_edge[1])
                if not control_flow_graph.has_vertex(pred_id):
                    control_flow_graph.add_vertex(vertices.Vertex(pred_id))
                if not control_flow_graph.has_vertex(succ_id):
                    control_flow_graph.add_vertex(vertices.Vertex(succ_id))
            # Add edges to the control flow graph and construct the call graph
            for an_edge in edge_list:
                pred_id = get_vertex_id(an_edge[0])
                succ_id = get_vertex_id(an_edge[1])
                pred_vertex = control_flow_graph.get_vertex(pred_id)
                succ_vertex = control_flow_graph.get_vertex(succ_id)
                control_flow_graph.add_edge(pred_vertex, succ_vertex)
            control_flow_graph.find_and_set_entry_vertex()
            control_flow_graph.find_and_set_exit_vertex()
            control_flow_graph.add_edge(control_flow_graph.exit_vertex,
                                        control_flow_graph.entry_vertex)
            self._control_flow_graphs[function_name] = control_flow_graph
            dot.make_file(control_flow_graph)
        
        # Create call graph edges
        for call_site_id, caller, callee in edges_in__call_graph:
            pred_call_vertex = self._call_graph.get_vertex_with_name(caller)
            succ_call_vertex = self._call_graph.get_vertex_with_name(callee)
            self._call_graph.add_edge(pred_call_vertex, 
                                     succ_call_vertex, 
                                     get_vertex_id(call_site_id))
        dot.make_file(self._call_graph)  
    
    
    def has_function(self, function_name):
        return function_name in self._control_flow_graphs
        
    
    def control_flow_graph_iterator(self):
        for _, control_flow_graph in self._control_flow_graphs.items():
            yield control_flow_graph
    
    
    def get_control_flow_graph(self, function_name):
        try:
            return self._control_flow_graphs[function_name]
        except KeyError:
            raise KeyError('Unable to find control flow graph for function %s' 
                           % function_name)
    
    
    def __len__(self):
        return len(self._control_flow_graphs)

