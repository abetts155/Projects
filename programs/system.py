import collections
import re
import sys

sys.path.append('/Users/abetts/Development/github')

from programs.graphs import directed_graphs
from programs.graphs import vertices



def read_program_information_from_file(file_name):
    all_edges_in_program = {}
    current_function     = None
    with open(file_name) as the_file:
        for line in the_file:
            if re.match(r'\S', line):
                # Transform line into lower case and then strip all whitespace
                line = ''.join(line.lower().split())
                if re.match(r'\w+', line):
                    current_function = line
                    all_edges_in_program[current_function] = []
                else:
                    line = line[1:len(line)-1]
                    source, destination, destination_function = line.split(',')
                    all_edges_in_program[current_function].append((source, 
                                                                   destination, 
                                                                   destination_function))
    program = Program()
    program.create_from_parsed_input(all_edges_in_program)



class Program:
    
    """
    Models a program as a set of graphs.  These graphs always include the
    control flow graphs of each function and a call graph.  Other graphs,
    like super block graphs, may be included as well, but it depends on
    the purpose of the analysis.
    """
    
    def __init__(self):
        self.call_graph                   = directed_graphs.CallGraph()
        self.control_flow_graphs          = collections.OrderedDict()
        self.state_transition_graphs      = collections.OrderedDict()
        self.loop_nesting_trees           = collections.OrderedDict()
        self.super_block_graphs           = collections.OrderedDict()
        self.instrumentation_point_graphs = collections.OrderedDict()
    
    def create_from_parsed_input(self, control_flow_graph_dict):
        def get_vertex_id(the_input):
            try:
                return int(the_input)
            except ValueError:
                raise ValueError('Unable to convert %r into a vertex id' % the_input)
        
        for function_name, edge_list in control_flow_graph_dict.items():
            # Create new call graph vertex
            self.call_graph.add_vertex(vertices.CallVertex
                                       (self.call_graph.get_new_vertex_id(),
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
                if an_edge[2] == control_flow_graph.name:
                    control_flow_graph.add_edge(pred_vertex, succ_vertex)
                else:
                    pred_call_vertex = self.call_graph.get_vertex(pred_id)
                    succ_call_vertex = self.call_graph.get_vertex_with_name(an_edge[2])
                    self.call_graph.add_edge(pred_call_vertex, succ_call_vertex)       
            self.control_flow_graphs[function_name] = control_flow_graph
            print(control_flow_graph)
            print(self.get_state_transition_graph(function_name))
        
    
    def get_control_flow_graph(self, function_name):
        try:
            return self.control_flow_graphs[function_name]
        except KeyError:
            raise KeyError('Unable to find control flow graph for function %s' 
                           % function_name)
            
            
    def get_state_transition_graph(self, function_name):
        if function_name not in self.state_transition_graphs:
            self.state_transition_graphs[function_name] = directed_graphs.\
                StateTransitionGraph(self.get_control_flow_graph(function_name))
        return self.state_transition_graphs[function_name]

                    
if __name__ == "__main__":
    read_program_information_from_file(sys.argv[1]) 

