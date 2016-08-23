from graphs import directed_graphs

import collections
import re
import sys


class Program:
    
    """
    Models a program as a set of graphs.  These graphs always include the
    control flow graphs of each function and a call graph.  Other graphs,
    like super block graphs, may be included as well, but it depends on
    the purpose of the analysis.
    """
    
    def __init__(self):
        self.the_call_graph               = directed_graphs.ContextGraph()
        self.control_flow_graphs          = collections.OrderedDict()
        self.state_transition_graphs      = collections.OrderedDict()
        self.loop_nesting_trees           = collections.OrderedDict()
        self.super_block_graphs           = collections.OrderedDict()
        self.instrumentation_point_graphs = collections.OrderedDict()
        
    
    def create_from_parsed_input(self, control_flow_graph_dict):
        pass
        

    def add_control_flow_graph(self, the_cfg):
        self.control_flow_graphs[the_cfg.name] = the_cfg 
        
    
    def control_flow_graph_iterator(self):
        for a_cfg in self.control_flow_graphs:
            yield a_cfg
            


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
    print(all_edges_in_program)
                    
if __name__ == "__main__":
    read_program_information_from_file(sys.argv[1]) 

