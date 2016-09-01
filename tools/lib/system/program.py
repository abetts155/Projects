import collections
import re

from tools.lib.system import directed_graphs
from tools.lib.system import vertices
from tools.lib.utils import dot
from tools.lib.utils import config



class Program:
    
    """
    Models a program as a set of graphs.  These graphs always include the
    control flow graphs of each function and a call graph.  Other graphs,
    like a call-context graph, may be included as well, but it depends on
    the purpose of the analysis.
    """
    
    @staticmethod
    def create():
        
        def get_vertex_id(the_input):
            try:
                return int(the_input)
            except ValueError:
                raise ValueError('Unable to convert %r into a vertex id' 
                                 % the_input)
        
        def create_control_flow_graphs(program,
                                       edges_in_control_flow_graphs): 
            for function_name, edge_list in edges_in_control_flow_graphs.items():
                control_flow_graph = directed_graphs.ControlFlowGraph(function_name)
                # Add vertices to control flow graph representing basic blocks
                for an_edge in edge_list:
                    pred_id = get_vertex_id(an_edge[0])
                    succ_id = get_vertex_id(an_edge[1])
                    if not control_flow_graph.has_vertex(pred_id):
                        control_flow_graph.add_vertex(vertices.\
                                                      ProgramPointVertex(pred_id, pred_id))
                    if not control_flow_graph.has_vertex(succ_id):
                        control_flow_graph.add_vertex(vertices.\
                                                      ProgramPointVertex(succ_id, succ_id))
                # Add vertices to control flow graph representing transitions and
                # then link these to basic blocks
                for an_edge in edge_list:
                    pred_id = get_vertex_id(an_edge[0])
                    succ_id = get_vertex_id(an_edge[1])
                    vertex = vertices.ProgramPointVertex(control_flow_graph.get_new_vertex_id(),
                                                         (pred_id, succ_id))
                    control_flow_graph.add_vertex(vertex)
                    control_flow_graph.add_edge(control_flow_graph.get_vertex(pred_id),
                                                vertex,
                                                None)
                    control_flow_graph.add_edge(vertex,
                                                control_flow_graph.get_vertex(succ_id),
                                                None)
                # Find entry and exit vertex, then add vertex representing an edge 
                # from the exit vertex to the entry vertex 
                entry_vertex = control_flow_graph.find_entry_vertex()
                exit_vertex = control_flow_graph.find_exit_vertex()
                exit_to_entry_edge = (exit_vertex.vertex_id,
                                      entry_vertex.vertex_id)
                exit_to_entry_vertex = vertices.ProgramPointVertex\
                                            (control_flow_graph.get_new_vertex_id(),
                                             exit_to_entry_edge)
                control_flow_graph.add_vertex(exit_to_entry_vertex)
                control_flow_graph.add_edge(exit_vertex,
                                            exit_to_entry_vertex,
                                            None)
                control_flow_graph.add_edge(exit_to_entry_vertex,
                                            entry_vertex,
                                            None)
                control_flow_graph.entry_vertex = entry_vertex
                # This may seem weird but bear with me.  The reason we set the
                # exit of the control flow graph to the control flow edge is that 
                # this edge always executes last, even though technically it does 
                # not actually exist in the program.  Really, this makes sures the 
                # post-dominator tree is correct.
                control_flow_graph.exit_vertex = exit_to_entry_vertex
                program.add_control_flow_graph(control_flow_graph)
                control_flow_graph.get_super_block_graph()
        
        
        def create_call_graph(program, edges_in_call_graph):
            call_graph = directed_graphs.CallGraph()
            for call_site_id, caller, callee in edges_in_call_graph:
                if not call_graph.has_vertex_with_name(caller):
                    call_graph.add_vertex(vertices.SubprogramVertex\
                                          (call_graph.get_new_vertex_id(),
                                           caller))
                if not call_graph.has_vertex_with_name(callee):
                    call_graph.add_vertex(vertices.SubprogramVertex\
                                          (call_graph.get_new_vertex_id(),
                                           callee))
                pred_call_vertex = call_graph.get_vertex_with_name(caller)
                succ_call_vertex = call_graph.get_vertex_with_name(callee)
                call_graph.add_edge(pred_call_vertex, 
                                    succ_call_vertex, 
                                    get_vertex_id(call_site_id))
            program.call_graph = call_graph
        
        edges_in_control_flow_graphs = collections.OrderedDict()
        edges_in_call_graph = []
        current_function = None
        with open(config.Arguments.program_file) as the_file:
            for line in the_file:
                if re.match(r'\S', line):
                    # Transform line into lower case and then strip all whitespace
                    line = ''.join(line.lower().split())
                    if re.match(r'\w+', line):
                        current_function = line
                        edges_in_control_flow_graphs[current_function] = []
                    else:
                        line = line[1:len(line)-1]
                        source, destination = line.split(',')
                        if re.match(r'\d+', destination):
                            edges_in_control_flow_graphs[current_function].\
                                append((source, destination))
                        else:
                            edges_in_call_graph.append((source, 
                                                        current_function, 
                                                        destination))
                            
        program = Program()
        create_control_flow_graphs(program, edges_in_control_flow_graphs)
        create_call_graph(program, edges_in_call_graph)
        return program
        
    
    def __init__(self):
        self._call_graph = None
        self._control_flow_graphs = collections.OrderedDict()
    
    
    @property
    def call_graph(self):
        return self._call_graph
    
    
    @call_graph.setter
    def call_graph(self, value):
        self._call_graph = value
    
    
    def add_control_flow_graph(self, control_flow_graph):
        assert control_flow_graph.name not in self._control_flow_graphs,\
            'Duplicate control flow graph with name %s' % control_flow_graph.name
        dot.make_file(control_flow_graph)
        self._control_flow_graphs[control_flow_graph.name] = control_flow_graph
    
    
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

