
import random
import collections
import re

from tools.lib.system.directed_graphs import ControlFlowGraph, CallGraph
from tools.lib.system.vertices import ProgramPointVertex, SubprogramVertex
from tools.lib.utils import dot
from tools.lib.utils import config
from tools.lib.utils import debug



class UnknownInstrumentationPointError(Exception):
    
    """
    Exception to catch when an instrumented program point is not found in a
    control flow graph.
    """
    
    def __init__(self, message):
        Exception.__init__(self, message)
        
        
    
class TimingAnalysisData:
    
    """
    Maintains data required to do timing analysis on a control flow graph.
    """
    
    def __init__(self, control_flow_graph):
        self._vertex_to_execution_time = {}
        self._header_to_loop_bound = {}
        self.__create_execution_times(control_flow_graph)
        self.__create_loop_bound_data(control_flow_graph)
        
        
    def get_wcet(self, vertex):
        try:
            return self._vertex_to_execution_time[vertex]
        except KeyError:
            raise KeyError('No timing data for vertex {}'.
                           format(vertex.program_point))
            
        
    def get_loop_bound(self, header):
        try:
            return self._header_to_loop_bound[header]
        except KeyError:
            raise KeyError('No loop bound for header {}'.
                           format(header.program_point))
        
        
    def __create_execution_times(self, control_flow_graph):
        for vertex in control_flow_graph:
            if ProgramPointVertex.is_basic_block(vertex.program_point):
                self._vertex_to_execution_time[vertex] = random.randint(1, 20)
            else:
                self._vertex_to_execution_time[vertex] = 0
                
    
    def __create_loop_bound_data(self, control_flow_graph):
        loop_nesting_tree = control_flow_graph.get_loop_nesting_tree()
        maximum_loop_bound = random.randint(1, config.Arguments.max_loop_bound)
        
        def create_loop_bound_tuple_for_header(level, abstract_vertex):
            if level == 0:
                loop_bound_tuple = (1,)
            elif level == 1:
                loop_bound_tuple = (random.randint(1, maximum_loop_bound),)
            else:
                parent_abstract_vertex = loop_nesting_tree.get_vertex\
                                            (abstract_vertex.
                                             get_ith_predecessor_edge(0).
                                             vertex_id)
                parent_header = control_flow_graph.\
                                    get_vertex_for_program_point\
                                        (parent_abstract_vertex.program_point)
                
                loop_bound_tuple = ()                        
                for number_of_iterations in self._header_to_loop_bound[parent_header]:
                    for _ in range(1, number_of_iterations+1):
                        loop_bound_tuple += (random.randint(1, maximum_loop_bound),)
            
            header = control_flow_graph.get_vertex_for_program_point\
                        (abstract_vertex.program_point)
            self._header_to_loop_bound[header] = loop_bound_tuple
            debug.debug_message('{} {} {}'.format(control_flow_graph.name, 
                                                  header.program_point, 
                                                  loop_bound_tuple),
                                __name__)
        
        for level, tree_vertices in loop_nesting_tree.\
                                        level_by_level_iterator(False, True):
            for abstract_vertex in tree_vertices:
                if ProgramPointVertex.is_basic_block(abstract_vertex.program_point):
                    create_loop_bound_tuple_for_header(level, abstract_vertex)
            



class Program:
    
    """
    Models a program as a set of graphs.  These graphs always include the
    control flow graphs of each function and a call graph.  Other graphs,
    like a call-context graph, may be included as well, but it depends on
    the purpose of the analysis.
    """
    
    @staticmethod
    def create():
        program = Program()
        
        def get_vertex_id(value):
            try:
                return int(value)
            except ValueError:
                raise ValueError('Unable to convert {} into a vertex id'.\
                                 format(value))

        
        edges_in_control_flow_graphs = collections.OrderedDict()
        def create_control_flow_graphs(): 
            for function_name, edge_list in edges_in_control_flow_graphs.items():
                control_flow_graph = ControlFlowGraph(function_name)
                # Add vertices to control flow graph representing basic blocks
                for an_edge in edge_list:
                    pred_id = get_vertex_id(an_edge[0])
                    succ_id = get_vertex_id(an_edge[1])
                    if not control_flow_graph.has_vertex(pred_id):
                        control_flow_graph.add_vertex(ProgramPointVertex
                                                      (pred_id, pred_id))
                    if not control_flow_graph.has_vertex(succ_id):
                        control_flow_graph.add_vertex(ProgramPointVertex
                                                      (succ_id, succ_id))
                # Add vertices to control flow graph representing transitions and
                # then link these to basic blocks
                for an_edge in edge_list:
                    pred_id = get_vertex_id(an_edge[0])
                    succ_id = get_vertex_id(an_edge[1])
                    vertex = ProgramPointVertex\
                                (control_flow_graph.get_new_vertex_id(),
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
                exit_to_entry_vertex = ProgramPointVertex\
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
        
        
        instrumentation_in_control_flow_graphs = {}
        def set_instrumented_program_points():
            for function_name, instrumentation_points in\
                instrumentation_in_control_flow_graphs.items():
                control_flow_graph = program.get_control_flow_graph(function_name)
                for program_point in instrumentation_points:
                    if isinstance(program_point, tuple):
                        pred_id = get_vertex_id(program_point[0])
                        succ_id = get_vertex_id(program_point[1])
                        try:
                            program_point_vertex =\
                                control_flow_graph.get_vertex_for_program_point\
                                                    ((pred_id, succ_id))
                            program_point_vertex.instrumented = True
                        except KeyError:
                            raise UnknownInstrumentationPointError\
                                ('Program point ({},{}) does not belong to'
                                ' control flow graph {}'.format(pred_id, 
                                                                succ_id, 
                                                                function_name)) 
                    else:
                        vertex_id = get_vertex_id(program_point)
                        try:
                            program_point_vertex =\
                                control_flow_graph.get_vertex_for_program_point\
                                                    (vertex_id)
                            program_point_vertex.instrumented = True
                        except KeyError:
                            raise UnknownInstrumentationPointError\
                                ('Program point {} does not belong to'
                                ' control flow graph {}'.format(vertex_id,
                                                                function_name)) 
                dot.make_file(control_flow_graph)
                
        
        edges_in_call_graph = set()
        def create_call_graph():
            call_graph = CallGraph()
            for call_site_id, caller, callee in edges_in_call_graph:
                if not call_graph.has_vertex_with_name(caller):
                    call_graph.add_vertex(SubprogramVertex
                                          (call_graph.get_new_vertex_id(),
                                           caller))
                if not call_graph.has_vertex_with_name(callee):
                    call_graph.add_vertex(SubprogramVertex
                                          (call_graph.get_new_vertex_id(),
                                           callee))
                pred_call_vertex = call_graph.get_vertex_with_name(caller)
                succ_call_vertex = call_graph.get_vertex_with_name(callee)
                call_graph.add_edge(pred_call_vertex, 
                                    succ_call_vertex, 
                                    get_vertex_id(call_site_id))
            program.call_graph = call_graph
        
        
        current_function = None
        with open(config.Arguments.program_file) as the_file:
            for line in the_file:
                if re.match(r'\S', line):
                    # Transform line into lower case and then strip all whitespace
                    line = ''.join(line.lower().split())
                    if re.match(r'[a-zA-Z]\w+', line):
                        # Function name
                        current_function = line
                        edges_in_control_flow_graphs[current_function] = set()
                        instrumentation_in_control_flow_graphs[current_function] = set()
                    elif re.match(r'\{.*\}', line):
                        # Instrumented program points in this function
                        line = line[1:len(line)-1]
                        for program_point in line.split(','):
                            if re.match(r'\d+-\d+', program_point):
                                source, destination = program_point.split('-')
                                instrumentation_in_control_flow_graphs\
                                    [current_function].add((source, destination))
                            else:
                                instrumentation_in_control_flow_graphs\
                                    [current_function].add(program_point)
                    else:
                        # An edge in this function
                        source, destination = line.split('-')
                        if re.match(r'\d+', destination):
                            edges_in_control_flow_graphs[current_function].\
                                add((source, destination))
                        else:
                            edges_in_call_graph.add((source, 
                                                     current_function, 
                                                     destination))
                            
        create_control_flow_graphs()
        set_instrumented_program_points()
        create_call_graph()
        return program
        
    
    def __init__(self):
        self._call_graph = None
        self._control_flow_graphs = collections.OrderedDict()
        self._timing_analysis_data = {}
    
    
    @property
    def call_graph(self):
        return self._call_graph
    
    
    @call_graph.setter
    def call_graph(self, value):
        self._call_graph = value
    
    
    def add_control_flow_graph(self, control_flow_graph):
        assert control_flow_graph.name not in self._control_flow_graphs,\
            'Duplicate control flow graph with name {}'.\
            format(control_flow_graph.name)
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
            raise KeyError('Unable to find control flow graph for function {}'.\
                           format(function_name))
            
            
    def get_timing_data_for_function(self, function_name, redo=False):
        if function_name not in self._timing_analysis_data or redo:
            function_data = TimingAnalysisData\
                                (self.get_control_flow_graph(function_name))
            self._timing_analysis_data[function_name] = function_data
        return self._timing_analysis_data[function_name]
    
    
    def __len__(self):
        return len(self._control_flow_graphs)

