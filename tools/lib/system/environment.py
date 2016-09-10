
import collections
import re

from lib.system.directed_graphs import (ControlFlowGraph, 
                                              CallGraph)
from lib.system.vertices import (ProgramPointVertex, 
                                       SubprogramVertex)
from lib.utils import dot
from lib.utils import globals


def generate_program():
    program = Program()
    for function_id in range(1, globals.args['subprograms'] + 1):
        function_name = 'F{}'.format(function_id)
        control_flow_graph = ControlFlowGraph.create(function_name)
        program.add_control_flow_graph(control_flow_graph)
    return program


def write_program_to_file(program):
    with open(globals.args['program_file'], 'w') as the_file:
        for control_flow_graph in program.control_flow_graph_iterator():
            the_file.write('{}\n'.format(control_flow_graph.name))
            for vertex in control_flow_graph:
                if vertex != control_flow_graph.exit_vertex:
                    for succ_edge in vertex.successor_edge_iterator():
                        the_file.write('{}-{}\n'.format(vertex.vertex_id, 
                                                        succ_edge.vertex_id))
            the_file.write('\n')


def create_program_from_input_file():
    program = Program()
    
    def parse_int(value):
        try:
            return int(value)
        except ValueError:
            raise ValueError('Unable to convert {} into an integer'.\
                             format(value))

    
    edges_in_control_flow_graphs = collections.OrderedDict()
    def create_control_flow_graphs(): 
        for function_name, edge_list in edges_in_control_flow_graphs.items():
            control_flow_graph = ControlFlowGraph(function_name)
            # Add vertices to control flow graph representing basic blocks
            for an_edge in edge_list:
                pred_id = parse_int(an_edge[0])
                succ_id = parse_int(an_edge[1])
                if not control_flow_graph.has_vertex(pred_id):
                    control_flow_graph.add_vertex(ProgramPointVertex
                                                  (pred_id, pred_id))
                if not control_flow_graph.has_vertex(succ_id):
                    control_flow_graph.add_vertex(ProgramPointVertex
                                                  (succ_id, succ_id))
            # Add vertices to control flow graph representing transitions and
            # then link these to basic blocks
            for an_edge in edge_list:
                pred_id = parse_int(an_edge[0])
                succ_id = parse_int(an_edge[1])
                vertex = ProgramPointVertex\
                            (control_flow_graph.get_new_vertex_id(),
                             (pred_id, succ_id))
                control_flow_graph.add_vertex(vertex)
                control_flow_graph.add_edge(control_flow_graph.get_vertex(pred_id),
                                            vertex)
                control_flow_graph.add_edge(vertex,
                                            control_flow_graph.get_vertex(succ_id))
            # Find entry and exit vertex, then add vertex representing an edge 
            # from the exit vertex to the entry vertex 
            control_flow_graph.set_entry_vertex()
            control_flow_graph.set_exit_vertex()
            exit_to_entry_edge = (control_flow_graph.exit_vertex.vertex_id,
                                  control_flow_graph.entry_vertex.vertex_id)
            exit_to_entry_vertex = ProgramPointVertex\
                                    (control_flow_graph.get_new_vertex_id(),
                                     exit_to_entry_edge)
            control_flow_graph.add_vertex(exit_to_entry_vertex)
            control_flow_graph.add_edge(control_flow_graph.exit_vertex,
                                        exit_to_entry_vertex)
            control_flow_graph.add_edge(exit_to_entry_vertex,
                                        control_flow_graph.entry_vertex)
            # This may seem weird but bear with me.  The reason we set the
            # exit of the control flow graph to the control flow edge is that 
            # this edge always executes last, even though technically it does 
            # not actually exist in the program.  Really, this makes sures the 
            # post-dominator tree is correct.
            control_flow_graph.exit_vertex = exit_to_entry_vertex
            program[function_name] = control_flow_graph
            dot.make_file(control_flow_graph)
    
    
    edges_in_call_graph = set()
    def create_call_graph():
        for call_site_id, caller, callee in edges_in_call_graph:
            pred_call_vertex = program.call_graph.get_vertex_with_name(caller)
            succ_call_vertex = program.call_graph.get_vertex_with_name(callee)
            program.call_graph.add_edge(pred_call_vertex, 
                                        succ_call_vertex, 
                                        parse_int(call_site_id))
        dot.make_file(program.call_graph)
    
    
    function_name_regex = re.compile(r'[a-zA-Z]\w+')
    edge_regex = re.compile(r'\d+-\w+$')
    basic_block_id_regex = re.compile(r'\d+')
    program_property_regex = re.compile(r'(\d+|\d+-\d+)\.[a-zA-Z]+')
    delimiter_regex = re.compile(r'(\-|\.|=)')
        
    def first_pass_of_input_file():
        with open(globals.args['program_file']) as the_file:
            for line in the_file:
                if re.match(r'\S', line):
                    line = ''.join(line.lower().split())
                    if function_name_regex.match(line):
                        # Function name
                        current_function = line
                        edges_in_control_flow_graphs[current_function] = set()
                    elif edge_regex.match(line): 
                        # An edge in this function
                        source, destination = line.split('-')
                        if basic_block_id_regex.match(destination):
                            edges_in_control_flow_graphs[current_function].\
                                add((source, destination))
                        else:
                            edges_in_call_graph.add((source, 
                                                     current_function, 
                                                     destination))
                        
                        
    def second_pass_of_input_file():
        with open(globals.args['program_file']) as the_file:
            for line in the_file:
                if re.match(r'\S', line):
                    line = ''.join(line.lower().split())
                    if function_name_regex.match(line):
                        # Function name
                        current_function = line
                        control_flow_graph = program[current_function]
                    elif program_property_regex.match(line):
                        # A property concerning a program point
                        assert control_flow_graph, 'The property {} is not '
                        'attached to any control flow graph'.format(line)
                        
                        lexemes = delimiter_regex.split(line)
                        if len(lexemes) == 5:
                            # Vertex program point
                            program_point = parse_int(lexemes[0])
                            property_index = 2
                        else:
                            # Edge program point
                            program_point = (parse_int(lexemes[0]), 
                                             parse_int(lexemes[2]))
                            property_index = 4
                        
                        if lexemes[property_index] == 'instrument':
                            if lexemes[-1].startswith('t'):
                                control_flow_graph.\
                                program_point_data.\
                                set_instrumented(program_point, True)
                            else:
                                control_flow_graph.\
                                program_point_data.\
                                set_instrumented(program_point, False)
                        elif lexemes[property_index] == 'wcet':
                            control_flow_graph.\
                            program_point_data.\
                            set_wcet(program_point, parse_int(lexemes[-1]))
                        elif lexemes[property_index] == 'loop_bound':
                            assert lexemes[-1][0] == '(' \
                            and lexemes[-1][len(lexemes[-1])-1] == ')'
                            # Strip parentheses from tuple
                            loop_bound_tuple = lexemes[-1][1:len(lexemes[-1])-1]
                            loop_bound_tuple = tuple(parse_int(val) for val in 
                                                     loop_bound_tuple.split(','))
                            control_flow_graph.\
                            program_point_data.\
                            set_loop_bound(program_point, loop_bound_tuple)
                        else:
                            assert False, 'Unknown program point property {}'.\
                            format(lexemes[property_index])   
                                
    
    first_pass_of_input_file()  
    create_control_flow_graphs()
    create_call_graph()
    second_pass_of_input_file()
    return program



class Program:
    
    """
    Models a program as a set of graphs.  These graphs always include the
    control flow graphs of each function and a call graph.  Other graphs,
    like a call-context graph, may be included as well, but it depends on
    the purpose of the analysis.
    """
    
    def __init__(self):
        self._call_graph = CallGraph()
        self._control_flow_graphs = collections.OrderedDict()
        
    
    @property
    def call_graph(self):
        return self._call_graph
    
    
    @call_graph.setter
    def call_graph(self, value):
        self._call_graph = value
        
    
    def delete_unlisted_functions(self, functions_to_keep):
        if functions_to_keep is not None:
            functions_to_delete = [vertex.name for vertex in self._call_graph 
                                   if vertex.name not in functions_to_keep]
            for function_name in functions_to_delete:
                del self[function_name]
    
    
    def __delitem__(self, function_name):
        try:
            del self._control_flow_graphs[function_name]
            call_vertex = self._call_graph.get_vertex_with_name(function_name)
            self._call_graph.remove_vertex(call_vertex)
        except KeyError:
            raise KeyError('No function called {} to delete'.
                           format(function_name))
    
    
    def __setitem__(self, function_name, control_flow_graph):
        assert function_name not in self._control_flow_graphs,\
            'Duplicate control flow graph with name {}'.\
            format(function_name)
        self._control_flow_graphs[function_name] = control_flow_graph
        self._call_graph.add_vertex(SubprogramVertex
                                    (self._call_graph.get_new_vertex_id(),
                                     control_flow_graph.name))
    
    
    def __getitem__(self, function_name):
        try:
            return self._control_flow_graphs[function_name]
        except KeyError:
            raise KeyError('Unable to find control flow graph for function {}'.\
                           format(function_name))
    
    
    def __contains__(self, function_name):
        return function_name in self._control_flow_graphs
        
    
    def __iter__(self):
        for control_flow_graph in self._control_flow_graphs.values():
            yield control_flow_graph
    
    
    def __len__(self):
        return len(self._control_flow_graphs)

