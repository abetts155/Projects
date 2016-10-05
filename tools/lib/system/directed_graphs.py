"""
This module includes all directed graphs used in different types of analyses. 
"""


import collections
import abc
import random
import numpy

from lib.utils import dot
from lib.utils import debug
from lib.utils import globals

from lib.system.edges import (Edge,
                              TransitionEdge,
                              CallGraphEdge)

from lib.system.vertices import (Vertex, 
                                 ProgramPointVertex, 
                                 RegularExpressionVertex, 
                                 SuperBlock,
                                 is_basic_block)


        
class NoValidVertexError(Exception):
    
    """
    Exception to catch when we cannot find a vertex need to analyse a directed 
    graph, such as the entry or exit vertex or the root of a tree.
    """
    
    def __init__(self, message):
        Exception.__init__(self, message)
        
        

class DirectedGraph:
    
    """
    Models a graph with directed edges.
    """
    
    __metaclass__ = abc.ABCMeta

    def __init__(self, name):
        self._vertices = {}
        self._name = name
        self._reverse_graph = None
        self.__new_edge_id = 0
        

    @property
    def name(self):
        return self._name
        
        
    def get_new_vertex_id(self):
        vertex_id = 1
        while vertex_id in self._vertices.keys():
            vertex_id += 1 
        return vertex_id
    
    
    def get_new_edge_id(self):
        self.__new_edge_id += 1
        return self.__new_edge_id
    
        
    def add_vertex(self, vertex):
        assert vertex.vertex_id not in self._vertices,\
        'The graph already has vertex {}'.format(vertex.vertex_id)
        self._vertices[vertex.vertex_id] = vertex
        
        
    def remove_vertex(self, vertex):
        for pred_edge in vertex.predecessor_edge_iterator():
            pred_vertex = self.get_vertex(pred_edge.vertex_id)
            pred_vertex.remove_successor_edge(vertex.vertex_id)
        for succ_edge in vertex.successor_edge_iterator():
            succ_vertex = self.get_vertex(succ_edge.vertex_id)
            succ_vertex.remove_predecessor_edge(vertex.vertex_id)
        del self._vertices[vertex.vertex_id]
        
    
    def get_vertex(self, vertex_id):
        try:
            return self._vertices[vertex_id]
        except KeyError:
            raise KeyError('Vertex {} is not in the graph'.format(vertex_id))
        
        
    def has_vertex(self, vertex_id):
        return vertex_id in self._vertices
    
    
    def add_edge(self, pred_vertex, succ_vertex):
        edge_id = self.get_new_edge_id()
        pred_vertex.add_successor_edge(Edge(succ_vertex.vertex_id, 
                                            edge_id))
        succ_vertex.add_predecessor_edge(Edge(pred_vertex.vertex_id, 
                                              edge_id))
        
    
    def remove_edge(self, pred_vertex, succ_vertex):
        pred_vertex.remove_successor_edge(succ_vertex.vertex_id)
        succ_vertex.remove_predecessor_edge(pred_vertex.vertex_id)
        
    
    def number_of_vertices(self):
        return len(self._vertices)
    
    
    def number_of_edges(self):
        total = 0
        for v in self._vertices.values():
            total += v.number_of_successors()
        return total
        
    
    def __iter__(self):
        return self._vertices.values().__iter__()


    def __repr__(self):
        return '{}(vertices={})'.format(self.__class__.__name__,
                                        ' '.join(repr(vertex) 
                                                 for vertex in self))
    


class ProgramPointData:
    def __init__(self, control_flow_graph):
        self._control_flow_graph = control_flow_graph
        self._wcets = {}
        self._loop_bounds = {}
        self._instrumented = {}
    
    
    def set_wcet(self, program_point, value):
        self._wcets[program_point] = value
    
    
    def set_loop_bound(self, program_point, value):
        self._loop_bounds[program_point] = value
        
    
    def set_instrumented(self, program_point, value):
        self._instrumented[program_point] = value
        
    
    def get_wcet(self, program_point):
        try:
            return self._wcets[program_point]
        except KeyError:
            raise KeyError('No WCET value for program point {}'.
                           format(program_point))
        
    
    def get_loop_bound(self, program_point):
        try:
            return self._loop_bounds[program_point]
        except KeyError:
            raise KeyError('No loop bound for program point {}'.
                           format(program_point))
    
    
    def get_instrumented(self, program_point):
        try:
            return self._instrumented[program_point]
        except KeyError:
            if program_point is None:
                # This is a dummy vertex added for analysis purposes.  Assume
                # that it is not instrumented. 
                return False
            raise KeyError('No information on whether program point {} is'
                            ' instrumented'.format(program_point))
    
        
    def create_timing_data(self, redo=False):
        for vertex in self._control_flow_graph:
            if is_basic_block(vertex.program_point):
                if vertex.program_point not in self._wcets or redo:
                    self._wcets[vertex.program_point] = random.randint(1, 20)
            else:
                self._wcets[vertex.program_point] = 0
                    
        maximum_loop_bound = random.randint(1, globals.args['max_loop_bound'])
        def create_loop_bound_tuple_for_header(level, abstract_vertex):
            if level == 0:
                return numpy.array([1])
            elif level == 1:
                return numpy.array([random.randint(1, maximum_loop_bound)])
            else:
                parent_abstract_vertex = loop_nesting_tree.get_vertex\
                                            (abstract_vertex.
                                             get_ith_predecessor_edge(0).
                                             vertex_id)
                loop_bound = []                        
                for number_of_iterations in self._loop_bounds[parent_abstract_vertex.
                                                              program_point]:
                    for _ in range(1, number_of_iterations+1):
                        loop_bound.append(random.randint(1, maximum_loop_bound))
                return numpy.array(loop_bound)
    
        loop_nesting_tree = self._control_flow_graph.get_loop_nesting_tree()
        for level, tree_vertices in loop_nesting_tree.\
                                        level_by_level_iterator(False, True):
            for abstract_vertex in tree_vertices:
                if is_basic_block(abstract_vertex.program_point):
                    header = self._control_flow_graph.get_vertex_for_program_point\
                                (abstract_vertex.program_point)
                    if header not in self._loop_bounds or redo:
                        self._loop_bounds[header.program_point] =\
                            create_loop_bound_tuple_for_header(level, abstract_vertex)



class ControlFlowGraph(DirectedGraph):
    
    """
    Models a control flow graph.  We use vertices to represent BOTH basic blocks
    and transitions among basic blocks.  An edge in the control flow graph either
    shows that:
    a) the source is a basic block and the destination is a transition among basic 
    blocks such that the source basic block equals the first element of the 
    transition; or,
    b) the source is a transition among basic blocks and the destination is
    a basic block such that the second element of the transition equals the
    basic block.
    """
    
    def __init__(self, name):
        DirectedGraph.__init__(self, name)
        self._entry_vertex = None
        self._exit_vertex  = None
        self._program_point_to_vertex = {}
        self._basic_block_vertices = set()
        self._control_flow_edge_vertices = set()
        self._pre_dominator_tree = None
        self._post_dominator_tree = None
        self._loop_nesting_tree = None
        self._super_block_graphs = {}
        self._instrumentation_point_graph = None
        self._program_point_data = ProgramPointData(self)
        
        
    @property
    def entry_vertex(self):
        return self._entry_vertex
    
    
    @entry_vertex.setter
    def entry_vertex(self, value):
        self._entry_vertex = value
    
    
    @property
    def exit_vertex(self):
        return self._exit_vertex
    
    
    @exit_vertex.setter
    def exit_vertex(self, value):
        self._exit_vertex = value
        
    
    @property  
    def basic_block_vertices(self):
        return self._basic_block_vertices
    
    
    @property
    def control_flow_edge_vertices(self):
        return self._control_flow_edge_vertices
    
    
    @property
    def program_point_data(self):
        return self._program_point_data
    
    
    def set_entry_vertex(self):
        without_predecessors = [vertex for vertex in self 
                                if vertex.number_of_predecessors() == 0]
        if len(without_predecessors) == 0:
            raise NoValidVertexError('All vertices have at least one predecessor')
        elif len(without_predecessors) > 1:
            raise NoValidVertexError('Too many entry candidates found: {}'.\
                                     format(','.join(str(vertex.vertex_id) 
                                                     for vertex in without_predecessors)))
        else:
            self._entry_vertex = without_predecessors[0]
            
    
    def set_exit_vertex(self):
        without_successors = [vertex for vertex in self 
                              if vertex.number_of_successors() == 0]
        if len(without_successors) == 0:
            raise NoValidVertexError('All vertices have at least one successor')
        elif len(without_successors) > 1:
            raise NoValidVertexError('Too many exit candidates found: {}'.\
                                    format(','.join(str(vertex.vertex_id) 
                                                    for vertex in without_successors)))
        else:
            self._exit_vertex = without_successors[0]
    
    
    def add_vertex(self, vertex):
        DirectedGraph.add_vertex(self, vertex)
        if vertex.program_point is not None:
            # No program point means that the vertex is dummy
            self._program_point_to_vertex[vertex.program_point] = vertex
            if is_basic_block(vertex.program_point):
                self._basic_block_vertices.add(vertex)
            else:
                self._control_flow_edge_vertices.add(vertex)
            self.program_point_data.set_instrumented(vertex.program_point, False)
           
            
    def has_vertex_for_program_point(self, program_point):
        return program_point in self._program_point_to_vertex
    
    
    def get_vertex_for_program_point(self, program_point):
        try:
            return self._program_point_to_vertex[program_point]
        except KeyError:
            raise KeyError('No vertex found for program point {}'.\
                           format(program_point))
            
        
    def get_pre_dominator_tree(self):
        if self._pre_dominator_tree is None:
            self._pre_dominator_tree = Dominators(self, False)
            dot.make_file(self._pre_dominator_tree)
        return self._pre_dominator_tree
    
    
    def get_post_dominator_tree(self):
        if self._post_dominator_tree is None:
            self._post_dominator_tree = Dominators(self, True)
            dot.make_file(self._post_dominator_tree)
        return self._post_dominator_tree
    
            
    def get_loop_nesting_tree(self):
        if self._loop_nesting_tree is None:
            self._loop_nesting_tree = LoopNestingHierarchy(self)
            dot.make_file(self._loop_nesting_tree)
        return self._loop_nesting_tree
    
    
    def get_super_block_subgraph(self, abstract_vertex, redo=False):
        if abstract_vertex not in self._super_block_graphs or redo:
            subgraph = SuperBlockGraph.create_for_loop(self, abstract_vertex)
            self._super_block_graphs[abstract_vertex] = subgraph
            dot.make_file(subgraph)
        return self._super_block_graphs[abstract_vertex]

    
    def instrument_all_control_flow_edges(self):
        for vertex in self:
            if not is_basic_block(vertex.program_point):
                self.program_point_data.set_instrumented(vertex.program_point,
                                                         True)
            
                
                
    def instrument_all_basic_blocks(self):
        for vertex in self:
            if is_basic_block(vertex.program_point):
                self.program_point_data.set_instrumented(vertex.program_point,
                                                         True)
        
    
    def add_exit_to_entry_edge(self):
        exit_to_entry_edge = (self.exit_vertex.vertex_id,
                              self.entry_vertex.vertex_id)
        exit_to_entry_vertex = ProgramPointVertex\
                                (self.get_new_vertex_id(),
                                 exit_to_entry_edge)
        self.add_vertex(exit_to_entry_vertex)
        self.add_edge(self.exit_vertex, exit_to_entry_vertex)
        self.add_edge(exit_to_entry_vertex, self.entry_vertex)
        # The reason we set the exit of the control flow graph to the control 
        # flow edge is that this edge always executes last, even though 
        # technically it does not actually exist in the program.  Really, this 
        # makes sures the post-dominator tree is correct.
        self._exit_vertex = exit_to_entry_vertex
                
                
    def add_persistent_dummy_entry_and_exit_vertices(self):
        new_entry_vertex = ProgramPointVertex(self.get_new_vertex_id(), None)
        self.add_vertex(new_entry_vertex)
        self.add_edge(new_entry_vertex, self._entry_vertex)
        self._entry_vertex = new_entry_vertex
        new_exit_vertex = ProgramPointVertex(self.get_new_vertex_id(), None)
        self.add_vertex(new_exit_vertex)
        self.add_edge(self._exit_vertex, new_exit_vertex)
        self._exit_vertex = new_exit_vertex
        
        
    def get_vertices_on_frontier_of_loop_body(self, loop_body):
        loop_nesting_tree = self.get_loop_nesting_tree()
        loop_exits = set()
        loop_headers = set()
        for tree_vertex in loop_body:
            vertex = self.get_vertex(tree_vertex.vertex_id)
            for succ_edge in vertex.successor_edge_iterator():
                tree_succ_vertex = loop_nesting_tree.get_vertex\
                                    (succ_edge.vertex_id)
                if tree_succ_vertex not in loop_body:
                    if loop_nesting_tree.is_loop_header(tree_succ_vertex.program_point):
                        loop_headers.add(tree_succ_vertex)
                    else:
                        loop_exits.add(tree_succ_vertex)
        return loop_headers, loop_exits
    
    
    def induce_subgraph_for_loop_header(self, name, header_program_point):
        loop_nesting_tree = self.get_loop_nesting_tree()     
        loop_body = loop_nesting_tree.get_loop_body_for_program_point\
                        (header_program_point) 
        loop_headers, loop_exits = self.get_vertices_on_frontier_of_loop_body\
                                    (loop_body)
        induced_graph = ControlFlowGraph(name)
        
        # Add a vertex for each program point at this loop-nesting level.
        for tree_vertex in loop_body:
            induced_graph.add_vertex(ProgramPointVertex(tree_vertex.vertex_id,
                                                        tree_vertex.program_point))
        # Add edges between program points at this loop-nesting level.
        for tree_vertex in loop_body:
            cfg_vertex = self.get_vertex(tree_vertex.vertex_id)
            for succ_edge in cfg_vertex.successor_edge_iterator():
                succ_tree_vertex = loop_nesting_tree.get_vertex\
                                    (succ_edge.vertex_id)
                if succ_tree_vertex in loop_body\
                and succ_tree_vertex.program_point != header_program_point:  
                    vertex = induced_graph.get_vertex(tree_vertex.vertex_id)
                    succ_vertex = induced_graph.get_vertex(succ_edge.vertex_id)   
                    induced_graph.add_edge(vertex, succ_vertex)    
            
        # Add a vertex for each loop-exit program point out of this loop.
        for tree_vertex in loop_exits:
            induced_graph.add_vertex(ProgramPointVertex(tree_vertex.vertex_id,
                                                        tree_vertex.program_point))
        # Add edges between basic block source and loop-exit program point.
        for tree_vertex in loop_exits:
            succ_vertex = induced_graph.get_vertex(tree_vertex.vertex_id)
            vertex = induced_graph.get_vertex_for_program_point(succ_vertex.program_point[0])
            induced_graph.add_edge(vertex, succ_vertex)
        
        # Add an abstract vertex per inner loop.
        for tree_vertex in loop_headers:
            induced_graph.add_vertex(ProgramPointVertex(tree_vertex.vertex_id,
                                                        tree_vertex.program_point,
                                                        True))
        # Add edges between loop-entry program points and the inner loop header.
        loop_body_edge_program_points = [vertex for vertex in loop_body 
                                         if not is_basic_block
                                         (vertex.program_point)]
        for tree_vertex in loop_headers:
            succ_vertex = induced_graph.get_vertex(tree_vertex.vertex_id)
            source_tree_vertices = [vertex for vertex in loop_body_edge_program_points
                                    if vertex.program_point[1] == tree_vertex.program_point]
            for source_tree_vertex in source_tree_vertices:
                vertex = induced_graph.get_vertex(source_tree_vertex.vertex_id)
                induced_graph.add_edge(vertex, succ_vertex)
        # Add edges from each inner loop header to its loop-exit program points.
        for succ_vertex in induced_graph:
            if succ_vertex.program_point != header_program_point\
            and succ_vertex.number_of_predecessors() == 0:
                assert not is_basic_block(succ_vertex.program_point)   
                abstract_vertex = loop_nesting_tree.\
                                    get_header_abstract_vertex_for_program_point\
                                        (succ_vertex.program_point[0]) 
                for tree_vertex in loop_headers:
                    if tree_vertex.program_point == abstract_vertex.program_point:
                        vertex = induced_graph.get_vertex(tree_vertex.vertex_id)
                        induced_graph.add_edge(vertex, succ_vertex)
                        
        # Guarantee that there is a unique exit vertex
        without_successors = [vertex for vertex in induced_graph 
                              if vertex.number_of_successors() == 0]
        if len(without_successors) > 1:
            dummy_program_point_vertex = ProgramPointVertex\
                                            (induced_graph.get_new_vertex_id(),
                                             None)
            induced_graph.add_vertex(dummy_program_point_vertex)
            for vertex in without_successors:
                induced_graph.add_edge(vertex,
                                       dummy_program_point_vertex)
        
        induced_graph.set_entry_vertex()
        induced_graph.set_exit_vertex()
        dot.make_file(induced_graph)
        return induced_graph
    
    
    def generate_trace(self, executions=1):
        vertex_count = {vertex:0 for vertex in self}
        trace = []
        vertex = self._entry_vertex
        while True:
            vertex_count[vertex] += 1
            trace.append(vertex.program_point)
            
            if vertex == self._exit_vertex\
            and vertex_count[vertex] == executions:
                break
            
            # Move to random successor vertex.
            random_index = random.randint(0, vertex.number_of_successors()-1)
            succ_edge = vertex.get_ith_successor_edge(random_index)
            vertex = self.get_vertex(succ_edge.vertex_id)
        
        return trace
    
    
    def check_connected(self):
        reachable_from_entry = set()
        stack = [self._entry_vertex]
        while stack:
            stack_vertex = stack.pop()
            reachable_from_entry.add(stack_vertex)
            for succ_edge in stack_vertex.successor_edge_iterator():
                succ_vertex = self.get_vertex(succ_edge.vertex_id)
                if succ_vertex not in reachable_from_entry:
                    stack.append(succ_vertex)
                    
        reachable_from_exit = set()
        stack = [self._exit_vertex]
        while stack:
            stack_vertex = stack.pop()
            reachable_from_exit.add(stack_vertex)
            for pred_edge in stack_vertex.predecessor_edge_iterator():
                pred_vertex = self.get_vertex(pred_edge.vertex_id)
                if pred_vertex not in reachable_from_exit:
                    stack.append(pred_vertex)
        
        unreachable = [vertex for vertex in self 
                       if vertex in reachable_from_entry 
                       or vertex in reachable_from_exit]
        assert unreachable, 'The following vertices cannot be reached from the'
        ' entry or cannot reach the exit vertex: {}'.format(','.join(str(vertex.vertex_id) 
                                                                     for vertex in unreachable))
    
    
    def dot_filename(self):
        return '{}.cfg'.format(self.name)
    
    
    def __str__(self):
        def write_edges(edge_iterator, total_number_of_edges):
            value = ''
            counter = 1
            for edge in edge_iterator:
                value += '{}'.format(edge.vertex_id)
                if counter < total_number_of_edges:
                    value += ', '
                counter += 1
            return value
        
        value = 'CFG: {}\n'.format(self._name)
        value += '{\n'
        for vertex in self:
            value += 'vertex(id={}, program_point={}, abstract={})\n'.\
                        format(vertex.vertex_id, 
                               vertex.program_point,
                               vertex.abstract)
            value += '  pred = {{{}}}\n'.\
                        format(write_edges(vertex.predecessor_edge_iterator(),
                                           vertex.number_of_predecessors()))      
            value += '  succ = {{{}}}\n'.\
                        format(write_edges(vertex.successor_edge_iterator(),
                                           vertex.number_of_successors()))
        value += '}\n'
        return value



class InstrumentationPointGraph(DirectedGraph):
    
    @staticmethod
    def instrument_as_per_user_instruction(control_flow_graph):
        instrumentation_point_graph = InstrumentationPointGraph(control_flow_graph.name)
        instrumentation_point_graph._reduce_for_timing_analysis(control_flow_graph)
        return instrumentation_point_graph
    
    
    @staticmethod
    def instrument_but_maintain_path_reconstructibility(control_flow_graph):
        instrumentation_point_graph = InstrumentationPointGraph(control_flow_graph.name)
        instrumentation_point_graph._reduce_for_profiling_and_tracing(control_flow_graph)
        return instrumentation_point_graph
        
    
    def __init__(self, name):
        DirectedGraph.__init__(self, name)
        self._program_point_to_vertex = {}
        self._program_point_to_removed_vertex = {}
        
        
    def _reduce_for_profiling_and_tracing(self, control_flow_graph):
        # We use a temporary state transition to path expression mapping to
        # ensure linear growth of path expression trees. 
        state_transition_to_path_expression = {}
        
        def connect_predecessors_to_successors(vertex):
            """
            For this vertex, connect each of its predecessor to each of its 
            successors.
            """
            for pred_edge in vertex.predecessor_edge_iterator():
                pred_vertex = self.get_vertex(pred_edge.vertex_id)
                for succ_edge in vertex.successor_edge_iterator():
                    succ_vertex = self.get_vertex(succ_edge.vertex_id)
                    state_transition_to_path_expression[(pred_vertex, succ_vertex)]\
                        = state_transition_to_path_expression[(pred_vertex, vertex)]\
                        + [vertex.program_point]\
                        + state_transition_to_path_expression[(vertex, succ_vertex)]
                    self.add_edge(pred_vertex, succ_vertex, None)
                    
        
        def can_remove_program_point(vertex):
            """
            Is it possible to remove this program point while maintaining path
            reconstructibility?
            """
            if vertex.program_point is None:
                return False
            
            for pred_edge in vertex.predecessor_edge_iterator():
                pred_vertex = self.get_vertex(pred_edge.vertex_id)
                for succ_edge in vertex.successor_edge_iterator():
                    if pred_vertex.has_successor(succ_edge.vertex_id):
                        return False
            return True
                    
                    
        # Do not destroy the original control flow graph structure.
        self.__copy_control_flow_graph(control_flow_graph)
        
        entry_vertex = ProgramPointVertex(self.get_new_vertex_id(), None)
        self.add_vertex(entry_vertex)
        exit_vertex = ProgramPointVertex(self.get_new_vertex_id(), None)
        self.add_vertex(exit_vertex)
        self.add_edge(entry_vertex, 
                      self.get_vertex(control_flow_graph.entry_vertex.vertex_id), 
                      None)
        self.add_edge(self.get_vertex(control_flow_graph.exit_vertex.vertex_id), 
                      exit_vertex, 
                      None)
        
        
        # The following loop does two things:
        # 1) It removes unmonitored program points.
        # 2) It sets up the temporary state transition to path expression 
        # mapping. Initially the values in the mapping will be empty.
        program_points_to_remove = set()
        for vertex in self:
            if not control_flow_graph.program_point_data.get_instrumented(vertex.program_point)\
            and vertex.program_point:
                program_points_to_remove.add(vertex)
            
            for succ_edge in vertex.successor_edge_iterator():
                succ_vertex = self.get_vertex(succ_edge.vertex_id)
                assert succ_edge.path_expression is None,\
                       'Expected the path expression of the edge {}->{} to be '\
                       ' empty'.format(vertex.vertex_id, succ_edge.vertex_id)
                state_transition_to_path_expression[(vertex, succ_vertex)] = []
        
        # Maintain graph connectedness and remove each unneeded program point.
        for vertex in program_points_to_remove:
            connect_predecessors_to_successors(vertex)
            self.__remove_vertex(vertex)
            control_flow_graph.program_point_data.set_instrumented(vertex.program_point,
                                                                   False)
        
        
        # Reduce graph until removal of state violates path reconstructibility
        changed = True
        while changed:
            changed = False
            candidiates = list(self._vertices.values())
            random.shuffle(candidiates)
            for vertex in candidiates:
                if can_remove_program_point(vertex):
                    changed = True
                    connect_predecessors_to_successors(vertex)
                    self.__remove_vertex(vertex)
                    control_flow_graph.program_point_data.set_instrumented(vertex.program_point,
                                                                           False)
        
        
        # Create each path expression tree from a list of program points
        for vertex in self:
            for succ_edge in vertex.successor_edge_iterator():
                succ_vertex = self.get_vertex(succ_edge.vertex_id)
                pred_edge = succ_vertex.get_predecessor_edge(vertex.vertex_id)
                path_expression = PathExpression.create_sequence_from_list_of_program_points\
                    (state_transition_to_path_expression[(vertex, succ_vertex)], 
                     self._name)
                succ_edge.path_expression = path_expression
                pred_edge.path_expression = path_expression
                
        dot.make_file(self)
        
        
    def _reduce_for_timing_analysis(self, control_flow_graph):
        self.__copy_control_flow_graph(control_flow_graph)
        self.__scatter_loop_bounds(control_flow_graph.get_loop_nesting_tree(),
                                   control_flow_graph.program_point_data)
        self.__initialise_loop_properties(control_flow_graph.get_loop_nesting_tree())
        self.__reduce(control_flow_graph.get_loop_nesting_tree(), 
                      control_flow_graph.program_point_data)
    
    
    def get_entry_vertex(self):
        for vertex in self:
            if vertex.program_point is None and vertex.number_of_predecessors() == 0:
                return vertex
        assert False, 'Unable to find entry vertex of instrumentation point graph'
    
    def get_vertex_for_program_point(self, program_point):
        try:
            if program_point in self._program_point_to_vertex:
                return self._program_point_to_vertex[program_point]
            return self._program_point_to_removed_vertex[program_point]
        except KeyError:
            raise KeyError('No vertex found for program point {}'.
                           format(program_point))
    
        
    def add_edge(self, pred_vertex, succ_vertex, path_expression):
        edge_id = self.get_new_edge_id()
        pred_vertex.add_successor_edge(TransitionEdge(succ_vertex.vertex_id, 
                                                      edge_id, 
                                                      path_expression))
        succ_vertex.add_predecessor_edge(TransitionEdge(pred_vertex.vertex_id, 
                                                        edge_id,
                                                        path_expression))
        
    
    def __remove_vertex(self, vertex):
        DirectedGraph.remove_vertex(self, vertex)
        if vertex.program_point:
            del self._program_point_to_vertex[vertex.program_point]
            self._program_point_to_removed_vertex[vertex.program_point] = vertex
        
    
    def __copy_control_flow_graph(self, control_flow_graph):
        for vertex in control_flow_graph:
            vertex_copy = ProgramPointVertex(vertex.vertex_id,
                                             vertex.program_point)
            self.add_vertex(vertex_copy)
            self._program_point_to_vertex[vertex.program_point] = vertex_copy
        for vertex in control_flow_graph:
            for succ_edge in vertex.successor_edge_iterator():
                self.add_edge(self.get_vertex(vertex.vertex_id),
                              self.get_vertex(succ_edge.vertex_id),
                              None)
                
                
    def __scatter_loop_bounds(self,
                              loop_nesting_tree,
                              program_point_data):
        for level, the_vertices in loop_nesting_tree.level_by_level_iterator\
                                (abstract_vertices_only=True):
            for abstract_vertex in the_vertices:
                if is_basic_block(abstract_vertex.program_point):
                    loop_body = set()
                    loop_body_when_exiting = set()
                    
                    # The loop body returned by the loop-nesting tree contains
                    # tree type vertices.  For the analysis below, we want 
                    # vertices in the instrumentation point graph, so grab them.
                    for tree_vertex in loop_nesting_tree.\
                                        get_loop_body_for_program_point\
                                        (abstract_vertex.program_point):
                        vertex = self.get_vertex(tree_vertex.vertex_id)
                        loop_body.add(vertex)
                    
                    if level == 0:
                        # The outermost (dummy) loop: all vertices found in the
                        # loop body are part of the exit space.
                        loop_body_when_exiting = loop_body
                    else:
                        # Work out which vertices are sources of loop-exit edges
                        # and then travel backwards from those vertices.  During
                        # the search, any vertex met can feasibly be executed
                        # each time the loop executes its final iteration (with
                        # respect to the parent loop).
                        loop_exits = set()
                        for vertex in loop_body:
                            for succ_edge in vertex.successor_edge_iterator():
                                succ_vertex = self.get_vertex(succ_edge.vertex_id)
                                abstract_vertex_for_succ =\
                                    loop_nesting_tree.get_header_abstract_vertex_for_program_point\
                                        (succ_vertex.program_point)
                                if not loop_nesting_tree.is_ancestor(abstract_vertex, abstract_vertex_for_succ):
                                    loop_exits.add(vertex)
                                           
                        stack = [vertex for vertex in loop_exits]
                        while stack:
                            stack_vertex = stack.pop()
                            loop_body_when_exiting.add(stack_vertex)
                            for pred_edge in stack_vertex.predecessor_edge_iterator():
                                pred_vertex = self.get_vertex(pred_edge.vertex_id)
                                abstract_vertex_for_pred = loop_nesting_tree.\
                                                            get_header_abstract_vertex_for_program_point\
                                                                (pred_vertex.program_point)
                                if not loop_nesting_tree.is_loop_tail(pred_vertex.program_point)\
                                and loop_nesting_tree.is_ancestor(abstract_vertex, abstract_vertex_for_pred)\
                                and pred_vertex not in loop_body_when_exiting:
                                    stack.append(pred_vertex)
                    
                    # Strip off one iteration from the loop-bound tuple if the 
                    # vertex cannot be executed on the final iteration.
                    loop_bound_when_exiting = program_point_data.\
                                                get_loop_bound\
                                                    (abstract_vertex.program_point)
                    loop_bound_when_iterating = numpy.array(loop_bound_when_exiting,
                                                            copy=True)
                    for index, value in enumerate(loop_bound_when_iterating):
                        loop_bound_when_iterating[index] = value-1
                    for vertex in loop_body:
                        if vertex in loop_body_when_exiting:
                            program_point_data.set_loop_bound(vertex.program_point,
                                                              loop_bound_when_exiting)
                        else:
                            program_point_data.set_loop_bound(vertex.program_point,
                                                              loop_bound_when_iterating)
                    
                            
                    
        for vertex in self:
            if vertex.number_of_predecessors() == 1:
                for pred_edge in vertex.predecessor_edge_iterator():
                    pred_edge.loop_bound = program_point_data.get_loop_bound(vertex.program_point)
                
            if vertex.number_of_successors() == 1:
                for succ_edge in vertex.successor_edge_iterator():
                    succ_edge.loop_bound = program_point_data.get_loop_bound(vertex.program_point)
                    
        
        for vertex in self:
            if vertex.number_of_predecessors() > 1:
                pass

    
    def __initialise_loop_properties(self,
                                     loop_nesting_tree):
        for _, the_vertices in loop_nesting_tree.level_by_level_iterator\
                                (abstract_vertices_only=True):
            for abstract_vertex in the_vertices:
                if is_basic_block(abstract_vertex.program_point):
                    pass    
    
        
    def __reduce(self, loop_nesting_tree, program_point_data):
        
        """
        Eliminate program points that are not instrumented.  During this 
        reduction, reconnect the remaining program points if there is at 
        least one path between them in the original control flow graph.
        """
        
        def create_path_expression_for_edge(pred_vertex,
                                            succ_vertex,
                                            pred_to_vertex_edge,
                                            vertex_to_succ_edge,
                                            loop_expression):
            list_of_child_expressions = []
            if loop_expression is not None:
                list_of_child_expressions.append(loop_expression)
            if pred_to_vertex_edge.path_expression is not None:
                list_of_child_expressions.append(pred_to_vertex_edge.
                                                 path_expression)
            program_point_path_expression =\
                PathExpression.create_sequence_from_single_program_point\
                    (vertex.program_point, self._name)
            list_of_child_expressions.append(program_point_path_expression)
            if vertex_to_succ_edge.path_expression is not None:
                list_of_child_expressions.append(vertex_to_succ_edge.
                                                 path_expression)
            return PathExpression.create_sequence_of_path_expressions\
                    (list_of_child_expressions, self._name)
        
        
        def update_loop_information(pred_to_vertex_edge,
                                    vertex_to_succ_edge, 
                                    new_edge_tuple):
            pass
            
        
        def connect_predecessors_to_successors(vertex):
            """
            For this vertex, connect each of its predecessor to each of its 
            successors.
            """
            
            loop_expression = None
            if vertex.has_successor(vertex.vertex_id):
                self_loop_edge = vertex.get_successor_edge(vertex.vertex_id)
                loop_expression =\
                    PathExpression.create_loop_path_expression\
                        (self_loop_edge.path_expression, 
                         self._name)
                        
            for pred_edge in vertex.predecessor_edge_iterator():
                if pred_edge.vertex_id != vertex.vertex_id:
                    pred_vertex = self.get_vertex(pred_edge.vertex_id)
                    for succ_edge in vertex.successor_edge_iterator():
                        if succ_edge.vertex_id != vertex.vertex_id:
                            succ_vertex = self.get_vertex(succ_edge.vertex_id)
                            new_path_expression = create_path_expression_for_edge\
                                                    (pred_vertex, 
                                                     succ_vertex,
                                                     pred_edge,
                                                     succ_edge,
                                                     loop_expression)
                            
                            if not pred_vertex.has_successor(succ_edge.vertex_id):
                                self.add_edge(pred_vertex, 
                                              succ_vertex, 
                                              new_path_expression)
                            else:
                                pred_edge = succ_vertex.get_predecessor_edge\
                                                (pred_edge.vertex_id)
                                succ_edge = pred_vertex.get_successor_edge\
                                                (succ_edge.vertex_id)
                                alternative_expression =\
                                    PathExpression.create_alternative_of_two_path_expressions\
                                        (pred_edge.path_expression,
                                         new_path_expression,
                                         self._name)
                                pred_edge.path_expression = alternative_expression
                                succ_edge.path_expression = alternative_expression
                            
                            pred_edge = succ_vertex.get_predecessor_edge\
                                            (pred_vertex.vertex_id)
                            succ_edge = pred_vertex.get_successor_edge\
                                            (succ_vertex.vertex_id)
                            new_edge_tuple = (pred_edge, succ_edge)
                            update_loop_information(vertex.get_predecessor_edge(pred_vertex.vertex_id),
                                                    vertex.get_successor_edge(succ_vertex.vertex_id),
                                                    new_edge_tuple)                                    
        
        
        program_points_to_remove = [vertex for vertex in self 
                                    if not program_point_data.
                                    get_instrumented(vertex.program_point)]
        random.shuffle(program_points_to_remove)
        for vertex in program_points_to_remove:
            debug.debug_message('Removing program point {}'.
                                format(vertex.program_point), 
                                __name__)
            connect_predecessors_to_successors(vertex)
            self.__remove_vertex(vertex)
            
            
        for vertex in self:
            for succ_edge in vertex.successor_edge_iterator():
                succ_vertex = self.get_vertex(succ_edge.vertex_id)
                if succ_edge.path_expression:
                    succ_edge.path_expression.pred_vertex = vertex
                    succ_edge.path_expression.succ_vertex = succ_vertex
                    dot.make_file(succ_edge.path_expression)


    def dot_filename(self):
        return '{}.ipg'.format(self.name)



class CallGraph(DirectedGraph):
    
    """
    Models the call graph of a program.
    """
    
    def __init__(self):
        DirectedGraph.__init__(self, 'call')
        self._function_name_to_vertex = {}
        
        
    def add_vertex(self, vertex):
        DirectedGraph.add_vertex(self, vertex)
        self._function_name_to_vertex[vertex.name] = vertex
        
        
    def remove_vertex(self, vertex):
        for pred_edge in vertex.predecessor_edge_iterator():
            pred_vertex = self.get_vertex(pred_edge.vertex_id)
            pred_vertex.remove_successor_edge(vertex.vertex_id)
        for succ_edge in vertex.successor_edge_iterator():
            succ_vertex = self.get_vertex(succ_edge.vertex_id)
            succ_vertex.remove_predecessor_edge(vertex.vertex_id)
        del self._vertices[vertex.vertex_id]
        del self._function_name_to_vertex[vertex.name]
        
        
    def has_vertex_with_name(self, function_name):
        return function_name in self._function_name_to_vertex
        
    
    def get_vertex_with_name(self, function_name):
        try:
            return self._function_name_to_vertex[function_name]
        except ValueError:
            raise ValueError('No vertex found for function {}'.\
                             format(function_name))
        
    
    def add_edge(self, pred_vertex, succ_vertex, call_site_id):
        edge_id = self.get_new_edge_id()
        if not pred_vertex.has_successor(succ_vertex.vertex_id):
            succ_edge = CallGraphEdge(succ_vertex.vertex_id, edge_id) 
            pred_vertex.add_successor_edge(succ_edge)
        if not succ_vertex.has_predecessor(pred_vertex.vertex_id):
            pred_edge = CallGraphEdge(pred_vertex.vertex_id, edge_id)
            succ_vertex.add_predecessor_edge(pred_edge)
        succ_edge = pred_vertex.get_successor_edge(succ_vertex.vertex_id)
        pred_edge = succ_vertex.get_predecessor_edge(pred_vertex.vertex_id)
        succ_edge.call_sites.add(call_site_id)
        pred_edge.call_sites.add(call_site_id)
        
    
    def dot_filename(self):
        return '{}'.format(self._name)
        
        

class ContextGraph(DirectedGraph):
    
    """
    Models different execution contexts in the program.  The canonical example
    is a call graph where each context represents a function with an
    indistinguishable calling context.  But a context is a much broader concept
    that can encapsulate, e.g., a specific loop iteration containing a function
    call   
    """    
    
    def __init__(self):
        DirectedGraph.__init__(self, 'context')


        
class Tree(DirectedGraph):
    
    """
    Models a directed graph that does not contain cycles. 
    """        
    
    def __init__ (self, name):
        DirectedGraph.__init__(self, name)
        self._root_vertex = None
        self._least_common_ancestor_query = None
        self._level_to_vertices = collections.OrderedDict()
        
    
    @property
    def root_vertex(self):
        return self._root_vertex
    
    
    def level_by_level_iterator(self, up=True):
        if up:
            for level in reversed(sorted(self._level_to_vertices.keys())):
                yield level, self._level_to_vertices[level]
        else:
            for level in sorted(self._level_to_vertices.keys()):
                yield level, self._level_to_vertices[level]

    
    def is_ancestor(self, candidate_ancestor_vertex, vertex):
        if candidate_ancestor_vertex == vertex:
            return True
        elif vertex == self._root_vertex:
            return False
        else:
            parent = self.get_vertex(vertex.get_ith_predecessor_edge(0).vertex_id)
            while parent != self._root_vertex\
            and parent != candidate_ancestor_vertex:
                vertex = parent
                parent = self.get_vertex(vertex.get_ith_predecessor_edge(0).vertex_id)
            return parent == candidate_ancestor_vertex
        
    
    def is_proper_ancestor(self, candidate_ancestor_vertex, vertex):
        if candidate_ancestor_vertex == vertex:
            return False
        else:
            return self.is_ancestor(candidate_ancestor_vertex, vertex)
    
    
    def _set_tree_properties(self):
        # Find and set the root vertex
        for vertex in self:
            if vertex.number_of_predecessors() == 0:
                if self._root_vertex is not None:
                    raise NoValidVertexError('The tree has multiple root vertices')
                self._root_vertex = vertex
        if self._root_vertex is None:
            raise NoValidVertexError("The tree has no root vertex")
        
        # Find the vertices at each level of the tree
        vertices_to_level = {}
        queue = [self._root_vertex]
        while queue:
            vertex = queue.pop()
            for succ_edge in vertex.successor_edge_iterator():
                queue.insert(0, self.get_vertex(succ_edge.vertex_id))
            if vertex == self._root_vertex:
                self._level_to_vertices[0] = [self._root_vertex]
                vertices_to_level[self._root_vertex] = 0
            else:
                parent_vertex = self.get_vertex(vertex.get_ith_predecessor_edge(0).\
                                                vertex_id)
                vertex_level = vertices_to_level[parent_vertex] + 1
                self._level_to_vertices.setdefault(vertex_level, set()).add(vertex)
                vertices_to_level[vertex] = vertex_level
        

       
class DepthFirstSearch(Tree):
    
    """
    Models the tree discovered during a depth-first traversal of a directed
    graph.
    """

    def __init__(self, directed_graph, root_vertex, reverse_edge_directions):
        Tree.__init__(self, directed_graph.name)
        self.__reverse_edge_directions = reverse_edge_directions
        self._backedges = []
        self._pre_order  = []
        self._post_order = []
        self._vertex_pre_order_numbering = {}
        self._vertex_post_order_numbering = {}
        self.__initialise(directed_graph, root_vertex)
        self.__do_search(directed_graph, root_vertex)
        
    
    @property
    def backedges(self):
        return self._backedges
    
    
    @property
    def pre_order(self):
        return self._pre_order
    
    
    @property
    def post_order(self):
        return self._post_order
    
    
    def get_vertex_with_this_post_order_value(self, post_order_value):
        try:
            return self._post_order[post_order_value-1]
        except KeyError:
            raise KeyError('Post-order value {} is out of range'.\
                           format(post_order_value))
        
    
    def get_vertex_with_this_pre_order_value(self, pre_order_value):
        try:
            return self._pre_order[pre_order_value-1]
        except KeyError:
            raise KeyError('Pre-order value {} is out of range'.\
                           format(pre_order_value))
        
        
    def get_pre_order_value_for_vertex(self, vertex):
        try:
            return self._vertex_pre_order_numbering[vertex]
        except KeyError:
            raise KeyError('No pre-order value found for vertex {}'.\
                           format(vertex.vertex_id))
    
    
    def get_post_order_value_for_vertex(self, vertex):
        try:
            return self._vertex_post_order_numbering[vertex]
        except KeyError:
            raise KeyError('No post-order value found for vertex {}'.\
                           format(vertex.vertex_id))
    
        
    def __initialise(self, directed_graph, root_vertex):
        for vertex in directed_graph:
            self.add_vertex(Vertex(vertex.vertex_id))
            self._vertex_pre_order_numbering[vertex] = None
            self._vertex_post_order_numbering[vertex] = None
        self._root_vertex = self.get_vertex(root_vertex.vertex_id)
        self.__pre_orderID  = 1
        self.__post_orderID = 1   
      
      
    def __do_search(self, directed_graph, vertex):
        # Append vertex to pre-order
        self._vertex_pre_order_numbering[vertex] = self.__pre_orderID
        self._pre_order.append(vertex)
        self.__pre_orderID += 1
        # Do exploration from this vertex
        if self.__reverse_edge_directions:
            edge_iterator = vertex.predecessor_edge_iterator()
        else:
            edge_iterator = vertex.successor_edge_iterator()
        for edge in edge_iterator:
            next_vertex = directed_graph.get_vertex(edge.vertex_id)
            if self._vertex_pre_order_numbering[next_vertex] is None:
                self.add_edge(self.get_vertex(vertex.vertex_id), 
                              self.get_vertex(edge.vertex_id))
                self.__do_search(directed_graph, next_vertex)
            elif self._vertex_pre_order_numbering[vertex]\
                    < self._vertex_pre_order_numbering[next_vertex]:
                pass
            elif self._vertex_post_order_numbering[next_vertex] is None:
                self._backedges.append((vertex, next_vertex))
        # Append vertex to post-order
        self._vertex_post_order_numbering[vertex] = self.__post_orderID
        self._post_order.append(vertex)
        self.__post_orderID += 1



class Dominators(Tree):
    
    """
    Models the pre-dominator and post-dominator trees of a control flow graph.
    """
    
    
    def __init__(self, control_flow_graph, reverse_edge_directions):
        Tree.__init__(self, control_flow_graph.name)
        self.__reverse_edge_directions = reverse_edge_directions
        if self.__reverse_edge_directions:
            root_vertex = control_flow_graph.exit_vertex
        else:
            root_vertex = control_flow_graph.entry_vertex
        self.__immediate_dominator = {}
        self.__initialise(control_flow_graph, root_vertex)
        self.__solve(control_flow_graph, root_vertex)
        self.__add_vertices(control_flow_graph)
        self.__add_edges(control_flow_graph, root_vertex)
        self._set_tree_properties()
        
    
    def __initialise(self, control_flow_graph, root_vertex):
        for vertex in control_flow_graph:
            if vertex == root_vertex:
                self.__immediate_dominator[vertex] = vertex 
            else:
                self.__immediate_dominator[vertex] = None
                
    
    def __solve(self, control_flow_graph, root_vertex):
        depth_first_search = DepthFirstSearch(control_flow_graph, 
                                              root_vertex,
                                              self.__reverse_edge_directions)
        changed = True
        while changed:
            changed = False
            post_order_id = control_flow_graph.number_of_vertices()
            while post_order_id >= 1:
                vertex = depth_first_search.\
                            get_vertex_with_this_post_order_value(post_order_id)
                if vertex != root_vertex:
                    processed_vertex = None
                    new_immediate_dominator = None
                    
                    if self.__reverse_edge_directions:
                        edge_iterator = vertex.successor_edge_iterator()
                    else:
                        edge_iterator = vertex.predecessor_edge_iterator()
                    for edge in edge_iterator:
                        next_vertex = control_flow_graph.get_vertex(edge.vertex_id)
                        if self.__immediate_dominator[next_vertex] is not None:
                            processed_vertex        = next_vertex
                            new_immediate_dominator = processed_vertex
                            
                    if self.__reverse_edge_directions:
                        edge_iterator = vertex.successor_edge_iterator()
                    else:
                        edge_iterator = vertex.predecessor_edge_iterator()     
                    for edge in edge_iterator:
                        next_vertex = control_flow_graph.get_vertex(edge.vertex_id)
                        if next_vertex != processed_vertex:
                            if self.__immediate_dominator[next_vertex] is not None:
                                new_immediate_dominator =\
                                    self.__intersect(depth_first_search, 
                                                     next_vertex, 
                                                     new_immediate_dominator)
    
                    if new_immediate_dominator is not None:
                        if self.__immediate_dominator[vertex] != new_immediate_dominator:
                            self.__immediate_dominator[vertex] =\
                                new_immediate_dominator
                            changed = True
                post_order_id -= 1
    
    
    def __intersect(self, depth_first_search, vertex_one, vertex_two):
        while (depth_first_search.get_post_order_value_for_vertex(vertex_one) 
               != 
               depth_first_search.get_post_order_value_for_vertex(vertex_two)):
            while (depth_first_search.get_post_order_value_for_vertex(vertex_one)
                   < 
                   depth_first_search.get_post_order_value_for_vertex(vertex_two)):
                vertex_one = self.__immediate_dominator[vertex_one]
            while (depth_first_search.get_post_order_value_for_vertex(vertex_two)
                   <
                   depth_first_search.get_post_order_value_for_vertex(vertex_one)):
                vertex_two = self.__immediate_dominator[vertex_two]
        return vertex_one
                
                
    def __add_vertices(self, control_flow_graph):
        for vertex in control_flow_graph:
            self.add_vertex(ProgramPointVertex(vertex.vertex_id,
                                               vertex.program_point))
            
    
    def __add_edges(self, control_flow_graph, root_vertex):
        # Add dominator edges between states
        for vertex, immediate_dominator in self.__immediate_dominator.items():
            if vertex != root_vertex:
                self.add_edge(self.get_vertex(immediate_dominator.vertex_id), 
                              self.get_vertex(vertex.vertex_id))


    def dot_filename(self):
        suffix = 'post' if self.__reverse_edge_directions else 'pre'
        return '{}.{}'.format(self.name, suffix)
        
        
        
class DominatorGraph(DirectedGraph):
    
    """
    Models the union of the pre-dominator and post-dominator trees of a control
    flow graph.
    """
    
    def __init__(self, name, pre_dominator_tree, post_dominator_tree):
        DirectedGraph.__init__(self, name)
        self.add_vertices(pre_dominator_tree)
        self.add_edges(pre_dominator_tree, post_dominator_tree)
        
        
    def add_vertices(self, pre_dominator_tree):
        for vertex in pre_dominator_tree:
            self.add_vertex(ProgramPointVertex(vertex.vertex_id,
                                               vertex.program_point))        


    def add_edges(self, pre_dominator_tree, post_dominator_tree):
        for vertex in pre_dominator_tree:
            for succ_edge in vertex.successor_edge_iterator():
                self.add_edge(self.get_vertex(vertex.vertex_id),
                              self.get_vertex(succ_edge.vertex_id))
        for vertex in post_dominator_tree:
            for succ_edge in vertex.successor_edge_iterator():
                if not self.get_vertex(vertex.vertex_id).\
                    has_successor(succ_edge.vertex_id):
                    self.add_edge(self.get_vertex(vertex.vertex_id),
                                  self.get_vertex(succ_edge.vertex_id))



class LoopNestingHierarchy(Tree):
    
    """
    Models the loop-nesting hierarchy of a function in a program.
    """
    
    def __init__(self, control_flow_graph):
        Tree.__init__(self, control_flow_graph.name)
        self._program_point_to_vertex = {}
        self.__current_parent = {}
        self.__construct(control_flow_graph, 
                         control_flow_graph.get_pre_dominator_tree(), 
                         DepthFirstSearch(control_flow_graph, 
                                          control_flow_graph.entry_vertex,
                                          False))
        # Cache results to speed up queries
        self.__cached_loop_bodies = {}
        
        
    def get_vertex_for_program_point(self, program_point):
        try:
            return self._program_point_to_vertex[program_point]
        except KeyError:
            raise KeyError('No vertex found for program point {}'.\
                           format(program_point))
            
            
    def is_loop_header(self, program_point):
        abstract_vertex =\
            self.get_header_abstract_vertex_for_program_point(program_point)
        return abstract_vertex.program_point == program_point
    
    
    def is_loop_tail(self, program_point):
        program_point_vertex = self._program_point_to_vertex[program_point]
        abstract_vertex = self.get_vertex(program_point_vertex.
                                          get_ith_predecessor_edge(0).
                                          vertex_id)
        return not is_basic_block(program_point)\
            and abstract_vertex.program_point == program_point
    
    
    def get_header_abstract_vertex_for_program_point(self, program_point):
        program_point_vertex = self._program_point_to_vertex[program_point]
        abstract_vertex = self.get_vertex(program_point_vertex.
                                          get_ith_predecessor_edge(0).
                                          vertex_id)
        if is_basic_block(abstract_vertex.program_point):
            return abstract_vertex
        return self.get_vertex(abstract_vertex.
                               get_ith_predecessor_edge(0).
                               vertex_id)
    
    
    def get_loop_body_for_program_point(self, program_point):
        program_point_vertex = self.get_vertex_for_program_point(program_point)
        abstract_vertex = self.get_vertex(program_point_vertex.
                                          get_ith_predecessor_edge(0).
                                          vertex_id)
        if abstract_vertex not in self.__cached_loop_bodies:
            loop_body = set()
            stack = [abstract_vertex]
            while stack:
                stack_vertex = stack.pop()
                for succ_edge in stack_vertex.successor_edge_iterator():
                    succ_vertex = self.get_vertex(succ_edge.vertex_id)
                    if succ_vertex.abstract: 
                        if not is_basic_block(succ_vertex.
                                                                 program_point):
                            stack.append(succ_vertex)
                    else:
                        loop_body.add(succ_vertex)
            self.__cached_loop_bodies[abstract_vertex] = loop_body
        return self.__cached_loop_bodies[abstract_vertex]
            
            
    def level_by_level_iterator(self, up=True, abstract_vertices_only=False):
        """
        Move up the loop-nesting tree in the given direction (up or down) and 
        return the set of vertices at that level.  Non-abstract vertices can 
        be filtered out before the set of vertices is returned.
        """
        if up:
            for level in reversed(sorted(self._level_to_vertices.keys())):
                if not abstract_vertices_only:
                    yield level, self._level_to_vertices[level]
                else:
                    yield level, set([tree_vertex for tree_vertex in 
                                      self._level_to_vertices[level] 
                                      if tree_vertex.abstract])
        else:
            for level in sorted(self._level_to_vertices.keys()):
                if not abstract_vertices_only:
                    yield level, self._level_to_vertices[level]
                else:
                    yield level, set([tree_vertex for tree_vertex in 
                                      self._level_to_vertices[level] 
                                      if tree_vertex.abstract])           
            
            
    def dot_filename(self):
        return '{}.lnt'.format(self._name)
        
        
    def __construct(self, 
                    control_flow_graph, 
                    pre_dominator_tree,
                    depth_first_search_tree):
        # The parent of a vertex in the loop-nesting tree is initially itself
        for vertex in control_flow_graph:
            self.__current_parent[vertex] = vertex
        loop_bodies = self.__find_loops(control_flow_graph,
                                        pre_dominator_tree,
                                        depth_first_search_tree)
        abstract_vertices = self.__add_vertices(control_flow_graph, 
                                                loop_bodies)
        self.__add_edges(control_flow_graph,
                         loop_bodies,
                         abstract_vertices)
        self._set_tree_properties()
        

    def __find_loops(self, 
                     control_flow_graph, 
                     pre_dominator_tree, 
                     depth_first_search_tree):
        loop_bodies = {}
        for vertex in reversed(depth_first_search_tree.pre_order):
            for pred_edge in vertex.predecessor_edge_iterator():
                pred_vertex = control_flow_graph.get_vertex(pred_edge.vertex_id)
                if (pred_vertex, vertex) in depth_first_search_tree.backedges:
                    self.__find_loop_body(control_flow_graph,
                                          pre_dominator_tree,
                                          depth_first_search_tree,
                                          pred_vertex, 
                                          vertex,
                                          loop_bodies)
        return loop_bodies
        
            
    def __find_loop_body(self, 
                         control_flow_graph,
                         pre_dominator_tree, 
                         depth_first_search_tree, 
                         tail,
                         header,
                         loop_bodies):
        assert pre_dominator_tree.is_ancestor\
        (pre_dominator_tree.get_vertex(header.vertex_id),\
         pre_dominator_tree.get_vertex(tail.vertex_id)),\
        'Depth-first backedge ({}, {}) identifies an irreducible loop'.\
        format(tail.vertex_id, header.vertex_id)
        
        loop_bodies[(tail, header)] = set({header})
        # Compute the loop body by moving up the graph from the loop tail until
        # we reach the header
        work_list = [tail]
        while work_list:
            work_list_vertex = work_list.pop()
            loop_bodies[(tail, header)].add(work_list_vertex)
            for pred_edge in work_list_vertex.predecessor_edge_iterator():
                pred_vertex = control_flow_graph.\
                                get_vertex(pred_edge.vertex_id)
                if (pred_vertex, work_list_vertex) not in\
                    depth_first_search_tree.backedges:
                    representative_vertex = self.__current_parent[pred_vertex]
                    if representative_vertex not in work_list\
                    and representative_vertex not in loop_bodies[(tail, header)]\
                    and representative_vertex != header:
                        work_list.append(representative_vertex)
        # Update internally to reflect the loop body just found
        for vertex in loop_bodies[(tail, header)]:
            self.__current_parent[vertex] = header
            

    def __add_vertices(self, control_flow_graph, loop_bodies):
        # Add a vertex per program point in the control flow graph
        for vertex in control_flow_graph:
            program_point_vertex = ProgramPointVertex(vertex.vertex_id,
                                                      vertex.program_point)
            self._program_point_to_vertex[vertex.program_point] =\
                program_point_vertex
            self.add_vertex(program_point_vertex)
            
        # Rip out the loop tails and the loop headers
        loop_headers = set({header for (_, header) in loop_bodies.keys()})
        loop_tails = set({tail for (tail, _) in loop_bodies.keys()})
        abstract_vertices = {}
        
        # Add an abstract vertex per loop header and per loop tail
        for header in loop_headers:
            abstract_vertex = ProgramPointVertex(self.get_new_vertex_id(),
                                                 header.program_point,
                                                 True) 
            self.add_vertex(abstract_vertex)
            abstract_vertices[header] = abstract_vertex
            
        for tail in loop_tails:
            abstract_vertex = ProgramPointVertex(self.get_new_vertex_id(),
                                                 tail.program_point,
                                                 True) 
            self.add_vertex(abstract_vertex)
            abstract_vertices[tail] = abstract_vertex
        
        return abstract_vertices
    
    
    def __add_edges(self, 
                    control_flow_graph, 
                    loop_bodies,
                    abstract_vertices):
        # For a loop-back edge (t, h), add an edge from the abstract vertex of 
        # h to the abstract vertex of t 
        for (tail, header), loop_body in loop_bodies.items():
            self.add_edge(abstract_vertices[header], 
                          abstract_vertices[tail])
        
        # Work out which headers have multiple backedges and, for each
        # such header, the set of vertices shared among those loops.
        header_loop_bodies = {}
        for (_, header), loop_body in loop_bodies.items():
            header_loop_bodies.setdefault(header, set()).update(loop_body)
            header_loop_bodies[header].intersection_update(loop_body)
                        
        # For each vertex in a shared loop body, set the header's abstract
        # vertex as the parent.  Otherwise, set the tail's abstract vertex as 
        # the parent.
        for (tail, header), loop_body in loop_bodies.items():
            for vertex in loop_body:
                if vertex in abstract_vertices\
                and vertex != header\
                and is_basic_block(vertex.program_point):
                    # An inner loop header.  Add an edge to the inner loop header's
                    # abstract vertex but not to the inner loop header's program 
                    # point vertex.  The latter edge insertion will be handled 
                    # while looping through the inner loop's body.
                    if not abstract_vertices[header].has_successor(abstract_vertices
                                                                   [vertex].
                                                                   vertex_id):
                        self.add_edge(abstract_vertices[header],
                                      abstract_vertices[vertex])
                else:
                    tree_vertex = self.get_vertex(vertex.vertex_id)
                    if tree_vertex.number_of_predecessors() == 0:
                        if header_loop_bodies[header] == loop_body:
                            self.add_edge(abstract_vertices[tail], 
                                          tree_vertex)
                        elif vertex not in header_loop_bodies[header]: 
                            self.add_edge(abstract_vertices[tail],
                                          tree_vertex)
                        else:
                            self.add_edge(abstract_vertices[header],
                                          tree_vertex)
                                



class PathExpression(DirectedGraph):
    
    """
    Models a path expression between two states in a control flow graph.
    """    
    
    @staticmethod
    def create_loop_path_expression(child_expression,
                                    name):
        path_expression = PathExpression(name)
        loop_vertex =\
            RegularExpressionVertex(path_expression.get_new_vertex_id(),
                                    RegularExpressionVertex.MIGHT_ITERATE)
        path_expression.add_vertex(loop_vertex)
        path_expression._root_vertex = loop_vertex
        path_expression.add_edge(loop_vertex,
                                 child_expression.root_vertex)
        path_expression._vertices.update(child_expression._vertices)
        return path_expression
    
    
    @staticmethod
    def create_alternative_of_two_path_expressions(child_expression_one,
                                                   child_expression_two,
                                                   name):
        path_expression = PathExpression(name)
        alternative_vertex =\
            RegularExpressionVertex(path_expression.get_new_vertex_id(),
                                    RegularExpressionVertex.ALTERNATIVE)
        path_expression.add_vertex(alternative_vertex)
        path_expression._root_vertex = alternative_vertex
        path_expression.add_edge(alternative_vertex,
                                 child_expression_one.root_vertex)
        path_expression._vertices.update(child_expression_one._vertices)
        path_expression.add_edge(alternative_vertex,
                                 child_expression_two.root_vertex)
        path_expression._vertices.update(child_expression_two._vertices)
        return path_expression
    
    
    @staticmethod
    def create_sequence_of_path_expressions(list_of_child_expressions, 
                                            name):
        path_expression = PathExpression(name)
        sequence_vertex =\
            RegularExpressionVertex(path_expression.get_new_vertex_id(),
                                    RegularExpressionVertex.SEQUENCE)
        path_expression.add_vertex(sequence_vertex)
        path_expression._root_vertex = sequence_vertex
        for child_path_expression in list_of_child_expressions:
            path_expression.add_edge(sequence_vertex,
                                     child_path_expression.root_vertex)
            path_expression._vertices.update(child_path_expression._vertices)
        return path_expression
        
    
    @staticmethod
    def create_sequence_from_single_program_point(program_point, name):
        path_expression = PathExpression(name)
        sequence_vertex =\
            RegularExpressionVertex(path_expression.get_new_vertex_id(),
                                    RegularExpressionVertex.SEQUENCE)
        path_expression.add_vertex(sequence_vertex)
        path_expression._root_vertex = sequence_vertex
        program_point_vertex = ProgramPointVertex(path_expression.get_new_vertex_id(),
                                                      program_point)
        path_expression.add_vertex(program_point_vertex)
        path_expression.add_edge(sequence_vertex, program_point_vertex)
        return path_expression
        
    
    @staticmethod
    def create_sequence_from_list_of_program_points(the_list, name):
        path_expression = PathExpression(name)
        sequence_vertex =\
            RegularExpressionVertex(path_expression.get_new_vertex_id(),
                                    RegularExpressionVertex.SEQUENCE)
        path_expression.add_vertex(sequence_vertex)
        path_expression._root_vertex = sequence_vertex
        for program_point in the_list:
            program_point_vertex = ProgramPointVertex(path_expression.get_new_vertex_id(),
                                                      program_point)
            path_expression.add_vertex(program_point_vertex)
            path_expression.add_edge(sequence_vertex, program_point_vertex)
        return path_expression
        
    
    def __init__(self, name):   
        DirectedGraph.__init__(self, name)
        self._root_vertex = None
        self.__string = None
        self._pred_vertex = None
        self._succ_vertex = None
        
    
    @property
    def root_vertex(self):
        return self._root_vertex
    
    
    @property
    def pred_vertex(self):
        return self._pred_vertex
    
    
    @property
    def succ_vertex(self):
        return self._succ_vertex
    
    
    @pred_vertex.setter
    def pred_vertex(self, value):
        self._pred_vertex = value
    
    
    @succ_vertex.setter
    def succ_vertex(self, value):
        self._succ_vertex = value
    
    
    new_vertex_id = 0
    def get_new_vertex_id(self):
        PathExpression.new_vertex_id += 1
        return PathExpression.new_vertex_id     
    
    
    def dot_filename(self):
        return '{}.{}_{}.pe'.format(self._name,
                                    self._pred_vertex.vertex_id,
                                    self._succ_vertex.vertex_id)  
        
    
    def __str__(self):
        if self.__string is None:
            cached_strings = {}
            depth_first_search = DepthFirstSearch(self, 
                                                  self._root_vertex, 
                                                  False)
            for vertex in depth_first_search.post_order:
                if isinstance(vertex, RegularExpressionVertex):
                    string = ''
                    
                    # Do we need to parenthesise the sub-expression?
                    if vertex.operator in [RegularExpressionVertex.ALTERNATIVE,
                                           RegularExpressionVertex.MIGHT_ITERATE,
                                           RegularExpressionVertex.MUST_ITERATE]:
                        string += '['
                    
                    # Construct the meat of the expression.
                    counter = 1
                    for succ_edge in vertex.successor_edge_iterator():
                        succ_vertex = self.get_vertex(succ_edge.vertex_id)
                        if isinstance(succ_vertex, ProgramPointVertex):
                            string += str(succ_vertex)
                        else:
                            string += cached_strings[succ_edge]
                        
                        if counter < vertex.number_of_successors():
                            string += vertex.operator
                        counter += 1
                    
                    # Do we need to parenthesise the sub-expression?
                    if vertex.operator in [RegularExpressionVertex.ALTERNATIVE,
                                           RegularExpressionVertex.MIGHT_ITERATE,
                                           RegularExpressionVertex.MUST_ITERATE]:
                        string += ']'
                        
                    # Repeat the expression zero or more times?
                    if vertex.operator in [RegularExpressionVertex.MIGHT_ITERATE,
                                           RegularExpressionVertex.MUST_ITERATE]:
                        string += vertex.operator
                    
                    # Move the expression up the DAG.
                    for pred_edge in vertex.predecessor_edge_iterator():
                        if self.has_vertex(pred_edge.vertex_id):
                            pred_vertex = self.get_vertex(pred_edge.vertex_id)
                            succ_edge = pred_vertex.get_successor_edge(vertex.vertex_id)
                            cached_strings[succ_edge] = string
                    if vertex == self.root_vertex:
                        cached_strings[vertex] = string
                        
            self.__string = cached_strings[self._root_vertex]
        return self.__string
                


class StronglyConnectedComponents:
    
    """
    Compute the strongly connected components of a directed graph.
    """    
    
    
    WHITE  = 'white'
    BLACK  = 'black'
    GRAY   = 'gray'
    BLUE   = 'blue'
    RED    = 'red'
    
    
    new_scc_id = 0
    @staticmethod
    def get_new_scc_id():
        StronglyConnectedComponents.new_scc_id += 1
        return StronglyConnectedComponents.new_scc_id 
    
    
    def __init__(self, directed_graph):
        self.__program_point_to_scc_id = {}
        self.__scc_id_to_vertices = {}
        vertex_color = self.__initialise(directed_graph)
        vertex_list = self.__do_forward_visit(directed_graph, vertex_color)
        self.__do_reverse_visit(directed_graph, vertex_color, vertex_list)
    
    
    def get_scc_id_for_program_point(self, program_point):
        try:
            return self.__program_point_to_scc_id[program_point]
        except:
            raise KeyError('Unable to find SCC for program point {}'.\
                           format(program_point))
        
    
    def __iter__(self):
        for scc_id, vertices in self.__scc_id_to_vertices.items():
            yield scc_id, vertices

        
    def __initialise(self, directed_graph):
        vertex_color = {}
        for vertex in directed_graph:
            vertex_color[vertex] = StronglyConnectedComponents.WHITE
        return vertex_color
    
            
    def __do_forward_visit(self, directed_graph, vertex_color):
        vertex_list = []
        for vertex in directed_graph:
            if vertex_color[vertex] == StronglyConnectedComponents.WHITE:
                stack = [vertex]
                while stack:
                    stack_vertex = stack.pop()
                    if vertex_color[stack_vertex] == StronglyConnectedComponents.WHITE:
                        vertex_color[stack_vertex] = StronglyConnectedComponents.GRAY
                        stack.append(stack_vertex)
                        for succ_edge in stack_vertex.successor_edge_iterator():
                            succ_vertex = directed_graph.get_vertex(succ_edge.vertex_id)
                            if vertex_color[succ_vertex] \
                            == StronglyConnectedComponents.WHITE:
                                stack.append(succ_vertex)
                    elif vertex_color[stack_vertex] ==\
                            StronglyConnectedComponents.GRAY:  
                        vertex_color[stack_vertex] =\
                            StronglyConnectedComponents.BLACK
                        vertex_list.append(stack_vertex)
        return vertex_list
                        

    def __do_reverse_visit(self, directed_graph, vertex_color, vertex_list):
        for vertex in reversed(vertex_list):
            if vertex_color[vertex] == StronglyConnectedComponents.BLACK:
                scc_id = StronglyConnectedComponents.get_new_scc_id()
                self.__scc_id_to_vertices[scc_id] = set()
                stack = [vertex]
                while stack:
                    stack_vertex = stack.pop()
                    self.__program_point_to_scc_id[stack_vertex.program_point] = scc_id
                    self.__scc_id_to_vertices[scc_id].add(stack_vertex)
                    if vertex_color[stack_vertex] == StronglyConnectedComponents.BLACK:
                        vertex_color[stack_vertex] = StronglyConnectedComponents.BLUE
                        stack.append(stack_vertex)
                        for pred_edge in stack_vertex.predecessor_edge_iterator():
                            pred_vertex = directed_graph.get_vertex(pred_edge.vertex_id)
                            if vertex_color[pred_vertex] ==\
                                StronglyConnectedComponents.BLACK:
                                stack.append(pred_vertex)
                    elif vertex_color[stack_vertex] ==\
                            StronglyConnectedComponents.BLUE:
                        vertex_color[stack_vertex] =\
                            StronglyConnectedComponents.RED  
                            


class SuperBlockGraph(DirectedGraph):
    
    """
    Models the super block graph of a control flow graph whereby program points 
    are divided into sets.  A set of program points has the property that all
    program points must execute together and execute the same number of times,
    in any program run.  Edges between these sets maintain control flow 
    information and effectively show how execution count is siphoned off from 
    one super block to others.
    """
    
    @staticmethod
    def create_for_loop(control_flow_graph, abstract_vertex):
        name = '{}.{}'.format(control_flow_graph.name, abstract_vertex.program_point)
        induced_subgraph = control_flow_graph.\
                            induce_subgraph_for_loop_header\
                                (name,
                                 abstract_vertex.program_point)
        super_block_graph = SuperBlockGraph(name, induced_subgraph)
        pre_dominator_tree = induced_subgraph.get_pre_dominator_tree()
        post_dominator_tree = induced_subgraph.get_post_dominator_tree()
        dominator_graph = DominatorGraph(name,
                                         pre_dominator_tree,
                                         post_dominator_tree)
        strong_components = StronglyConnectedComponents(dominator_graph) 
         
        super_block_graph.add_super_blocks(abstract_vertex,
                                           induced_subgraph,
                                           strong_components)
        super_block_graph.add_edges(abstract_vertex,
                                    induced_subgraph) 
        return super_block_graph
    
    
    def __init__(self, name, induced_subgraph):
        DirectedGraph.__init__(self, name)
        self._induced_subgraph = induced_subgraph
        self._root_vertex = None
        self._program_point_to_vertex = {}
    
    
    @property
    def induced_subgraph(self):
        return self._induced_subgraph
    
    
    @property
    def root_vertex(self):
        return self._root_vertex
    
    
    def has_vertex_for_program_point(self, program_point):
        return program_point in self._program_point_to_vertex
    
    
    def get_vertex_for_program_point(self, program_point):
        try:
            return self._program_point_to_vertex[program_point]
        except KeyError:
            raise KeyError('No super block found for program point {}'.
                           format(program_point))
    
    
    def add_super_blocks(self,
                         abstract_vertex,
                         induced_subgraph,
                         strong_components_of_dominator_graph):
        # Add a super block for every strong component identified in the 
        # dominator graph 
        for scc_id, _ in strong_components_of_dominator_graph:
            self.add_vertex(SuperBlock(scc_id))

        # Add program points to super blocks and set the representative program
        # point in each one
        depth_first_search_tree = DepthFirstSearch(induced_subgraph,
                                                   induced_subgraph.entry_vertex,
                                                   False)
        for induced_vertex in reversed(depth_first_search_tree.post_order):
            if induced_vertex.program_point is not None: 
                scc_id = strong_components_of_dominator_graph.\
                            get_scc_id_for_program_point(induced_vertex.program_point)
                super_vertex = self.get_vertex(scc_id)
                super_vertex.vertices.append(induced_vertex)
                self._program_point_to_vertex[induced_vertex.program_point] =\
                    super_vertex 
                
                if is_basic_block(induced_vertex.program_point):
                    if not induced_vertex.abstract:
                        super_vertex.representative = induced_vertex
                else:
                    if induced_subgraph.has_vertex_for_program_point\
                        (induced_vertex.program_point[0])\
                    and not induced_subgraph.has_vertex_for_program_point\
                        (induced_vertex.program_point[1]):
                        super_vertex.is_loop_exit_edge = True
                
                # The root vertex of the super block graph is the super block containing
                # the header.
                if induced_vertex.program_point == abstract_vertex.program_point:
                    assert self._root_vertex == None
                    self._root_vertex = super_vertex

        # If the super block does not have a representative yet, then there is
        # no suitable basic block program point.  Instead, select the first 
        # transition between basic blocks.             
        for super_vertex in self:
            if super_vertex.representative is None:
                for induced_vertex in super_vertex.vertices:
                    super_vertex.representative = induced_vertex
                    if not is_basic_block(induced_vertex.
                                                             program_point):
                        super_vertex.representative = induced_vertex
                        break
                    
        
    def add_edges(self, abstract_vertex, induced_subgraph):
        for super_vertex in self:
            induced_vertex = super_vertex.vertices[0]
            if not is_basic_block(induced_vertex.program_point):
                # The first program point in the super block is a control-flow 
                # edge
                assert induced_vertex.number_of_predecessors() == 1
                pred_edge = induced_vertex.get_ith_predecessor_edge(0)
                pred_induced_vertex = induced_subgraph.\
                                        get_vertex(pred_edge.vertex_id)
                pred_super_vertex = self._program_point_to_vertex\
                                        [pred_induced_vertex.program_point]
                self.add_edge(pred_super_vertex, super_vertex)                
                pred_super_vertex.\
                    add_successor_edge_to_partition(pred_induced_vertex,
                                                    pred_super_vertex.\
                                                    get_successor_edge(super_vertex.vertex_id))
                
            elif induced_vertex.program_point != abstract_vertex.program_point:
                # The first program point in the super block is a basic block 
                # but not the loop header of this region.
                assert induced_vertex.number_of_predecessors() > 1
                for pred_edge in induced_vertex.predecessor_edge_iterator():
                    pred_induced_vertex = induced_subgraph.\
                                            get_vertex(pred_edge.vertex_id)
                    pred_super_vertex = self._program_point_to_vertex\
                                            [pred_induced_vertex.program_point]
                    self.add_edge(pred_super_vertex, super_vertex)
    
                    
    def choose_instrumentation_points_for_profiling(self,
                                                    instrumentation_points):
        # Filter out basic blocks or transitions between basic blocks 
        # depending on the wished of the user.
        if globals.args['instrument'] != 'mixed':
            for super_vertex in self:
                super_vertex.vertices = [vertex for vertex in super_vertex.vertices
                                         if (is_basic_block(vertex.program_point) and 
                                             globals.args['instrument'] == 'vertices')
                                         or (not is_basic_block(vertex.program_point) and
                                             globals.args['instrument'] == 'edges')]
        
        # Remove super blocks that no longer have any program points.
        vertices_to_remove = set()
        for super_vertex in self:
            if not super_vertex.vertices:
                vertices_to_remove.add(super_vertex)
                for pred_edge in super_vertex.predecessor_edge_iterator():
                    pred_vertex = self.get_vertex(pred_edge.vertex_id)
                    for succ_edge in super_vertex.successor_edge_iterator():
                        succ_vertex = self.get_vertex(succ_edge.vertex_id)
                        self.add_edge(pred_vertex, succ_vertex)
        
        for super_vertex in vertices_to_remove:
            self.remove_vertex(super_vertex)
        
        # Set the representative because filtering out non-candidate program
        # points may have clobbered it.
        for super_vertex in self:
            random_index = random.randint(0,len(super_vertex.vertices)-1)
            super_vertex.representative = super_vertex.vertices[random_index]
        
        # Pick out super blocks to instrument and then choose the representatives
        # of those super blocks.
        for super_vertex in self:
            if super_vertex.number_of_successors() == 0:
                instrumentation_points.add(super_vertex.representative.program_point)
            if super_vertex.number_of_predecessors() > 1:
                list_of_predecessors = [pred_edge for pred_edge in super_vertex.predecessor_edge_iterator()]
                pred_edge_to_remove = list_of_predecessors[random.randint(0, len(list_of_predecessors)-1)]
                list_of_predecessors.remove(pred_edge_to_remove)
                for pred_edge in list_of_predecessors:
                    pred_super_vertex = self.get_vertex(pred_edge.vertex_id)
                    instrumentation_points.add(pred_super_vertex.representative.program_point)
    
    def dot_filename(self):
        return '{}.super'.format(self._name)



def create_control_flow_graph(name):
    
    class ArtificialLoopBody:
        """
        A loop body created during artificial construction of a control flow graph.
        """
        
        def __init__(self):
            self._header_vertex = None
            self._vertex_to_level = {}
            self._level_to_vertices = {}
            self._loop_exit_sources = set()
            
        
        @property
        def header_vertex(self):
            return self._header_vertex
        
        
        @property
        def loop_exit_sources(self):
            return self._loop_exit_sources
        
        
        def add_vertices(self, 
                         control_flow_graph, 
                         number_of_vertices, 
                         number_of_nested_loops,
                         tree_vertex):
            level = 0
            while number_of_vertices > 0:
                vertex_id = control_flow_graph.get_new_vertex_id()
                vertex = ProgramPointVertex(vertex_id, vertex_id)
                control_flow_graph.add_vertex(vertex)
                self._level_to_vertices.setdefault(level, []).append(vertex)
                self._vertex_to_level[vertex] = level
                number_of_vertices -= 1            
                
                if level == 0:
                    self._header_vertex = vertex
                    level += 1
                elif number_of_nested_loops > 0:
                    number_of_nested_loops -= 1
                    level += 1
                elif len(self._level_to_vertices[level]) > 2\
                or bool(random.getrandbits(1)):
                    level += 1
            
            highest_level = max(self._level_to_vertices.keys())
            loop_tails = self._level_to_vertices[highest_level]
            # Does this loop body have too many loop tails?
            # Yes, if this is the outermost loop and there is no single loop 
            # tail, or if we decide the proportion of loop tails to the number 
            # of loop body vertices is too great.
            if len(loop_tails) > 1: 
                if tree_vertex.number_of_predecessors() == 0\
                or len(loop_tails)/len(self._vertex_to_level) > 0.2:
                    # Promote a random vertex to be the unique loop tail.
                    new_highest_level = highest_level + 1
                    vertex = loop_tails[random.randint(0, len(loop_tails)-1)]
                    self._level_to_vertices.setdefault(new_highest_level, []).append(vertex)
                    self._level_to_vertices[highest_level].remove(vertex)
                    self._vertex_to_level[vertex] = new_highest_level
        
        
        def add_acyclic_edges(self, control_flow_graph):
            highest_level = max(self._level_to_vertices.keys())
            for level in sorted(self._level_to_vertices.keys(), reverse=True):
                if level > 0:
                    if random.random() < 0.3:
                        level_lower_than_current = random.randint(0, level-1)
                    else:
                        level_lower_than_current = level-1
                    candidate_vertices = self._level_to_vertices[level_lower_than_current]
                    for succ_vertex in self._level_to_vertices[level]:      
                        pred_vertex = candidate_vertices\
                                        [random.randint(0, len(candidate_vertices)-1)]
                        control_flow_graph.add_edge(pred_vertex, succ_vertex)
                        
                if level < highest_level:
                    vertices_without_successors = [vertex for vertex in 
                                                   self._level_to_vertices[level] 
                                                   if vertex.number_of_successors() == 0]
                    level_higher_than_current = random.randint(level+1, highest_level)
                    candidate_vertices = self._level_to_vertices[level_higher_than_current]
                    for pred_vertex in vertices_without_successors:
                        succ_vertex = candidate_vertices\
                                        [random.randint(0, len(candidate_vertices)-1)]
                        control_flow_graph.add_edge(pred_vertex, succ_vertex)
        
        
        def add_backedges(self, control_flow_graph):
            highest_level = max(self._level_to_vertices.keys())
            for vertex in self._level_to_vertices[highest_level]:
                control_flow_graph.add_edge(vertex, self._header_vertex)
                    
        
        def connect_nested_loops(self, 
                                 control_flow_graph, 
                                 nested_loop_components):
            highest_level = max(self._level_to_vertices.keys())
            candidate_loop_entry_sources = [vertex for vertex in self._vertex_to_level
                                            if self._vertex_to_level[vertex] != highest_level
                                            and self._vertex_to_level[vertex] != 0]
            for loop_component in nested_loop_components:
                loop_entry_vertex = candidate_loop_entry_sources\
                                        [random.randint(0, len(candidate_loop_entry_sources)-1)]
                control_flow_graph.add_edge(loop_entry_vertex,
                                            loop_component.header_vertex)
                
                for loop_exit_source in loop_component.loop_exit_sources:
                    loop_exit_destinations_level = random.randint\
                                                    (self._vertex_to_level[loop_entry_vertex]+1, 
                                                     highest_level)
                    candidate_loop_exit_destinations = self._level_to_vertices\
                                                        [loop_exit_destinations_level]
                    loop_exit_destination = candidate_loop_exit_destinations\
                                                [random.randint(0, 
                                                                len(candidate_loop_exit_destinations)-1)]
                    control_flow_graph.add_edge(loop_exit_source,
                                                loop_exit_destination)
                
                
        def select_loop_exit_sources(self):
            probability_of_selection = 1.0
            for vertex in self._vertex_to_level.keys():
                if vertex.number_of_successors() == 1\
                and probability_of_selection > random.random()\
                and vertex != self._header_vertex:
                    self._loop_exit_sources.add(vertex)
                    probability_of_selection /= 2
                    
            if not self._loop_exit_sources:
                self._loop_exit_sources.add(self._header_vertex)
    
    
    def create_artificial_loop_hierarchy(name, 
                                         number_of_loops, 
                                         maximum_nesting_depth):
        loop_nesting_tree = Tree(name)
        # Add abstract vertices to the tree, including an extra one for the 
        # dummy outer loop.
        vertex_to_level = {}
        root_vertex = None
        for loop_number in range(1, number_of_loops+2):
            tree_vertex = Vertex(loop_number)
            loop_nesting_tree.add_vertex(tree_vertex)
            root_vertex = tree_vertex
            vertex_to_level[tree_vertex] = 0
        # Add edges to the tree.
        parent_vertex = root_vertex
        for vertex in loop_nesting_tree:
            if vertex != root_vertex:
                new_level = vertex_to_level[parent_vertex] + 1
                if new_level <= maximum_nesting_depth:
                    loop_nesting_tree.add_edge(parent_vertex, vertex)
                    vertex_to_level[vertex] = new_level
                else:
                    # The height of the tree now exceeds the maximum depth, so
                    # backtrack to an arbitrary proper ancestor.
                    ancestor_vertex = parent_vertex
                    while True:
                        pred_edge = ancestor_vertex.get_ith_predecessor_edge(0)
                        ancestor_vertex = loop_nesting_tree.get_vertex\
                                            (pred_edge.vertex_id)
                        if bool(random.getrandbits(1)) \
                        or ancestor_vertex == root_vertex:
                            break
                    parent_vertex = ancestor_vertex
                    loop_nesting_tree.add_edge(parent_vertex, vertex)
                    vertex_to_level[vertex] = vertex_to_level[parent_vertex] + 1
                parent_vertex = vertex
        loop_nesting_tree._set_tree_properties()
        return loop_nesting_tree
    
    
    def create_structure(control_flow_graph, 
                         bare_loop_nesting_tree, 
                         number_of_basic_blocks):
        # Compute number of vertices in each loop.  Guarantee each loop has at 
        # least 2 vertices plus vertices needed to connect inner nested loops.
        number_of_vertices_per_loop = {}
        number_of_vertices_remaining = number_of_basic_blocks
        for tree_vertex in bare_loop_nesting_tree:
            min_vertices = 2 + tree_vertex.number_of_successors()
            number_of_vertices_per_loop[tree_vertex] = min_vertices
            number_of_vertices_remaining -= min_vertices
        # Arbitrarily distribute any remaining vertices to the loop bodies.
        while number_of_vertices_remaining > 0:
            for tree_vertex in bare_loop_nesting_tree:
                additional_vertices = random.randint(0, number_of_vertices_remaining)
                number_of_vertices_per_loop[tree_vertex] += additional_vertices
                number_of_vertices_remaining -= additional_vertices
        # Generate each loop body.
        loop_components = {}
        for _, vertices in bare_loop_nesting_tree.level_by_level_iterator():
            for tree_vertex in vertices:
                nested_loop_components = []
                for succ_edge in tree_vertex.successor_edge_iterator():
                    succ_vertex = bare_loop_nesting_tree.get_vertex(succ_edge.vertex_id)
                    nested_loop_components.append(loop_components[succ_vertex])
                
                loop_component = ArtificialLoopBody()
                loop_component.add_vertices(control_flow_graph,
                                            number_of_vertices_per_loop[tree_vertex],
                                            tree_vertex.number_of_successors(),
                                            tree_vertex)
                loop_component.connect_nested_loops(control_flow_graph, 
                                                    nested_loop_components)
                loop_component.add_acyclic_edges(control_flow_graph)
                loop_component.add_backedges(control_flow_graph)
                
                if tree_vertex == bare_loop_nesting_tree.root_vertex:
                    pred_edge = loop_component.\
                                header_vertex.\
                                get_ith_predecessor_edge(0)
                    control_flow_graph.entry_vertex =\
                        loop_component.header_vertex
                    control_flow_graph.exit_vertex =\
                        control_flow_graph.get_vertex(pred_edge.vertex_id)
                else:
                    loop_component.select_loop_exit_sources()
                    loop_components[tree_vertex] = loop_component
    
    control_flow_graph = ControlFlowGraph(name)
    bare_loop_nesting_tree = create_artificial_loop_hierarchy\
                                (name, 
                                 globals.args['loops'], 
                                 globals.args['nesting_depth'])
    create_structure(control_flow_graph,
                     bare_loop_nesting_tree, 
                     globals.args['vertices'])
    control_flow_graph.check_connected()
    return control_flow_graph


    