"""
This module includes all directed graphs used in different types of analyses. 
"""


import collections
import abc
import random

from tools.lib.utils import dot
from tools.lib.utils import config

from tools.lib.system.edges import (Edge,
                                    TransitionEdge,
                                    CallGraphEdge)

from tools.lib.system.vertices import (Vertex, 
                                       ProgramPointVertex, 
                                       RegularExpressionVertex, 
                                       SuperBlock)



class DuplicateVertexError(Exception):
    
    """
    Exception to catch when a vertex with the same id is added to a graph.
    """
    
    def __init__(self, message):
        Exception.__init__(self, message)
        
        
        
class NoValidVertexError(Exception):
    
    """
    Exception to catch when we cannot find a vertex need to analyse a directed 
    graph, such as the entry or exit vertex or the root of a tree.
    """
    
    def __init__(self, message):
        Exception.__init__(self, message)
        


class IrreducibleLoopError(Exception):
    
    """
    Exception to catch when an irreducible loop is discovered.
    """
    
    def __init__(self, message):
        Exception.__init__(self, message)
        
        

class DirectedGraph:
    
    """
    Models a graph with directed edges.
    """
    
    __metaclass__ = abc.ABCMeta

    def __init__(self, name):
        self._vertices = collections.OrderedDict()
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
        if vertex.vertex_id in self._vertices:
            raise DuplicateVertexError('The graph already has vertex %d' % 
                                       vertex.vertex_id)
        self._vertices[vertex.vertex_id] = vertex
        
    
    def get_vertex(self, vertex_id):
        try:
            return self._vertices[vertex_id]
        except KeyError:
            raise KeyError('Vertex %d is not in the graph' % vertex_id)
        
        
    def remove_vertex(self, vertex):
        for pred_edge in vertex.predecessor_edge_iterator():
            pred_vertex = self.get_vertex(pred_edge.vertex_id)
            pred_vertex.remove_successor_edge(vertex.vertex_id)
        for succ_edge in vertex.successor_edge_iterator():
            succ_vertex = self.get_vertex(succ_edge.vertex_id)
            succ_vertex.remove_predecessor_edge(vertex.vertex_id)
        del self._vertices[vertex.vertex_id]
    
    
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
        return '%s(vertices=%r)' % (self.__class__.__name__,
                                    ' '.join(repr(vertex) 
                                             for vertex in self))
        
    
    @abc.abstractmethod
    def dot_filename(self):
        pass



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
    Every control flow graph has a unique entry and a unique exit.
    """
    
    def __init__(self, name):
        DirectedGraph.__init__(self, name)
        self._entry_vertex = None
        self._exit_vertex  = None
        self._program_point_to_vertex = {}
        self._pre_dominator_tree = None
        self._post_dominator_tree = None
        self._loop_nesting_tree = None
        self._super_block_graphs = None
        
        
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
    
    
    def add_vertex(self, vertex):
        DirectedGraph.add_vertex(self, vertex)
        self._program_point_to_vertex[vertex.program_point] = vertex
        
    
    def add_edge(self, pred_vertex, succ_vertex, path_expression):
        edge_id = self.get_new_edge_id()
        pred_vertex.add_successor_edge(TransitionEdge(succ_vertex.vertex_id, 
                                                      edge_id, 
                                                      path_expression))
        succ_vertex.add_predecessor_edge(TransitionEdge(pred_vertex.vertex_id, 
                                                        edge_id,
                                                        path_expression))
        
    
    def get_vertex_for_program_point(self, program_point):
        try:
            return self._program_point_to_vertex[program_point]
        except KeyError:
            raise KeyError('No vertex found for program point {0}'.\
                           format(program_point))
            
        
    def find_entry_vertex(self):
        without_predecessors = []
        for vertex in self:
            if vertex.number_of_predecessors() == 0:
                without_predecessors.append(vertex)    
        if len(without_predecessors) == 0:
            raise NoValidVertexError('All vertices have at least one predecessor')
        elif len(without_predecessors) > 1:
            raise NoValidVertexError('Too many entry candidates found: %r' %\
                                    ','.join(str(v.vertex_id) for v in without_predecessors))
        else:
            return without_predecessors[0]
            
    
    def find_exit_vertex(self):
        without_successors = []
        for vertex in self:
            if vertex.number_of_successors() == 0:
                without_successors.append(vertex)
        if len(without_successors) == 0:
            raise NoValidVertexError('All vertices have at least one successor')
        elif len(without_successors) > 1:
            raise NoValidVertexError('Too many exit candidates found: %r' %\
                                    ','.join(str(v.vertex_id) for v in without_successors))
        else:
            return without_successors[0]
    
    
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
    
    
    def get_super_block_graph(self):
        if self._super_block_graphs is None:
            self._super_block_graphs = {}
            for header_vertex in self.get_loop_nesting_tree().header_iterator():
                self._super_block_graphs[header_vertex] =\
                    SuperBlockGraph.create_for_loop(self, 
                                                    self.get_loop_nesting_tree(),
                                                    header_vertex)
                dot.make_file(self._super_block_graphs[header_vertex])
        return self._super_block_graphs
    
    
    def split_program_points_into_basic_blocks_and_edges(self):
        basic_blocks = set()
        control_flow_edges = set()
        for vertex in self:
            if isinstance(vertex.program_point, int):
                basic_blocks.add(vertex.program_point)
            else:
                control_flow_edges.add(vertex.program_point)
        return basic_blocks, control_flow_edges
    
    
    def reduce(self, unmonitored_program_points):
        
        state_transition_to_path_expression = {}
            
            
    def reduce_but_maintain_path_reconstructibility(self,
                                                    unmonitored_program_points):
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
            for pred_edge in vertex.predecessor_edge_iterator():
                pred_vertex = self.get_vertex(pred_edge.vertex_id)
                for succ_edge in vertex.successor_edge_iterator():
                    if pred_vertex.has_successor(succ_edge.vertex_id):
                        return False
            return True
                    
        
        # The following loop does two things:
        # 1) It removes unmonitored program points.
        # 2) It sets up the temporary state transition to path expression 
        # mapping. Initially the values in the mapping will be empty.
        states_to_remove = set()
        for vertex in self:
            if vertex.program_point in unmonitored_program_points:
                    states_to_remove.add(vertex)    
            for succ_edge in vertex.successor_edge_iterator():
                succ_vertex = self.get_vertex(succ_edge.vertex_id)
                assert succ_edge.path_expression is None,\
                       'Expected the path expression of the edge %d->%d to be empty'\
                        % (vertex.vertex_id,
                           succ_edge.vertex_id)
                state_transition_to_path_expression[(vertex, succ_vertex)] = []
                    
        
        # Maintain graph connectedness and remove each unneeded state.
        for vertex in states_to_remove:
            connect_predecessors_to_successors(vertex)
            self.remove_vertex(vertex)
        dot.make_file(self)
        
        # Reduce graph until removal of state violates path reconstructibility
        changed = True
        while changed:
            changed = False
            candidiates = list(self._vertices.values())
            random.shuffle(candidiates)
            for vertex in candidiates:
                if can_remove_program_point(vertex):
                    print('Removing', vertex.program_point)
                    changed = True
                    connect_predecessors_to_successors(vertex)
                    self.remove_vertex(vertex)
        
        
        # Create each path expression tree from a list of program points
        for vertex in self:
            for succ_edge in vertex.successor_edge_iterator():
                succ_vertex = self.get_vertex(succ_edge.vertex_id)
                pred_edge = succ_vertex.get_predecessor_edge(vertex.vertex_id)
                path_expression = PathExpression.create_sequence_from_list_of_program_points\
                    (state_transition_to_path_expression[(vertex, succ_vertex)], 
                     self._name, 
                     vertex, 
                     succ_vertex)
                succ_edge.path_expression = path_expression
                pred_edge.path_expression = path_expression
    
    
    def dot_filename(self):
        return '%s.%s.cfg' % (config.get_filename_prefix(), self.name)
    
    
    def __str__(self):
        def write_edges(edge_iterator, total_number_of_edges):
            value = ''
            counter = 1
            for edge in edge_iterator:
                value += '%d' % edge.vertex_id
                if counter < total_number_of_edges:
                    value += ', '
                counter += 1
            return value
        
        value = 'CFG: %s\n' % self._name
        value += '{\n'
        for vertex in self:
            value += 'vertex(id=%d, program_point=%r, abstract=%r)\n' \
                        % (vertex.vertex_id, 
                           vertex.program_point,
                           vertex.abstract)
            value += '  pred = {%s}\n' % write_edges(vertex.predecessor_edge_iterator(),
                                                     vertex.number_of_predecessors())      
            value += '  succ = {%s}\n' % write_edges(vertex.successor_edge_iterator(),
                                                     vertex.number_of_successors())
        value += '}\n'
        return value


class CallGraph(DirectedGraph):
    
    """
    Models the call graph of a program.
    """
    
    def __init__(self):
        DirectedGraph.__init__(self, 'call')
        self.function_name_to_vertex = {}
        
        
    def add_vertex(self, vertex):
        DirectedGraph.add_vertex(self, vertex)
        self.function_name_to_vertex[vertex.name] = vertex
        
        
    def has_vertex_with_name(self, function_name):
        return function_name in self.function_name_to_vertex
        
    
    def get_vertex_with_name(self, function_name):
        try:
            return self.function_name_to_vertex[function_name]
        except ValueError:
            raise ValueError('No vertex found for function %s' % function_name)
        
    
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
        return '%s.%s' % (config.get_filename_prefix(), self._name)
        
        

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
        self._level_to_vertices = collections.OrderedDict()
        self._least_common_ancestor_query = None
        
    
    @property
    def root_id(self):
        return self._root_id
    
    
    def set_tree_properties(self):
        # Find and set the root vertex
        for vertex in self:
            if vertex.number_of_predecessors() == 0:
                if self.root_vertex is not None:
                    raise NoValidVertexError('The tree has multiple root vertices')
                self.root_vertex = vertex
        if self.root_vertex is None:
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
                if vertex_level not in self._level_to_vertices.keys():
                    self._level_to_vertices[vertex_level] = []
                self._level_to_vertices[vertex_level].append(vertex)
                vertices_to_level[vertex] = vertex_level
                
                
    def level_by_level_iterator(self, up=True):
        if up:
            for level in reversed(sorted(self._level_to_vertices.keys())):
                yield self._level_to_vertices[level]
        else:
            for level in sorted(self._level_to_vertices.keys()):
                yield self._level_to_vertices[level]

        
    def remove_edge(self, pred_vertex, succ_vertex):
        pred_vertex.remove_successor_edge(succ_vertex.vertex_id)
        succ_vertex.remove_predecessor_edge(pred_vertex.vertex_id)
    
    
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
            raise KeyError('Post-order value %d is out of range' % post_order_value)
        
    
    def get_vertex_with_this_pre_order_value(self, pre_order_value):
        try:
            return self._pre_order[pre_order_value-1]
        except KeyError:
            raise KeyError('Pre-order value %d is out of range' % pre_order_value)
        
        
    def get_pre_order_value_for_vertex(self, vertex):
        try:
            return self._vertex_pre_order_numbering[vertex]
        except KeyError:
            raise KeyError('No pre-order value found for vertex %d' % 
                           vertex.vertex_id)
    
    
    def get_post_order_value_for_vertex(self, vertex):
        try:
            return self._vertex_post_order_numbering[vertex]
        except KeyError:
            raise KeyError('No post-order value found for vertex %d' % 
                           vertex.vertex_id)
    
        
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
        return '%s.%s.%s' % (config.get_filename_prefix(), 
                             self.name, 
                             ('post' 
                              if self.__reverse_edge_directions 
                              else 'pre'))
        
        
        
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
        self.__loop_body_of_backedge = {}
        self.__loop_exit_edges_of_backedge = {}
        self.__loop_body_of_header = {}
        self.__loop_exit_edges_of_header = {}
        self.__abstract_vertices = {}
        self.__current_parent = {}
        self.__state_to_header = {}
        self.__inner_loop_headers_per_header = {}
        self.__construct(control_flow_graph, 
                         control_flow_graph.get_pre_dominator_tree(), 
                         DepthFirstSearch(control_flow_graph, 
                                          control_flow_graph.entry_vertex,
                                          False))
        self.__compute_loop_body_for_each_header(control_flow_graph)
        self.__find_loop_exits(control_flow_graph)
        
             
    def header_iterator(self):
        for vertex in self.__abstract_vertices.keys():
            if ProgramPointVertex.is_basic_block(vertex.program_point):
                yield vertex
                
        
    def is_loop_header(self, vertex):
        return vertex in self.__abstract_vertices
    
    
    def get_loop_exit_edges_for_header(self, header):
        try:
            return self.__loop_exit_edges_of_header[header]
        except KeyError:
            raise KeyError('No loop exit edges for program point %r' %
                           header.program_point)
    
    
    def get_abstract_vertex(self, header):
        try:
            return self.__abstract_vertices[header]
        except KeyError:
            raise KeyError('No abstract vertex for program point %r' %
                           header.program_point)
        
        
    def induce_subgraph_with_tails_and_exits(self, 
                                             name,
                                             control_flow_graph, 
                                             header_vertex):        
        induced_graph = ControlFlowGraph(name)
        inner_header_vertices = set()
        
        def add_vertices():
            # Duplicate vertex in the loop body
            for vertex in self.__loop_body_of_header[header_vertex]:
                induced_graph.add_vertex(ProgramPointVertex(vertex.vertex_id,
                                                            vertex.program_point))
            
            # Add a vertex for each loop-exit edge out of this loop
            for (_, succ_vertex) in self.__loop_exit_edges_of_header[header_vertex]:
                induced_graph.add_vertex(ProgramPointVertex(succ_vertex.vertex_id,
                                                            succ_vertex.program_point))
            
            # Add an abstract vertex per inner loop
            for succ_edge in self.get_abstract_vertex(header_vertex).successor_edge_iterator():
                succ_vertex = self.get_vertex(succ_edge.vertex_id)
                if succ_vertex.abstract \
                and ProgramPointVertex.is_basic_block(succ_vertex.program_point):
                    # Inner header detected
                    induced_graph.add_vertex(ProgramPointVertex\
                                (succ_vertex.vertex_id,
                                 succ_vertex.program_point,
                                 True))
                    inner_header_vertices.add(control_flow_graph.\
                                              get_vertex_for_program_point\
                                                (succ_vertex.program_point))            
                            
        
        def add_edges_at_this_loop_nesting_level():
            for induced_vertex in induced_graph:
                if not induced_vertex.abstract:
                    vertex = control_flow_graph.get_vertex(induced_vertex.vertex_id)
                    for succ_edge in vertex.successor_edge_iterator():
                        succ_vertex = control_flow_graph.get_vertex(succ_edge.vertex_id)
                        # Check that this edge does not induce a cycle
                        if (vertex, succ_vertex) not in self.__loop_body_of_backedge.keys():
                            if induced_graph.has_vertex(succ_edge.vertex_id):
                                induced_graph.add_edge(induced_vertex, 
                                                       induced_graph.get_vertex(succ_edge.vertex_id), 
                                                       None)
                            elif succ_vertex in self.__abstract_vertices:
                                inner_abstract_vertex = self.__abstract_vertices[succ_vertex]
                                induced_graph.add_edge(induced_vertex, 
                                                       induced_graph.get_vertex(inner_abstract_vertex.vertex_id), 
                                                       None)
        
        def add_edges_for_loop_exit_edges():
            for (pred_vertex, succ_vertex) in self.__loop_exit_edges_of_header[header_vertex]:
                induced_pred_vertex = induced_graph.get_vertex(pred_vertex.vertex_id)
                if not induced_pred_vertex.has_successor(succ_vertex.vertex_id):
                    induced_succ_vertex = induced_graph.get_vertex(succ_vertex.vertex_id)
                    induced_graph.add_edge(induced_pred_vertex, 
                                           induced_succ_vertex, 
                                           None)
        
        
        def add_edges_for_inner_loops():
            for inner_header_vertex in inner_header_vertices:
                for (_, succ_vertex) in self.__loop_exit_edges_of_header[inner_header_vertex]: 
                    induced_graph.add_edge(induced_graph.get_vertex(self.__abstract_vertices[inner_header_vertex].vertex_id),
                                           induced_graph.get_vertex(succ_vertex.vertex_id),
                                           None)
                    
                    
        def add_dummy_exit_vertex_if_required():
            without_successors = []
            for vertex in induced_graph:
                if vertex.number_of_successors() == 0:
                    without_successors.append(vertex)
                    
            if len(without_successors) > 1:
                dummy_program_point_vertex = ProgramPointVertex(induced_graph.get_new_vertex_id(),
                                                                None)
                induced_graph.add_vertex(dummy_program_point_vertex)
                for vertex in without_successors:
                    induced_graph.add_edge(vertex,
                                           dummy_program_point_vertex,
                                           None)
        
        add_vertices()
        add_edges_at_this_loop_nesting_level()
        add_edges_for_loop_exit_edges()
        add_edges_for_inner_loops()  
        add_dummy_exit_vertex_if_required()
        induced_graph.entry_vertex = induced_graph.find_entry_vertex()
        induced_graph.exit_vertex = induced_graph.find_exit_vertex()
        dot.make_file(induced_graph)
        return induced_graph
           
            
    def dot_filename(self):
        return '%s.%s.lnt' % (config.get_filename_prefix(), self._name)
        
        
    def __construct(self, 
                    control_flow_graph, 
                    pre_dominator_tree,
                    depth_first_search_tree):
        # The parent of a vertex in the loop-nesting tree is initially itself
        for vertex in control_flow_graph:
            self.__current_parent[vertex] = vertex
        self.__find_loops(control_flow_graph,
                          pre_dominator_tree,
                          depth_first_search_tree)
        self.__add_vertices(control_flow_graph)
        self.__add_edges(control_flow_graph)
        

    def __find_loops(self, 
                     control_flow_graph, 
                     pre_dominator_tree, 
                     depth_first_search_tree):
        for vertex in reversed(depth_first_search_tree.pre_order):
            for pred_edge in vertex.predecessor_edge_iterator():
                pred_vertex = control_flow_graph.get_vertex(pred_edge.vertex_id)
                if (pred_vertex, vertex) in depth_first_search_tree.backedges:
                    if not pre_dominator_tree.is_ancestor\
                        (pre_dominator_tree.get_vertex(vertex.vertex_id),
                         pre_dominator_tree.get_vertex(pred_vertex.vertex_id)):
                        raise IrreducibleLoopError('Depth-first backedge' 
                                                   ' (%d, %d) identifies an'
                                                   ' irreducible loop' % 
                                                   (pred_vertex.vertex_id,
                                                    vertex.vertex_id))
                    self.__find_loop_body(control_flow_graph,
                                          depth_first_search_tree,
                                          pred_vertex, 
                                          vertex)
        
            
    def __find_loop_body(self, 
                         control_flow_graph, 
                         depth_first_search_tree, 
                         tail, 
                         header):
        if header not in self.__inner_loop_headers_per_header:  
            self.__inner_loop_headers_per_header[header] = set()
        self.__loop_body_of_backedge[(tail, header)] = set()            
        self.__loop_body_of_backedge[(tail, header)].add(header)
        # Compute the loop body by moving up the graph from the loop tail until
        # we reach the header
        work_list = [tail]
        while work_list:
            work_list_vertex = work_list.pop()
            self.__loop_body_of_backedge[(tail, header)].add(work_list_vertex)
            for pred_edge in work_list_vertex.predecessor_edge_iterator():
                pred_vertex = control_flow_graph.\
                                get_vertex(pred_edge.vertex_id)
                if (pred_vertex, work_list_vertex) not in\
                    depth_first_search_tree.backedges:
                    representative_vertex = self.__current_parent[pred_vertex]
                    if representative_vertex not in work_list\
                    and representative_vertex not in self.__loop_body_of_backedge[(tail, header)]\
                    and representative_vertex != header:
                        work_list.append(representative_vertex)
        # Update internally to reflect the loop body just found
        for vertex in self.__loop_body_of_backedge[(tail, header)]:
            self.__current_parent[vertex] = header
            if vertex not in self.__state_to_header:
                self.__state_to_header[vertex] = header
            if vertex in self.__inner_loop_headers_per_header and vertex != header:
                self.__inner_loop_headers_per_header[header].add(vertex)
                
        self.__loop_body_of_backedge[(tail, header)].difference_update\
            (self.__inner_loop_headers_per_header[header])


    def __add_vertices(self, control_flow_graph):
        for vertex in control_flow_graph:
            self.add_vertex(ProgramPointVertex(vertex.vertex_id,
                                               vertex.program_point))
        # Add an abstract vertex per loop tail
        for (tail, header) in self.__loop_body_of_backedge.keys():
            abstract_vertex = ProgramPointVertex(self.get_new_vertex_id(),
                                                 tail.program_point,
                                                 True) 
            self.add_vertex(abstract_vertex)
            self.__abstract_vertices[tail] = abstract_vertex
            if header not in self.__abstract_vertices:
                abstract_vertex = ProgramPointVertex(self.get_new_vertex_id(),
                                                     header.program_point,
                                                     True) 
                self.add_vertex(abstract_vertex)
                self.__abstract_vertices[header] = abstract_vertex
        # Set the tree root to the abstract vertex representing the root of the 
        # directed graph
        self._root_vertex = self.__abstract_vertices[control_flow_graph.entry_vertex]
    
    
    def __add_edges(self, control_flow_graph):
        # Add edges between abstract vertices representing headers
        for outer_header_vertex in self.__inner_loop_headers_per_header.keys():
            for inner_header_vertex in self.__inner_loop_headers_per_header\
                                                [outer_header_vertex]:
                    self.add_edge(self.__abstract_vertices[outer_header_vertex],
                                  self.__abstract_vertices[inner_header_vertex])
        # Add edges between abstract vertices representing headers and abstract
        # vertices representing tails
        for (tail, header) in self.__loop_body_of_backedge.keys():
            self.add_edge(self.__abstract_vertices[header], 
                          self.__abstract_vertices[tail])
        # Add edges between abstract vertices and program point vertices.
        # First work out which headers have multiple backedges and, for each
        # such header, the set of vertices shared among those loops.  Then, 
        # for each vertex in the shared loop body, set the header's abstract
        # vertex as the parent.  For vertices not in the shared loop body, set 
        # the tail's abstract vertex as the parent.  For vertices in loops whose
        # bodies are not shared, set the tail's abstract vertex as the parent 
        header_to_loop_body = {}
        for (tail, header), loop_body in self.__loop_body_of_backedge.items():
            if header not in header_to_loop_body:
                header_to_loop_body[header] = set(loop_body)
            else:
                header_to_loop_body[header].intersection_update(loop_body)
                
        for (tail, header), loop_body in self.__loop_body_of_backedge.items():
            for vertex in loop_body:
                if header_to_loop_body[header] == loop_body:
                    self.add_edge(self.__abstract_vertices[tail], 
                                  self.get_vertex(vertex.vertex_id))
                else:
                    if vertex in header_to_loop_body[header]:
                        if not self.__abstract_vertices[header].has_successor(vertex.vertex_id):
                            self.add_edge(self.__abstract_vertices[header], 
                                          self.get_vertex(vertex.vertex_id))
                    else:
                        self.add_edge(self.__abstract_vertices[tail], 
                                  self.get_vertex(vertex.vertex_id))
                        
                        
    def __compute_loop_body_for_each_header(self, control_flow_graph):
        for vertex in self.__abstract_vertices.keys():
            if ProgramPointVertex.is_basic_block(vertex.program_point):
                self.__loop_body_of_header[vertex] = set()
                stack = [self.__abstract_vertices[vertex]]
                while stack:
                    stack_vertex = stack.pop()
                    for succ_edge in stack_vertex.successor_edge_iterator():
                        succ_vertex = self.get_vertex(succ_edge.vertex_id)
                        if not succ_vertex.abstract:
                            self.__loop_body_of_header[vertex].\
                                add(control_flow_graph.get_vertex(succ_vertex.vertex_id))
                        else:
                            if not ProgramPointVertex.is_basic_block(succ_vertex.program_point):
                                stack.append(succ_vertex)
        
                        
    def __find_loop_exits(self, control_flow_graph):
        for (tail, header), loop_body in self.__loop_body_of_backedge.items():
            self.__loop_exit_edges_of_backedge[(tail, header)] = set()
            for vertex in loop_body:
                for succ_edge in vertex.successor_edge_iterator():
                    succ_vertex = control_flow_graph.\
                                    get_vertex(succ_edge.vertex_id) 
                    if succ_vertex not in loop_body:
                        if not self.is_loop_header(succ_vertex):
                            self.__loop_exit_edges_of_backedge\
                                [(tail, header)].add((vertex, succ_vertex))
        
        
        for header in self.__loop_body_of_header.keys():
            self.__loop_exit_edges_of_header[header] = set()
            loop_body = self.__loop_body_of_header[header]
            for (_, other_header), loop_exit_edges in\
                self.__loop_exit_edges_of_backedge.items():
                if header == other_header:
                    for (pred_vertex, succ_vertex) in loop_exit_edges:
                        if succ_vertex not in loop_body:
                            self.__loop_exit_edges_of_header[header].\
                                add((pred_vertex, succ_vertex))
        return self.__loop_exit_edges_of_header[header]
    


class PathExpression(DirectedGraph):
    
    """
    Models a path expression between two states in a control flow graph.
    """    
    
    @staticmethod
    def create_sequence_from_list_of_program_points(the_list,
                                                    name,
                                                    pred_vertex,
                                                    succ_vertex):
        path_expression = PathExpression(name,
                                         pred_vertex,
                                         succ_vertex)
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
        
    
    def __init__(self, name, pred_vertex, succ_vertex):   
        DirectedGraph.__init__(self, name)
        self._pred_vertex = pred_vertex
        self._succ_vertex = succ_vertex
        self._root_vertex = None
        self.__string = None
        
        
    @property
    def pred_vertex(self):
        return self._pred_vertex
    
    
    @property
    def succ_state(self):
        return self._succ_vertex
    
    
    @property
    def root_vertex(self):
        return self._root_vertex
    
    
    new_vertex_id = 0
    def get_new_vertex_id(self):
        PathExpression.new_vertex_id += 1
        return PathExpression.new_vertex_id     
    
    
    def dot_filename(self):
        return '%s.%s.%d_%d.pe' % (config.get_filename_prefix(), 
                                   self._name,
                                   self._pred_vertex.vertex_id,
                                   self._succ_vertex.vertex_id)  
        
    
    def __str__(self):
        if self.__string is None:
            cached_strings = {}
            depth_first_search = DepthFirstSearch(self, self._root_vertex)
            for vertex in depth_first_search.post_order:
                if isinstance(vertex, RegularExpressionVertex):
                    string = ''
                    # Do we need to parenthesise the sub-expression?
                    if vertex.operator in [RegularExpressionVertex.ALTERNATIVE,
                                           RegularExpressionVertex.MIGHT_ITERATE,
                                           RegularExpressionVertex.MUST_ITERATE]:
                        string += '['
                    
                    # Construct the meat of the expression
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
                    if vertex.operator == RegularExpressionVertex.MIGHT_ITERATE:
                        string += '*'
                    elif vertex.operator == RegularExpressionVertex.MUST_ITERATE:
                        string += '+'
                        
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
            raise KeyError('Unable to find SCC for program point %r' % 
                           program_point)
        
    
    def __iter__(self):
        """
        Iterate through each strong component.
        """        
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
    def create_for_loop(control_flow_graph, loop_nesting_tree, header_vertex):
        name = '%s.%d' % (control_flow_graph.name, header_vertex.vertex_id)
        super_block_graph = SuperBlockGraph(name)
        induced_subgraph = loop_nesting_tree.\
                            induce_subgraph_with_tails_and_exits(name,
                                                                 control_flow_graph,
                                                                 header_vertex)
        pre_dominator_tree = induced_subgraph.get_pre_dominator_tree()
        post_dominator_tree = induced_subgraph.get_post_dominator_tree()
        dominator_graph = DominatorGraph(name,
                                         pre_dominator_tree,
                                         post_dominator_tree)
        strong_components = StronglyConnectedComponents(dominator_graph) 
         
        super_block_graph.add_super_blocks(loop_nesting_tree,
                                           header_vertex,
                                           induced_subgraph,
                                           strong_components)
        super_block_graph.add_edges(header_vertex,
                                    induced_subgraph) 
        super_block_graph.check_properties()       
        return super_block_graph
    
    
    def __init__(self, name):
        DirectedGraph.__init__(self, name)
        self._root_vertex = None
        self.__program_point_to_super_vertex = {}
    
    
    @property
    def root_vertex(self):
        return self._root_vertex
    
    
    def add_super_blocks(self, 
                         loop_nesting_tree,
                         header_vertex,
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
                self.__program_point_to_super_vertex\
                    [induced_vertex.program_point] = super_vertex
                super_vertex.program_points.append(induced_vertex.program_point) 
                
                if not induced_vertex.abstract:
                    super_vertex.representative = induced_vertex.program_point
                    
                if not ProgramPointVertex.is_basic_block(induced_vertex.program_point):
                    for (_, succ_vertex) in loop_nesting_tree.get_loop_exit_edges_for_header(header_vertex):
                        if succ_vertex.program_point == induced_vertex.program_point:
                            super_vertex.is_loop_exit_edge = True
                            break
                    
        # The root vertex of the super block graph is the super block containing
        # the header
        self._root_vertex = self.__program_point_to_super_vertex[header_vertex.program_point] 
    
        
    def add_edges(self, header_vertex, induced_subgraph):
        for super_vertex in self:
            induced_vertex = induced_subgraph.get_vertex_for_program_point\
                                (super_vertex.program_points[0])
            if not ProgramPointVertex.is_basic_block(super_vertex.program_points[0]):
                # The first program point in the super block is a control-flow 
                # edge
                assert induced_vertex.number_of_predecessors() == 1
                pred_edge = induced_vertex.get_ith_predecessor_edge(0)
                pred_induced_vertex = induced_subgraph.\
                                        get_vertex(pred_edge.vertex_id)
                pred_super_vertex = self.__program_point_to_super_vertex\
                                            [pred_induced_vertex.program_point]
                self.add_edge(pred_super_vertex, super_vertex)                
                pred_super_vertex.\
                    add_successor_edge_to_partition(pred_induced_vertex,
                                                    pred_super_vertex.\
                                                        get_successor_edge(super_vertex.vertex_id))
                
            elif super_vertex.program_points[0] != header_vertex.program_point:
                # The first program point in the super block is a basic block 
                # but not the loop header of this region
                assert induced_vertex.number_of_predecessors() > 1
                for pred_edge in induced_vertex.predecessor_edge_iterator():
                    pred_induced_vertex = induced_subgraph.\
                                            get_vertex(pred_edge.vertex_id)
                    pred_super_vertex = self.__program_point_to_super_vertex\
                                            [pred_induced_vertex.program_point]
                    self.add_edge(pred_super_vertex, super_vertex)
                    
    
    def check_properties(self):
        visited = {}
        for super_vertex in self:
            visited[super_vertex] = False
            assert super_vertex.representative is not None,\
            'No representative found for super block with program points ' +\
            ','.join(str(program_point) for program_point in super_vertex.program_points)
            
        
        stack = [self._root_vertex]
        while stack:
            stack_vertex = stack.pop()
            visited[stack_vertex] = True
            for succ_edge in stack_vertex.successor_edge_iterator():
                stack.append(self.get_vertex(succ_edge.vertex_id))
        
        for super_vertex in self:
            assert visited[super_vertex],\
            'Super block with program points ' +\
            ','.join(str(program_point) for program_point in super_vertex.program_points) +\
            ' can not be reached from root vertex'
    
    
    def dot_filename(self):
        return '%s.%s.super' % (config.get_filename_prefix(), 
                                self._name)
            
        