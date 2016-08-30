"""
This module includes all directed graphs used in different types of analyses. 
"""


import collections
import abc
import random

from tools.lib.utils import dot
from tools.lib.utils import config
from tools.lib.system import edges
from tools.lib.system import vertices



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
        pred_vertex.add_successor_edge(edges.Edge(succ_vertex.vertex_id, 
                                                  edge_id))
        succ_vertex.add_predecessor_edge(edges.Edge(pred_vertex.vertex_id, 
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
        self._depth_first_search_tree = None
        self._pre_dominator_tree = None
        self._post_dominator_tree = None
        self._loop_nesting_tree = None
        self._reverse_control_flow_graph = None
        
        
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
        pred_vertex.add_successor_edge(edges.TransitionEdge(succ_vertex.vertex_id, 
                                                            edge_id, 
                                                            path_expression))
        succ_vertex.add_predecessor_edge(edges.TransitionEdge(pred_vertex.vertex_id, 
                                                              edge_id, 
                                                              path_expression))
        
    
    def get_vertex_for_program_point(self, program_point):
        try:
            return self._program_point_to_vertex[program_point]
        except KeyError:
            raise KeyError('No vertex found for program point %r' % program_point)
            
        
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
    
    
    def get_depth_first_search_tree(self):
        if self._depth_first_search_tree is None:
            self._depth_first_search_tree = DepthFirstSearch\
                                                (self, 
                                                 self._entry_vertex)
        return self._depth_first_search_tree
    
            
    def get_loop_nesting_tree(self):
        if self._loop_nesting_tree is None:
            self._loop_nesting_tree = LoopNestingHierarchy\
                                        (self, 
                                         self.get_pre_dominator_tree(),
                                         self.get_depth_first_search_tree())
            dot.make_file(self._loop_nesting_tree)
        return self._loop_nesting_tree
    
    
    def get_pre_dominator_tree(self):
        if self._pre_dominator_tree is None:
            self._pre_dominator_tree = Dominators(self, Dominators.PRE)
            dot.make_file(self._pre_dominator_tree)
        return self._pre_dominator_tree
    
    
    def get_post_dominator_tree(self):
        if self._post_dominator_tree is None:
            self._post_dominator_tree = Dominators(self.get_reverse_control_flow_graph(),
                                                   Dominators.POST)
            dot.make_file(self._post_dominator_tree)
        return self._post_dominator_tree
    
    
    def get_reverse_control_flow_graph(self):
        if self._reverse_control_flow_graph is None:
            self._reverse_control_flow_graph = ControlFlowGraph(self._name)
            for vertex in self:
                self._reverse_control_flow_graph.\
                    add_vertex(vertices.ProgramPointVertex(vertex.vertex_id,
                                                           vertex.program_point))
            for vertex in self:
                for succ_edge in vertex.successor_edge_iterator():
                    self._reverse_control_flow_graph.\
                        add_edge(self._reverse_control_flow_graph.\
                                    get_vertex(succ_edge.vertex_id), 
                                 self._reverse_control_flow_graph.\
                                    get_vertex(vertex.vertex_id), 
                                 succ_edge.path_expression)
            self._reverse_control_flow_graph._entry_vertex\
                = self._reverse_control_flow_graph.get_vertex\
                    (self._exit_vertex.vertex_id)
            self._reverse_control_flow_graph._exit_vertex\
                = self._reverse_control_flow_graph.get_vertex\
                    (self._entry_vertex.vertex_id)
        return self._reverse_control_flow_graph
    
    
    def split_program_points_into_basic_blocks_and_edges(self):
        basic_blocks = set()
        control_flow_edges = set()
        for vertex in self:
            if isinstance(vertex.program_point, int):
                basic_blocks.add(vertex.program_point)
            else:
                control_flow_edges.add(vertex.program_point)
        return basic_blocks, control_flow_edges
            
            
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

        # Reduce graph until removal of vertex violates path reconstructibility   
        changed = True
        while changed:
            changed = False
            candidiate_states = list(self._vertices.values())
            random.shuffle(candidiate_states)
            for vertex in candidiate_states:
                if can_remove_program_point(vertex):
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
        
    
    def get_vertex_with_name(self, function_name):
        try:
            return self.function_name_to_vertex[function_name]
        except ValueError:
            raise ValueError('No vertex found for function %s' % function_name)
        
    
    def add_edge(self, pred_vertex, succ_vertex, call_site_id):
        edge_id = self.get_new_edge_id()
        if not pred_vertex.has_successor(succ_vertex.vertex_id):
            succ_edge = edges.CallGraphEdge(succ_vertex.vertex_id, edge_id) 
            pred_vertex.add_successor_edge(succ_edge)
        if not succ_vertex.has_predecessor(pred_vertex.vertex_id):
            pred_edge = edges.CallGraphEdge(pred_vertex.vertex_id, edge_id)
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
                parent_vertex = self.get_vertex(vertex.parent_id)
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

    
        
    def add_edge(self, pred_vertex, succ_vertex):
        DirectedGraph.add_edge(self, pred_vertex, succ_vertex)
        succ_vertex.parent_id = pred_vertex.vertex_id
        
        
    def remove_edge(self, pred_vertex, succ_vertex):
        succ_vertex.parent_id = None
        pred_vertex.remove_successor_edge(succ_vertex.vertex_id)
        succ_vertex.remove_predecessor_edge(pred_vertex.vertex_id)
    
    
    def is_ancestor(self, candidate_ancestor_vertex, vertex):
        if candidate_ancestor_vertex == vertex:
            return True
        elif vertex == self._root_vertex:
            return False
        else:
            parent = self.get_vertex(vertex.parent_id)
            while parent != self._root_vertex\
            and parent != candidate_ancestor_vertex:
                vertex = parent
                parent = self.get_vertex(vertex.parent_id)
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

    def __init__(self, directed_graph, root_vertex):
        Tree.__init__(self, directed_graph.name)
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
            self.add_vertex(vertices.Vertex(vertex.vertex_id))
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
        for succ_edge in vertex.successor_edge_iterator():
            succ_vertex = directed_graph.get_vertex(succ_edge.vertex_id)
            if self._vertex_pre_order_numbering[succ_vertex] is None:
                self.add_edge(self.get_vertex(vertex.vertex_id), 
                              self.get_vertex(succ_edge.vertex_id))
                self.__do_search(directed_graph, succ_vertex)
            elif self._vertex_pre_order_numbering[vertex]\
                    < self._vertex_pre_order_numbering[succ_vertex]:
                pass
            elif self._vertex_post_order_numbering[succ_vertex] is None:
                self._backedges.append((vertex, succ_vertex))
        # Append vertex to post-order
        self._vertex_post_order_numbering[vertex] = self.__post_orderID
        self._post_order.append(vertex)
        self.__post_orderID += 1



class Dominators(Tree):
    PRE  = 'pre'
    POST = 'post'
    
    def __init__(self, control_flow_graph, tree_type):
        assert tree_type == Dominators.PRE or tree_type == Dominators.POST,\
        '%s is an invalid type of dominator tree' % tree_type
        Tree.__init__(self, control_flow_graph.name)
        self.__tree_type = tree_type
        self.__immediate_dominator = {}
        self.__initialise(control_flow_graph)
        self.__solve(control_flow_graph)
        self.__add_vertices(control_flow_graph)
        self.__add_edges(control_flow_graph)
        
    
    def __initialise(self, control_flow_graph):
        for vertex in control_flow_graph:
            self.__immediate_dominator[vertex] = vertex if \
                vertex == control_flow_graph.entry_vertex else None
                
    
    def __solve(self, control_flow_graph):
        depth_first_search = control_flow_graph.get_depth_first_search_tree()
        changed = True
        while changed:
            changed = False
            post_order_id = control_flow_graph.number_of_vertices()
            while post_order_id >= 1:
                vertex = depth_first_search.\
                            get_vertex_with_this_post_order_value(post_order_id)
                if vertex != control_flow_graph.entry_vertex:
                    pred_vertex_processed = None
                    new_immediate_dominator = None
                    for pred_edge in vertex.predecessor_edge_iterator():
                        pred_vertex = control_flow_graph.\
                                        get_vertex(pred_edge.vertex_id)
                        if self.__immediate_dominator[pred_vertex] is not None:
                            pred_vertex_processed   = pred_vertex
                            new_immediate_dominator = pred_vertex_processed
                    for pred_edge in vertex.predecessor_edge_iterator():
                        pred_vertex = control_flow_graph.\
                                        get_vertex(pred_edge.vertex_id)
                        if pred_vertex != pred_vertex_processed:
                            if self.__immediate_dominator[pred_vertex] is not None:
                                new_immediate_dominator =\
                                    self.__intersect(depth_first_search, 
                                                     pred_vertex, 
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
            self.add_vertex(vertices.ProgramPointVertex(vertex.vertex_id,
                                                        vertex.program_point))
            
    
    def __add_edges(self, control_flow_graph):
        # Add dominator edges between states
        for vertex, immediate_dominator in self.__immediate_dominator.items():
            if vertex != control_flow_graph.entry_vertex:
                self.add_edge(self.get_vertex(immediate_dominator.vertex_id), 
                              self.get_vertex(vertex.vertex_id))


    def dot_filename(self):
        return '%s.%s.%s' % (config.get_filename_prefix(), 
                             self.name, 
                             self.__tree_type)



class LoopNestingHierarchy(Tree):
    
    """
    Models the loop-nesting hierarchy of a function in a program.
    """
    
    def __init__(self, 
                 control_flow_graph, 
                 pre_dominator_tree, 
                 depth_first_search_tree):
        Tree.__init__(self, control_flow_graph.name)
        self.__loop_body_of_backedge = {}
        self.__abstract_vertices = {}
        self.__current_parent = {}
        self.__state_to_header = {}
        self.__inner_loop_headers_per_header = {}
        self.__construct(control_flow_graph, 
                         pre_dominator_tree, 
                         depth_first_search_tree)
        # The following are used to cache results of queries on this data
        # structure
        self.__loop_bodies_per_header = {}
    
    
    def backedge_and_loop_body_iterator(self):
        for (tail, header), loop_body in self.__loop_body_of_backedge.items():
            yield (tail, header), loop_body
            
    
    def header_iterator(self):
        for header in self.__abstract_vertices.keys():
            yield header
    
    
    def backedge_iterator(self):
        for (tail, header) in self.__loop_body_of_backedge.keys():
            yield (tail, header)
    
             
    def is_loop_header(self, vertex):
        return vertex in self.__abstract_vertices.keys()
                 
            
    def get_loop_body_for_header(self, header):
        if header not in self.__loop_bodies_per_header:
            loop_body = set()
            for backedge in self.__loop_body_of_backedge.keys():
                if backedge[1] == header:
                    loop_body.update(self.__loop_body_of_backedge[backedge])
            assert loop_body
            self.__loop_bodies_per_header[header] = loop_body
        try:
            return self.__loop_bodies_per_header[header]
        except KeyError:
            raise KeyError('The vertex %r is not a valid loop header' %
                           header)
            
            
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
        work_list = []
        work_list.append(tail)
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
            self.add_vertex(vertices.ProgramPointVertex(vertex.vertex_id,
                                                        vertex.program_point))
        # Add an abstract vertex per loop tail
        for (tail, header) in self.__loop_body_of_backedge.keys():
            abstract_vertex = vertices.LoopInternalVertex(self.get_new_vertex_id(),
                                                          tail.program_point) 
            self.add_vertex(abstract_vertex)
            self.__abstract_vertices[tail] = abstract_vertex
            if header not in self.__abstract_vertices:
                abstract_vertex = vertices.LoopInternalVertex(self.get_new_vertex_id(),
                                                              header.program_point) 
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
        
                        
    def __find_loop_exits(self, state_transition_graph):
        for (tail, header), loop_body in self.__loop_body_of_backedge.items():
            for vertex in loop_body:
                for succ_edge in vertex.successor_edge_iterator():
                    succ_vertex = state_transition_graph.\
                                    get_vertex(succ_edge.vertex_id) 
                    if succ_vertex not in loop_body:
                        if self.is_loop_header(vertex) and vertex != header:
                            if succ_vertex not in self.loop_bodies_per_header[vertex]:
                                self.loop_exit_edges_per_backedge[(tail, header)].\
                                    add((vertex, succ_vertex))
                                self.loop_bodies_per_header[header].\
                                    add((vertex, succ_vertex))
                        else:
                            self.loop_exit_edges_per_backedge[(tail, header)].\
                                add((vertex, succ_vertex))
                            self.loop_bodies_per_header[header].\
                                    add((vertex, succ_vertex))
                            
    
    def __find_loop_entries(self, state_transition_graph):
        for header in self.loop_bodies_per_header.keys():
            for pred_edge in header.predecessor_edge_iterator():
                pred_vertex = state_transition_graph.get_vertex(pred_edge.vertex_id)
                if (pred_vertex, header) not in self.__loop_body_of_backedge.keys():
                    self.loop_entry_edges_per_header[header].add((pred_vertex, header))
    


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
            vertices.RegularExpressionVertex(path_expression.get_new_vertex_id(),
                                             vertices.RegularExpressionVertex.SEQUENCE)
        path_expression.add_vertex(sequence_vertex)
        path_expression._root_vertex = sequence_vertex
        for program_point in the_list:
            program_point_vertex = vertices.ProgramPointVertex(path_expression.get_new_vertex_id(),
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
                if isinstance(vertex, vertices.RegularExpressionVertex):
                    string = ''
                    # Do we need to parenthesise the sub-expression?
                    if vertex.operator in [vertices.RegularExpressionVertex.ALTERNATIVE,
                                           vertices.RegularExpressionVertex.MIGHT_ITERATE,
                                           vertices.RegularExpressionVertex.MUST_ITERATE]:
                        string += '['
                    
                    # Construct the meat of the expression
                    counter = 1
                    for succ_edge in vertex.successor_edge_iterator():
                        succ_vertex = self.get_vertex(succ_edge.vertex_id)
                        if isinstance(succ_vertex, vertices.ProgramPointVertex):
                            string += str(succ_vertex)
                        else:
                            string += cached_strings[succ_edge]
                        
                        if counter < vertex.number_of_successors():
                            string += vertex.operator
                        counter += 1
                    
                    # Do we need to parenthesise the sub-expression?
                    if vertex.operator in [vertices.RegularExpressionVertex.ALTERNATIVE,
                                           vertices.RegularExpressionVertex.MIGHT_ITERATE,
                                           vertices.RegularExpressionVertex.MUST_ITERATE]:
                        string += ']'
                    # Repeat the expression zero or more times?
                    if vertex.operator == vertices.RegularExpressionVertex.MIGHT_ITERATE:
                        string += '*'
                    elif vertex.operator == vertices.RegularExpressionVertex.MUST_ITERATE:
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
                
            
            
        