"""
This module includes all directed graphs used in different types of analyses. 
"""


import collections
import abc
import random
import math

from lib.utils import dot
from lib.utils import config
from lib.utils import debug

from lib.system.edges import (Edge,
                                    TransitionEdge,
                                    CallGraphEdge)

from lib.system.vertices import (Vertex, 
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
        if vertex.vertex_id in self._vertices:
            raise DuplicateVertexError('The graph already has vertex {}'.\
                                       format(vertex.vertex_id))
        self._vertices[vertex.vertex_id] = vertex
        
    
    def get_vertex(self, vertex_id):
        try:
            return self._vertices[vertex_id]
        except KeyError:
            raise KeyError('Vertex {} is not in the graph'.format(vertex_id))
        
        
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
        return '{}(vertices={})'.format(self.__class__.__name__,
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
    """
    
    class ArtificialLoopBody():
        """
        A loop body created during artificial construction of a control flow graph.
        """
        
        def __init__(self, 
                     control_flow_graph, 
                     number_of_vertices, 
                     nested_loop_components):
            self._header_vertex = None
            self.__created_vertices = set()
            self.__disconnected_vertices = []
            self.__singleton_vertices = []
            self.__free_seses = []
            self.add_vertices(control_flow_graph, 
                              number_of_vertices)
            self.create_sese_regions(control_flow_graph,
                                     number_of_vertices, 
                                     nested_loop_components)
            self.connect_sese_regions(control_flow_graph)
            self.connect_singleton_vertices(control_flow_graph)
            self.add_nested_loop_entry_and_exit_edges(control_flow_graph,
                                                      nested_loop_components)
            self.add_backedges_and_find_exit_sources(control_flow_graph)  
            
            
        @property
        def header_vertex(self):
            return self._header_vertex
            
            
        def add_vertices(self, control_flow_graph, number_of_vertices):
            for _ in range(1, number_of_vertices+1):
                vertex_id = control_flow_graph.get_new_vertex_id()
                vertex = ProgramPointVertex(vertex_id, vertex_id)
                control_flow_graph.add_vertex(vertex)
                self.__created_vertices.add(vertex)
                self.__disconnected_vertices.append(vertex)
            
            
        def create_sese_regions(self, 
                                control_flow_graph, 
                                number_of_vertices, 
                                nested_loop_components):
            remaining_vertices = number_of_vertices - len(nested_loop_components)
            number_of_singleton_vertices = len(nested_loop_components)
            
            def how_many_components(component_size):
                nonlocal remaining_vertices
                number_of_components = 0
                if remaining_vertices >= component_size:
                    number_of_components = random.randint\
                                            (1, math.floor(remaining_vertices/component_size))
                    remaining_vertices -= component_size * number_of_components
                return number_of_components
            
            number_of_if_then_else = how_many_components(4)\
                                        if random.random() > 0.3 else 0
            number_of_short_circuit = how_many_components(5)\
                                        if random.random() > 0.3 else 0
            number_of_if_then = how_many_components(3)\
                                    if random.random() > 0.3 else 0
            number_of_switch = how_many_components(2 + config.Arguments.fan_out)\
                                    if random.random() > 0.3 else 0
            
            number_of_singleton_vertices += remaining_vertices
            for _ in range(1, number_of_singleton_vertices):
                vertex = self.__disconnected_vertices.pop()
                self.__singleton_vertices.append(vertex)
                
            while number_of_if_then_else > 0 or number_of_if_then > 0\
            or number_of_short_circuit > 0 or number_of_switch > 0:
                if number_of_if_then_else > 0 and bool(random.getrandbits(1)):
                    sese = self.create_if_then_else(control_flow_graph)
                    self.__free_seses.append(sese)
                    number_of_if_then_else -= 1
                if number_of_if_then > 0 and bool(random.getrandbits(1)):
                    sese = self.create_if_then(control_flow_graph)
                    self.__free_seses.append(sese)
                    number_of_if_then -= 1
                if number_of_short_circuit > 0 and bool(random.getrandbits(1)):
                    sese = self.create_short_circuit(control_flow_graph)
                    self.__free_seses.append(sese)
                    number_of_short_circuit -= 1
                if number_of_switch > 0 and bool(random.getrandbits(1)):
                    sese = self.create_switch(control_flow_graph)
                    self.__free_seses.append(sese)
                    number_of_switch -= 1
        
        
        def create_switch(self, control_flow_graph):
            branch_vertex = self.__disconnected_vertices.pop()
            merge_vertex = self.__disconnected_vertices.pop()
            for _ in range(1, config.Arguments.fan_out+1):
                switch_arm = self.create_sese_region(control_flow_graph)
                control_flow_graph.add_edge(branch_vertex, switch_arm[0], None)
                control_flow_graph.add_edge(switch_arm[1], merge_vertex, None)
            return [branch_vertex, merge_vertex]
        
        
        def create_short_circuit(self, control_flow_graph):
            outer_branch_vertex = self.__disconnected_vertices.pop()
            inner_branch_vertex = self.__disconnected_vertices.pop()
            merge_vertex = self.__disconnected_vertices.pop()
            then_branch = self.create_sese_region(control_flow_graph)
            else_branch = self.create_sese_region(control_flow_graph)
            control_flow_graph.add_edge(outer_branch_vertex, 
                                        inner_branch_vertex, 
                                        None)
            control_flow_graph.add_edge(outer_branch_vertex,
                                        else_branch[0], 
                                        None)
            control_flow_graph.add_edge(inner_branch_vertex, 
                                        then_branch[0], 
                                        None)
            control_flow_graph.add_edge(inner_branch_vertex, 
                                        else_branch[0], 
                                        None)
            control_flow_graph.add_edge(then_branch[1], 
                                        merge_vertex, 
                                        None)
            control_flow_graph.add_edge(else_branch[1], 
                                        merge_vertex, 
                                        None)
            return [outer_branch_vertex, merge_vertex]
        
        
        def create_if_then(self, control_flow_graph):
            branch_vertex = self.__disconnected_vertices.pop()
            merge_vertex = self.__disconnected_vertices.pop()
            then_branch = self.create_sese_region(control_flow_graph)
            control_flow_graph.add_edge(branch_vertex, 
                                        then_branch[0], 
                                        None)
            control_flow_graph.add_edge(then_branch[1], 
                                        merge_vertex, 
                                        None)
            control_flow_graph.add_edge(branch_vertex, 
                                        merge_vertex,
                                        None)
            return [branch_vertex, merge_vertex]
        
        
        def create_if_then_else(self, control_flow_graph):
            branch_vertex = self.__disconnected_vertices.pop()
            merge_vertex  = self.__disconnected_vertices.pop()
            then_branch = self.create_sese_region(control_flow_graph)
            else_branch = self.create_sese_region(control_flow_graph)
            control_flow_graph.add_edge(branch_vertex, 
                                        then_branch[0], 
                                        None)
            control_flow_graph.add_edge(branch_vertex, 
                                        else_branch[0], 
                                        None)
            control_flow_graph.add_edge(then_branch[1], 
                                        merge_vertex, 
                                        None)
            control_flow_graph.add_edge(else_branch[1], 
                                        merge_vertex, 
                                        None)
            return [branch_vertex, merge_vertex]
        
        
        def create_sese_region(self, control_flow_graph):
            entry_vertex = self.__disconnected_vertices.pop()
            exit_vertex = entry_vertex
            if self.__singleton_vertices and bool(random.getrandbits(1)):
                vertex = self.__singleton_vertices.pop()
                control_flow_graph.add_edge(exit_vertex, vertex, None)
                exit_vertex = vertex
            elif self.__free_seses and bool(random.getrandbits(1)):
                nested_sese = self.__free_seses.pop()
                control_flow_graph.add_edge(exit_vertex, nested_sese[0], None)
                exit_vertex = nested_sese[1]
            return [entry_vertex, exit_vertex]
        
        
        def connect_sese_regions(self, control_flow_graph):
            while len(self.__free_seses) > 1:
                sese1 = self.__free_seses.pop()
                sese2 = self.__free_seses.pop()
                source_vertex = sese1[1]
                while self.__singleton_vertices and bool(random.getrandbits(1)):
                    vertex = self.__singleton_vertices.pop()
                    control_flow_graph.add_edge(source_vertex, 
                                                vertex, 
                                                None)
                    source_vertex = vertex
                control_flow_graph.add_edge(source_vertex, 
                                            sese2[0], 
                                            None)
                sese1[1] = sese2[1]
                self.__free_seses.append(sese1)
        
        
        def connect_singleton_vertices(self, control_flow_graph):
            if self.__free_seses:
                last_sese = self.__free_seses.pop()
                while self.__singleton_vertices:
                    vertex = self.__singleton_vertices.pop()
                    if bool(random.getrandbits(1)):
                        control_flow_graph.add_edge(vertex, 
                                                    last_sese[0], 
                                                    None)
                        last_sese[0] = vertex
                    else:
                        control_flow_graph.add_edge(last_sese[1], vertex, None)
                        last_sese[1] = vertex        
                self._header_vertex = last_sese[0]
            elif self.__singleton_vertices:
                pred_vertex = None 
                vertex = None
                while self.__singleton_vertices:
                    vertex = self.__singleton_vertices.pop()
                    if pred_vertex is not None:
                        control_flow_graph.add_edge(pred_vertex, 
                                                    vertex, 
                                                    None)
                    else:
                        self._header_vertex = vertex
                    pred_vertex = vertex
                
            while self.__disconnected_vertices:
                vertex = self.__disconnected_vertices.pop()
                control_flow_graph.add_edge(vertex, 
                                            self._header_vertex, 
                                            None)
                self._header_vertex = vertex
                    
                    
        def add_nested_loop_entry_and_exit_edges(self, 
                                                 control_flow_graph,
                                                 nested_loop_components):
            if nested_loop_components:
                candidate_sources = [vertex for vertex in self.__created_vertices 
                                     if vertex.number_of_successors() == 1]
                random.shuffle(candidate_sources)
                assert len(candidate_sources) >= len(nested_loop_components)
                
                depth_first_search = DepthFirstSearch(control_flow_graph, 
                                                      self._header_vertex, 
                                                      False)
                for loop_component in nested_loop_components:
                    # Connect to inner loop header
                    pred_vertex = candidate_sources.pop()
                    control_flow_graph.add_edge(pred_vertex, loop_component.header_vertex, None)
                    # Connect inner loop exits to vertices whose post-order numbering is 
                    # less than the predecessor. This ensures exit control flow is in a 
                    # forward direction.
                    pred_vertex_index = depth_first_search.post_order.index(pred_vertex)
                    candidate_destinations = depth_first_search.post_order[:pred_vertex_index]
                    for exit_vertex in loop_component.exit_vertices:
                        succ_vertex_index = random.randint(0, len(candidate_destinations)-1)
                        succ_vertex = candidate_destinations[succ_vertex_index]
                        control_flow_graph.add_edge(exit_vertex, succ_vertex, None)
                    
                    
        def add_backedges_and_find_exit_sources(self, control_flow_graph):
            self._tail_vertices = [vertex for vertex in self.__created_vertices
                                   if vertex.number_of_successors() == 0]
            assert len(self._tail_vertices) == 1
            
            for tail_vertex in self._tail_vertices:
                control_flow_graph.add_edge(tail_vertex, 
                                            self._header_vertex, 
                                            None)
    
    
    
    @staticmethod
    def create(name):
        
        def create_artificial_hierarchy(name, number_of_loops, maximum_nesting_depth):
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
            # Add edges to the tree
            parent_vertex = root_vertex
            for vertex in loop_nesting_tree:
                if vertex != root_vertex:
                    new_level = vertex_to_level[parent_vertex] + 1
                    if new_level <= maximum_nesting_depth:
                        loop_nesting_tree.add_edge(parent_vertex, vertex)
                        vertex_to_level[vertex] = new_level
                    else:
                        # The height of the tree now exceeds the maximum depth, so
                        # backtrack to an arbitrary proper ancestor
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
            # Generate each loop body
            loop_components = {}
            for _, vertices in bare_loop_nesting_tree.level_by_level_iterator():
                for tree_vertex in vertices:
                    nested_loop_components = []
                    for succ_edge in tree_vertex.successor_edge_iterator():
                        succ_vertex = bare_loop_nesting_tree.\
                                        get_vertex(succ_edge.vertex_id)
                        nested_loop_components.append(loop_components[succ_vertex])
                    loop_component = ControlFlowGraph.ArtificialLoopBody\
                                        (control_flow_graph, 
                                         number_of_vertices_per_loop[tree_vertex], 
                                         nested_loop_components)
                    if tree_vertex == bare_loop_nesting_tree.root_vertex:
                        pred_edge = loop_component.\
                                    header_vertex.\
                                    get_ith_predecessor_edge(0)
                        control_flow_graph.entry_vertex =\
                            loop_component.header_vertex
                        control_flow_graph.exit_vertex =\
                            control_flow_graph.get_vertex(pred_edge.vertex_id)
                    else:
                        loop_components[tree_vertex] = loop_component
                   
                        
        def pick_and_remove_random_merge_vertices(control_flow_graph):
            disconnected_vertices = []
            
            def connect_predecessors_to_successors(vertex):
                for pred_edge in vertex.predecessor_edge_iterator():
                    pred_vertex = control_flow_graph.get_vertex(pred_edge.
                                                                vertex_id)
                    for succ_edge in vertex.successor_edge_iterator():
                        if not pred_vertex.has_successor(succ_edge.vertex_id):
                            succ_vertex = control_flow_graph.\
                                            get_vertex(succ_edge.vertex_id)
                            
                            control_flow_graph.add_edge(pred_vertex, 
                                                        succ_vertex,
                                                        None)
                            
            
            def disconnect_vertex(vertex):
                pred_edges_to_remove = set()
                for pred_edge in vertex.predecessor_edge_iterator():
                    pred_vertex = control_flow_graph.get_vertex(pred_edge.
                                                                vertex_id)
                    pred_vertex.remove_successor_edge(vertex.vertex_id)
                    pred_edges_to_remove.add(pred_edge)
                for pred_edge in pred_edges_to_remove:
                    vertex.remove_predecessor_edge(pred_edge.vertex_id)
                    
                succ_edges_to_remove = set()
                for succ_edge in vertex.successor_edge_iterator():
                    succ_vertex = control_flow_graph.get_vertex(succ_edge.
                                                                vertex_id)
                    succ_vertex.remove_predecessor_edge(vertex.vertex_id)
                    succ_edges_to_remove.add(succ_edge)
                for succ_edge in succ_edges_to_remove:
                    vertex.remove_successor_edge(succ_edge.vertex_id)
            
            
            for vertex in control_flow_graph:
                if vertex.number_of_predecessors() > 1\
                and bool(random.getrandbits(1)):
                    if not (control_flow_graph.exit_vertex == vertex):
                        connect_predecessors_to_successors(vertex)
                        disconnect_vertex(vertex)
                        disconnected_vertices.append(vertex)
    
            # At this point there is a bunch of vertices disconnected from the 
            # control flow graph.  Reconnect them to vertices which currently only 
            # have one successor or make the disconnected vertex the entry of the
            # control flow graph.
            candidate_sources = []
            for vertex in control_flow_graph:
                if vertex.number_of_successors() == 1 \
                and vertex != control_flow_graph.exit_vertex:
                    assert vertex not in disconnected_vertices
                    candidate_sources.append(vertex)
                    
            random.shuffle(candidate_sources)
            
            for vertex in disconnected_vertices:
                if candidate_sources and bool(random.getrandbits(1)):
                    source_index = random.randint(0, len(candidate_sources)-1)
                    source_vertex = candidate_sources[source_index]
                    for succ_edge in source_vertex.successor_edge_iterator():
                        succ_vertex = control_flow_graph.get_vertex(succ_edge.vertex_id)
                        control_flow_graph.add_edge(vertex, succ_vertex, None)
                    control_flow_graph.add_edge(source_vertex, vertex, None)
                else:
                    control_flow_graph.entry_vertex = vertex
        
        
        control_flow_graph = ControlFlowGraph(name)
        bare_loop_nesting_tree = create_artificial_hierarchy\
                                    (name, 
                                     config.Arguments.loops, 
                                     config.Arguments.nesting_depth)
        create_structure(control_flow_graph,
                         bare_loop_nesting_tree, 
                         config.Arguments.basic_blocks)
        if config.Arguments.unstructured:
            pick_and_remove_random_merge_vertices(control_flow_graph)
        control_flow_graph.check_connected()
        return control_flow_graph
    
    
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
        self._reduced = False
        
        
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
        
    
    @property  
    def basic_block_vertices(self):
        return self._basic_block_vertices
    
    
    @property
    def control_flow_edge_vertices(self):
        return self._control_flow_edge_vertices
    
    
    def add_vertex(self, vertex):
        DirectedGraph.add_vertex(self, vertex)
        if vertex.program_point is not None:
            # No program point means that the vertex is dummy
            self._program_point_to_vertex[vertex.program_point] = vertex
            if ProgramPointVertex.is_basic_block(vertex.program_point):
                self._basic_block_vertices.add(vertex)
            else:
                self._control_flow_edge_vertices.add(vertex)
                
                
    def remove_vertex(self, vertex):
        DirectedGraph.remove_vertex(self, vertex)
        if vertex == self._entry_vertex:
            self._entry_vertex = None
        if vertex == self._exit_vertex:
            self._exit_vertex = None
            
            
    def add_edge(self, pred_vertex, succ_vertex, path_expression):
        edge_id = self.get_new_edge_id()
        pred_vertex.add_successor_edge(TransitionEdge(succ_vertex.vertex_id, 
                                                      edge_id, 
                                                      path_expression))
        succ_vertex.add_predecessor_edge(TransitionEdge(pred_vertex.vertex_id, 
                                                        edge_id,
                                                        path_expression))
     
    
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
    
    
    def super_block_graph_iterator(self):
        for _, abstract_vertices in self.get_loop_nesting_tree().\
                                        level_by_level_iterator\
                                            (abstract_vertices_only=True):
            for abstract_vertex in abstract_vertices:
                if ProgramPointVertex.is_basic_block(abstract_vertex.program_point):
                    yield (abstract_vertex, 
                           self.get_super_block_subgraph(abstract_vertex))
            
            
    def get_super_block_subgraph(self, abstract_vertex):
        if abstract_vertex not in self._super_block_graphs:
            subgraph = SuperBlockGraph.create_for_loop(self, abstract_vertex)
            self._super_block_graphs[abstract_vertex] = subgraph
            dot.make_file(subgraph)
        return self._super_block_graphs[abstract_vertex]
    
    
    def construct_super_blocks_graph(self):
        for _, abstract_vertices in self.get_loop_nesting_tree().\
                                        level_by_level_iterator\
                                            (abstract_vertices_only=True):
            for abstract_vertex in abstract_vertices:
                if ProgramPointVertex.is_basic_block(abstract_vertex.program_point):
                    self.get_super_block_subgraph(abstract_vertex)
    
    
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
                    induced_graph.add_edge(vertex, succ_vertex, None)    
            
        # Add a vertex for each loop-exit program point out of this loop.
        for tree_vertex in loop_exits:
            induced_graph.add_vertex(ProgramPointVertex(tree_vertex.vertex_id,
                                                        tree_vertex.program_point))
        # Add edges between basic block source and loop-exit program point.
        for tree_vertex in loop_exits:
            succ_vertex = induced_graph.get_vertex(tree_vertex.vertex_id)
            vertex = induced_graph.get_vertex_for_program_point(succ_vertex.program_point[0])
            induced_graph.add_edge(vertex, succ_vertex, None)
        
        # Add an abstract vertex per inner loop.
        for tree_vertex in loop_headers:
            induced_graph.add_vertex(ProgramPointVertex(tree_vertex.vertex_id,
                                                        tree_vertex.program_point,
                                                        True))
        # Add edges between loop-entry program points and the inner loop header.
        loop_body_edge_program_points = [vertex for vertex in loop_body 
                                         if not ProgramPointVertex.is_basic_block
                                         (vertex.program_point)]
        for tree_vertex in loop_headers:
            succ_vertex = induced_graph.get_vertex(tree_vertex.vertex_id)
            source_tree_vertices = [vertex for vertex in loop_body_edge_program_points
                                    if vertex.program_point[1] == tree_vertex.program_point]
            for source_tree_vertex in source_tree_vertices:
                vertex = induced_graph.get_vertex(source_tree_vertex.vertex_id)
                induced_graph.add_edge(vertex, succ_vertex, None)
        # Add edges from each inner loop header to its loop-exit program points.
        for succ_vertex in induced_graph:
            if succ_vertex.program_point != header_program_point\
            and succ_vertex.number_of_predecessors() == 0:
                assert not ProgramPointVertex.is_basic_block(succ_vertex.program_point)   
                abstract_vertex = loop_nesting_tree.\
                                    get_header_abstract_vertex_for_program_point\
                                        (succ_vertex.program_point[0]) 
                for tree_vertex in loop_headers:
                    if tree_vertex.program_point == abstract_vertex.program_point:
                        vertex = induced_graph.get_vertex(tree_vertex.vertex_id)
                        induced_graph.add_edge(vertex, succ_vertex, None)
                        
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
                                       dummy_program_point_vertex,
                                       None)
        
        
        induced_graph.set_entry_vertex()
        induced_graph.set_exit_vertex()
        dot.make_file(induced_graph)
        return induced_graph
    
    
    def create_timing_data(self, redo=False):
        for vertex in self:
            if ProgramPointVertex.is_basic_block(vertex.program_point):
                if vertex.wcet is None or redo:
                    vertex.wcet = random.randint(1, 20)
                    
        maximum_loop_bound = random.randint(1, config.Arguments.max_loop_bound)
        def create_loop_bound_tuple_for_header(level, abstract_vertex):
            if level == 0:
                return (1,)
            elif level == 1:
                return (random.randint(1, maximum_loop_bound),)
            else:
                parent_abstract_vertex = loop_nesting_tree.get_vertex\
                                            (abstract_vertex.
                                             get_ith_predecessor_edge(0).
                                             vertex_id)
                parent_header = self.get_vertex_for_program_point\
                                    (parent_abstract_vertex.program_point)
                
                loop_bound = ()                        
                for number_of_iterations in parent_header.loop_bound:
                    for _ in range(1, number_of_iterations+1):
                        loop_bound += (random.randint(1, maximum_loop_bound),)
                return loop_bound
    
        loop_nesting_tree = self.get_loop_nesting_tree()
        for level, tree_vertices in loop_nesting_tree.\
                                        level_by_level_iterator(False, True):
            for abstract_vertex in tree_vertices:
                if ProgramPointVertex.is_basic_block(abstract_vertex.program_point):
                    header = self.get_vertex_for_program_point\
                                (abstract_vertex.program_point)
                    if header.loop_bound is None or redo:
                        header.loop_bound = create_loop_bound_tuple_for_header\
                                                (level, abstract_vertex)         
    
    
    def reduce(self):
        
        """
        Eliminate program points that are not instrumented.  During this reduction.
         and reconnect the
        remaining program points if there is at least one path between them
        in the original control flow graph.
        """
        
        self._reduced = True
        
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
           
            pred_vertex_to_succ_vertex_path_expression =\
                PathExpression.create_sequence_of_path_expressions\
                    (list_of_child_expressions, self._name)
            
            if not pred_vertex.has_successor(vertex_to_succ_edge.vertex_id):
                self.add_edge(pred_vertex, 
                              succ_vertex, 
                              pred_vertex_to_succ_vertex_path_expression)
            else:
                pred_to_succ_edge = pred_vertex.get_successor_edge(vertex_to_succ_edge.
                                                                   vertex_id)
                alternative_expression =\
                    PathExpression.create_alternative_of_two_path_expressions\
                        (pred_to_succ_edge.path_expression,
                         pred_vertex_to_succ_vertex_path_expression,
                         self._name)
                pred_to_succ_edge.path_expression = alternative_expression
        
        
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
                            create_path_expression_for_edge(pred_vertex, 
                                                            succ_vertex,
                                                            pred_edge,
                                                            succ_edge,
                                                            loop_expression)
        
        
        states_to_remove = [vertex for vertex in self if not vertex.instrumented]
        random.shuffle(states_to_remove)
        for vertex in states_to_remove:
            debug.verbose_message('Removing program point {}'.
                                  format(vertex.program_point), 
                                  __name__)
            connect_predecessors_to_successors(vertex)
            self.remove_vertex(vertex)
            
            
        for vertex in self:
            for succ_edge in vertex.successor_edge_iterator():
                succ_vertex = self.get_vertex(succ_edge.vertex_id)
                succ_edge.path_expression.pred_vertex = vertex
                succ_edge.path_expression.succ_vertex = succ_vertex
                dot.make_file(succ_edge.path_expression)
                
        dot.make_file(self)
            
            
    def reduce_but_maintain_path_reconstructibility(self,
                                                    unmonitored_program_points):
        
        self._reduced = True
        
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
                       'Expected the path expression of the edge {}->{} to be '\
                       ' empty'.format(vertex.vertex_id, succ_edge.vertex_id)
                state_transition_to_path_expression[(vertex, succ_vertex)] = []
                    
        
        # Maintain graph connectedness and remove each unneeded state.
        for vertex in states_to_remove:
            connect_predecessors_to_successors(vertex)
            self.remove_vertex(vertex)
        
    
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
                     self._name)
                succ_edge.path_expression = path_expression
                pred_edge.path_expression = path_expression
    
        dot.make_file(self)
        
    
    def check_connected(self):
        visited = set()
        stack = [self._entry_vertex]
        while stack:
            stack_vertex = stack.pop()
            visited.add(stack_vertex)
            for succ_edge in stack_vertex.successor_edge_iterator():
                succ_vertex = self.get_vertex(succ_edge.vertex_id)
                if succ_vertex not in visited:
                    stack.append(succ_vertex)
        for vertex in self:
            if vertex not in visited:
                assert False, 'Vertex {} is unreachable in {}'.\
                                format(vertex.vertex_id, self._name)
    
    
    def dot_filename(self):
        if self._reduced:
            return '{}.{}.cfg.reduced'.format(config.get_filename_prefix(), self.name)
        else:
            return '{}.{}.cfg'.format(config.get_filename_prefix(), self.name)
    
    
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
        return '{}.{}'.format(config.get_filename_prefix(), self._name)
        
        

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
                if vertex_level not in self._level_to_vertices.keys():
                    self._level_to_vertices[vertex_level] = set()
                self._level_to_vertices[vertex_level].add(vertex)
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
        return '{}.{}.{}'.format(config.get_filename_prefix(), 
                                 self.name, 
                                 suffix)
        
        
        
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
    
    
    def get_header_abstract_vertex_for_program_point(self, program_point):
        program_point_vertex = self._program_point_to_vertex[program_point]
        abstract_vertex = self.get_vertex(program_point_vertex.
                                          get_ith_predecessor_edge(0).
                                          vertex_id)
        if ProgramPointVertex.is_basic_block(abstract_vertex.program_point):
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
                        if not ProgramPointVertex.is_basic_block(succ_vertex.
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
        return '{}.{}.lnt'.format(config.get_filename_prefix(), self._name)
        
        
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
                    if not pre_dominator_tree.is_ancestor\
                        (pre_dominator_tree.get_vertex(vertex.vertex_id),
                         pre_dominator_tree.get_vertex(pred_vertex.vertex_id)):
                        raise IrreducibleLoopError('Depth-first backedge' 
                                                   ' ({}, {}) identifies an'
                                                   ' irreducible loop'.
                                                   format(pred_vertex.vertex_id,
                                                          vertex.vertex_id)
                                                   )
                    self.__find_loop_body(control_flow_graph,
                                          depth_first_search_tree,
                                          pred_vertex, 
                                          vertex,
                                          loop_bodies)
        return loop_bodies
        
            
    def __find_loop_body(self, 
                         control_flow_graph, 
                         depth_first_search_tree, 
                         tail, 
                         header,
                         loop_bodies):
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
            if header not in header_loop_bodies:
                header_loop_bodies[header] = set(loop_body)
            header_loop_bodies[header].intersection_update(loop_body)
                        
        # For each vertex in a shared loop body, set the header's abstract
        # vertex as the parent.  Otherwise, set the tail's abstract vertex as 
        # the parent.
        for (tail, header), loop_body in loop_bodies.items():
            for vertex in loop_body:
                if vertex in abstract_vertices\
                and vertex != header\
                and ProgramPointVertex.is_basic_block(vertex.program_point):
                    # An inner loop header.  Add an edge to the inner loop header's
                    # abstract vertex but not to the inner loop header's program 
                    # point vertex.  The latter edge insertion will be handled 
                    # while looping through the inner loop's body.
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
        return '{}.{}.{}_{}.pe'.format(config.get_filename_prefix(), 
                                       self._name,
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
                
                if ProgramPointVertex.is_basic_block(induced_vertex.program_point):
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
                    if not ProgramPointVertex.is_basic_block(induced_vertex.
                                                             program_point):
                        super_vertex.representative = induced_vertex
                        break
                    
        
    def add_edges(self, abstract_vertex, induced_subgraph):
        for super_vertex in self:
            induced_vertex = super_vertex.vertices[0]
            if not ProgramPointVertex.is_basic_block(induced_vertex.program_point):
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
                # but not the loop header of this region
                assert induced_vertex.number_of_predecessors() > 1
                for pred_edge in induced_vertex.predecessor_edge_iterator():
                    pred_induced_vertex = induced_subgraph.\
                                            get_vertex(pred_edge.vertex_id)
                    pred_super_vertex = self._program_point_to_vertex\
                                            [pred_induced_vertex.program_point]
                    self.add_edge(pred_super_vertex, super_vertex)
    
    
    def dot_filename(self):
        return '{}.{}.super'.format(config.get_filename_prefix(), 
                                    self._name)
            
        