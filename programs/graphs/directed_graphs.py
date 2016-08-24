import collections

from . import edges
from . import vertices


class DuplicateVertexError(Exception):
    
    """
    Exception to catch when a vertex with the same id is added to a graph.
    """
    
    def __init__(self, message):
        Exception.__init__(self, message)
        
        
        
class NoValidEntryError(Exception):
    
    """
    Exception to catch when we cannot find a unique entry point in a directed 
    graph.
    """
    
    def __init__(self, message):
        Exception.__init__(self, message)
        
                    
                    
class NoValidExitError(Exception):
    
    """
    Exception to catch when we cannot find a unique exit point in a directed 
    graph.
    """
    
    def __init__(self, message):
        Exception.__init__(self, message)
        
        

class DirectedGraph:
    
    """
    Models a graph with directed edges.
    """

    def __init__(self):
        self._vertices = collections.OrderedDict()
        self._name         = None
        self.__edge_id     = 0
        
        
    @property
    def name(self):
        return self._name
    
    
    @name.setter
    def name(self, value):
        self._name = value
        
        
    def get_new_vertex_id(self):
        vertex_id = 1
        while vertex_id in self._vertices.keys():
            vertex_id += 1 
        return vertex_id
    
    
    def get_new_edge_id(self):
        self.__edge_id += 1
        return self.__edge_id
    
        
    def add_vertex(self, vertex):
        if vertex.vertex_id in self._vertices:
            raise DuplicateVertexError('The graph already has vertex %d' % 
                                       vertex.vertex_id)
        self._vertices[vertex.vertex_id] = vertex
        
    
    def get_vertex(self, vertex_id):
        try:
            return self._vertices[vertex_id]
        except ValueError:
            raise ValueError('Vertex %d is not in the graph' % vertex_id)
        
        
    def remove_vertex(self, vertex_id):
        vertex = self.get_vertex(vertex_id)
        for pred_edge in vertex.predecessor_edge_iterator():
            pred_vertex = self.get_vertex(pred_edge.vertex_id)
            pred_vertex.remove_successor(vertex_id)
        for succ_edge in vertex.successor_edge_iterator():
            succ_vertex = self.get_vertex(succ_edge.vertex_id)
            succ_vertex.remove_predecessor(vertex_id)
        del self._vertices[vertex_id]
    
    
    def has_vertex(self, vertex_id):
        return vertex_id in self._vertices
    
    
    def add_edge(self, pred_vertex, succ_vertex):
        edge_id = self.get_new_edge_id()
        pred_vertex.add_successor_edge(edges.Edge(succ_vertex.vertex_id, 
                                                  edge_id))
        succ_vertex.add_predecessor_edge(edges.Edge(pred_vertex.vertex_id, 
                                                    edge_id))
        
    
    def remove_edge(self, pred_id, succ_id):
        pred_vertex = self.get_vertex(pred_id)
        succ_vertex = self.get_vertex(succ_id)
        pred_vertex.remove_successor(succ_id)
        succ_vertex.remove_predecessor(pred_id)
        
    
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
        return '%s(name=%r vertices=%r)' % (self.__class__.__name__,
                                            self._name,
                                            ' '.join(repr(a_vertex) 
                                                     for a_vertex in self))


class ControlFlowGraph(DirectedGraph):
    
    """
    Models a control flow graph in which vertices represent basic blocks and
    edges are potential transfers of control flow. 
    """
    
    def __init__(self, name):
        DirectedGraph.__init__(self)
        self._name         = name
        self._entry_vertex = None
        self._exit_vertex  = None
    
    
    @property
    def entry_vertex(self):
        return self._entry_vertex
    
    
    @entry_vertex.setter
    def entry_id(self, value):
        self._entry_vertex = value
        
    
    @property
    def exit_vertex(self):
        return self._exit_vertex
    
    
    @exit_vertex.setter
    def exit_vertex(self, value):
        self._exit_vertex = value
        
        
    def find_and_set_entry_vertex(self):
        without_predecessors = []
        for a_vertex in self:
            if a_vertex.number_of_predecessors() == 0:
                without_predecessors.append(a_vertex)
                
        if len(without_predecessors) == 0:
            raise NoValidEntryError('All vertices have at least one predecessor')
        elif len(without_predecessors) > 1:
            raise NoValidEntryError('Too many entry candidates found: %r' %\
                                    ','.join(str(v.vertex_id) for v in without_predecessors))
        else:
            self._entry_vertex = without_predecessors[0]
            
    
    def find_and_set_exit_vertex(self):
        without_successors = []
        for a_vertex in self:
            if a_vertex.number_of_successors() == 0:
                without_successors.append(a_vertex)
                
        if len(without_successors) == 0:
            raise NoValidExitError('All vertices have at least one successor')
        elif len(without_successors) > 1:
            raise NoValidExitError('Too many exit candidates found: %r' %\
                                    ','.join(str(v.vertex_id) for v in without_successors))
        else:
            self._exit_vertex = without_successors[0]
        
        
        
        
class StateTransitionGraph(DirectedGraph):
    
    """
    Models transitions between states such that each transition contains a chunk
    of code and each state is a snapshot of computation at that point.
    """
    
    def __init__(self, control_flow_graph):
        DirectedGraph.__init__(self)
        self._name = control_flow_graph.name
        self.add_states_and_transitions(control_flow_graph)
    
    
    def add_states_and_transitions(self, control_flow_graph):
        basic_block_pred_state = {}
        basic_block_succ_state = {}
        # Add two states for each basic block, for before and after execution.
        # The transition between these states triggers when that basic block
        # is executed.
        for basic_block in control_flow_graph:
            pred_state = vertices.Vertex(self.get_new_vertex_id())
            self.add_vertex(pred_state)
            succ_state = vertices.Vertex(self.get_new_vertex_id())
            self.add_vertex(succ_state)
            self.add_edge(pred_state, succ_state, (basic_block.vertex_id,))
            basic_block_pred_state[basic_block] = pred_state
            basic_block_succ_state[basic_block] = succ_state
        # Add transitions for edges in the control flow graph.
        for basic_block in control_flow_graph:
            for succ_edge in basic_block.successor_edge_iterator():
                succ_basic_block = control_flow_graph.\
                    get_vertex(succ_edge.vertex_id)
                pred_state = basic_block_succ_state[basic_block]
                succ_state = basic_block_pred_state[succ_basic_block]
                self.add_edge(pred_state, succ_state, (basic_block.vertex_id, 
                                                       succ_edge.vertex_id))
        
    
    def add_edge(self, pred_vertex, succ_vertex, program_point):
        edge_id = self.get_new_edge_id()
        pred_vertex.add_successor_edge(edges.TransitionEdge(succ_vertex.vertex_id, 
                                                            edge_id, 
                                                            program_point))
        succ_vertex.add_predecessor_edge(edges.TransitionEdge(pred_vertex.vertex_id, 
                                                              edge_id, 
                                                              program_point))
            


class CallGraph(DirectedGraph):
    
    """
    Models the call graph of a program.
    """
    
    def __init__(self):
        DirectedGraph.__init__(self)
        self.function_name_to_vertex = {}
        
        
    def add_vertex(self, vertex):
        DirectedGraph.add_vertex(self, vertex)
        self.function_name_to_vertex[vertex.name] = vertex
        
    
    def get_vertex_with_name(self, function_name):
        try:
            return self.function_name_to_vertex[function_name]
        except ValueError:
            raise ValueError('No vertex found for function %s' % function_name)
        

class ContextGraph(DirectedGraph):
    
    """
    Models different execution contexts in the program.  The canonical example
    is a call graph where each context represents a function with an
    indistinguishable calling context.  But a context is a much broader concept
    that can encapsulate, e.g., a specific loop iteration containing a function
    call   
    """    
    
    def __init__(self):
        DirectedGraph.__init__(self)
        
      
    