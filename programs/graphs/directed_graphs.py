import collections


class DuplicateVertexError(Exception):
    
    """
    Exception to catch when a vertex with the same id is added to a graph.
    """
    
    def __init__(self, message):
        Exception.__init__(self, message)
        
        
        
class NoValidEntryError(Exception):
    
    """
    Exception to catch when we cannot find a unique entry point in a directed graph
    """
    
    def __init__(self, message):
        Exception.__init__(self, message)
        
                    
                    
class NoValidExitError(Exception):
    
    """
    Exception to catch when we cannot find a unique exit point in a directed graph
    """
    
    def __init__(self, message):
        Exception.__init__(self, message)
        
        

class DirectedGraph:
    
    """
    Models a graph with directed edges.
    """

    def __init__(self):
        self._the_vertices = collections.OrderedDict()
        self._name         = None
        
        
    @property
    def name(self):
        return self._name
    
    
    @name.setter
    def name(self, value):
        self._name = value
        
        
    def get_next_vertex_id(self):
        next_id = 1
        while next_id in self._the_vertices.keys():
            next_id += 1 
        return next_id
    
        
    def add_vertex(self, the_vertex):
        if the_vertex.vertex_id in self._the_vertices:
            raise DuplicateVertexError('The graph already has vertex %d' % 
                                       the_vertex.vertex_id)
        self._the_vertices[the_vertex.vertex_id] = the_vertex
        
    
    def get_vertex(self, vertex_id):
        try:
            return self._the_vertices[vertex_id]
        except ValueError:
            raise ValueError('Vertex %d is not in the graph' % vertex_id)
        
        
    def remove_vertex(self, vertex_id):
        the_vertex = self.get_vertex(vertex_id)
        for pred_id, _ in the_vertex.predecessors_iterator():
            pred_vertex = self.get_vertex(pred_id)
            pred_vertex.remove_successor(vertex_id)
        for succ_id, _ in the_vertex.successors_iterator():
            succ_vertex = self.get_vertex(succ_id)
            succ_vertex.remove_predecessor(vertex_id)
        del self._the_vertices[vertex_id]
    
    
    def has_vertex(self, vertex_id):
        return vertex_id in self._the_vertices
    
    
    def add_edge(self, pred_id, succ_id, edge_id):
        pred_vertex = self.get_vertex(pred_id)
        succ_vertex = self.get_vertex(succ_id)
        pred_vertex.add_successor(succ_id, edge_id)
        succ_vertex.add_predecessor(pred_id, edge_id)
        
    
    def remove_edge(self, pred_id, succ_id):
        pred_vertex = self.get_vertex(pred_id)
        succ_vertex = self.get_vertex(succ_id)
        pred_vertex.remove_successor(succ_id)
        succ_vertex.remove_predecessor(pred_id)
        
    
    def number_of_vertices(self):
        return len(self._the_vertices)
    
    
    def number_of_edges(self):
        total = 0
        for v in self._the_vertices.values():
            total += v.number_of_successors()
        return total
        
    
    def __iter__(self):
        return self._the_vertices.values().__iter__()
    


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
    
    def __init__(self):
        DirectedGraph.__init__(self)



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
        
      
    