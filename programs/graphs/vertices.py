"""
This module includes all vertex types that appear in graphs. 
"""

from programs.graphs import edges
import collections


class DuplicateEdgeError(Exception):
    
    """
    Exception to catch when a duplicate predecessor or successor is added to a
    vertex.
    """
    
    def __init__(self, message):
        Exception.__init__(self, message)



class Vertex:
    
    """
    Models a vertex in a graph as a vertex id, a set of predecessors and a set
    of successors.
    """
    
    def __init__(self, vertex_id):
        self._vertex_id = vertex_id
        self._predecessors = collections.OrderedDict()
        self._successors   = collections.OrderedDict()

    
    @property
    def vertex_id(self):
        return self._vertex_id
    
    
    def add_predecessor(self, pred_id, edge_id):
        if pred_id in self._predecessors:
            raise DuplicateEdgeError('Vertex %d already has predecessor %d' % \
                                     (self._vertex_id, pred_id))
        self._predecessors[pred_id] = edges.Edge(pred_id, edge_id)
        
            
    def remove_predecessor(self, pred_id):
        try:
            del self._predecessors[pred_id]
        except KeyError:
            raise KeyError('Vertex %d has no predecessor %d to remove' % \
                           (self._vertex_id, pred_id))
    
            
    def get_predecessor_edge(self, pred_id):
        try:
            return self._predecessors[pred_id]
        except KeyError:
            raise KeyError('Vertex %d has no predecessor %d' % \
                           (self._vertex_id, pred_id))
            
    
    def number_of_predecessors(self):
        return len(self._predecessors)
    
    
    def has_predecessor(self, pred_id):
        return pred_id in self._predecessors.keys()
            
    
    def predecessors_iterator(self):
        for pred_id, pred_e in self._predecessors.items():
            yield pred_id, pred_e
            
    
    def add_successor(self, succ_id, edge_id):
        if succ_id in self._successors:
            raise DuplicateEdgeError('Vertex %r already has successor %r' % \
                                     (self._vertex_id, succ_id))
        self._successors[succ_id] = edges.Edge(succ_id, edge_id)
        
        
    def remove_successor(self, succ_id):
        try:
            del self._successors[succ_id]
        except KeyError:
            raise KeyError('Vertex %d has no successor %d to remove' % \
                           (self._vertex_id, succ_id))
        
        
    def get_successor_edge(self, succ_id):
        try:
            return self._successors[succ_id]
        except KeyError:
            raise KeyError('Vertex %d has no successor %d' % \
                           (self._vertex_id, succ_id))
            
                
    def number_of_successors(self):
        return len(self._successors)
    
    
    def has_successor(self, succ_id):
        return succ_id in self._successors.keys()
    
    
    def successors_iterator(self):
        for succ_id, succ_e in self._successors.items():
            yield succ_id, succ_e
            
    
    def __repr__(self):
        return '%s(%d: pred=%r succ=%r)' \
            % (self.__class__.__name__,
               self._vertex_id,
               ','.join(str(key) for key in self._predecessors.keys()),
               ','.join(str(key) for key in self._successors.keys()))



class TreeVertex(Vertex):
    
    """
    Models a vertex in a tree by extending the vertex class with parent and
    tree level information.
    """
    
    def __init__ (self, vertex_id):
        Vertex.__init__(self, vertex_id)
        self._parent_id     = None
        self._level_in_tree = None
        
    
    @property
    def parent_id(self):
        return self._parent_id
    
    
    @parent_id.setter
    def parent_id(self, value):
        self._parent_id = value
    
    
    @property
    def level_in_tree(self):
        return self._level_in_tree
    
    
    @level_in_tree.setter
    def level_in_tree(self, value):
        self._level_in_tree = value
        
    
    def __repr__(self):
        return '%s(%d: parent=%r level=%r)' \
            % (self.__class__.__name__,
               self._vertex_id,
               self._parent_id,
               self._level_in_tree)
        
        
    
class LoopHeaderVertex(TreeVertex):
    
    def __init__ (self, vertex_id, header_id):
        TreeVertex.__init__(self, vertex_id)
        self._header_id = header_id
        
    
    @property
    def header_id(self):
        return self._header_id
        
        

class ContextVertex(Vertex):
    
    def __init__(self, vertex_id, name):
        Vertex.__init__(self, vertex_id)
        self._name = name
        
    @property
    def name(self):
        return self._name
    
    def add_predecessor(self, pred_id, call_site_id):
        if pred_id not in self._predecessors:
            self._predecessors[pred_id] = edges.CallGraphEdge(pred_id)
        the_edge = self._predecessors[pred_id]
        the_edge.add_call_site(call_site_id)
    
    def add_successor(self, succ_id, call_site_id):
        if succ_id not in self._successors:
            self._successors[succ_id] = edges.CallGraphEdge(succ_id)
        the_edge = self._successors[succ_id]
        the_edge.add_call_site(call_site_id)



class SuperBlock(Vertex):
    def __init__ (self, vertex_id, header_id):
        Vertex.__init__(self, vertex_id)
        self.header_id            = header_id
        self.program_points       = []
        self.representative       = None
        self.successor_partitions = collections.OrderedDict()
        self.exit_edge            = False
        