"""
This module includes all vertex types that appear in graphs. 
"""

import collections

from . import edges


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
    
    
    def add_predecessor_edge(self, pred_edge):
        if pred_edge.vertex_id in self._predecessors:
            raise DuplicateEdgeError('Vertex %d already has predecessor %d' % \
                                     (self._vertex_id, pred_edge.vertex_id))
        self._predecessors[pred_edge.vertex_id] = pred_edge
        
            
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
            
    
    def predecessor_edge_iterator(self):
        for _, pred_edge in self._predecessors.items():
            yield pred_edge
            
    
    def add_successor_edge(self, succ_edge):
        if succ_edge.vertex_id in self._successors:
            raise DuplicateEdgeError('Vertex %r already has successor %r' % \
                                     (self._vertex_id, succ_edge.vertex_id))
        self._successors[succ_edge.vertex_id] = succ_edge
        
        
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
    
    
    def successor_edge_iterator(self):
        for _, succ_edge in self._successors.items():
            yield succ_edge
            
    
    def __repr__(self):
        return '%s(id=%r pred=%r succ=%r)' \
            % (self.__class__.__name__,
               self._vertex_id,
               ','.join(repr(value) for value in self._predecessors.values()),
               ','.join(repr(value) for value in self._successors.values()))



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
        return '%s(id=%r parent=%r level=%r)' \
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
        
        

class CallVertex(Vertex):
    
    def __init__(self, vertex_id, name):
        Vertex.__init__(self, vertex_id)
        self._name = name
        
    @property
    def name(self):
        return self._name
    
    def add_predecessor(self, pred_id, call_site_id):
        if pred_id not in self._predecessors:
            self._predecessors[pred_id] = edges.CallGraphEdge(pred_id)
        edge = self._predecessors[pred_id]
        edge.add_call_site(call_site_id)
    
    def add_successor(self, succ_id, call_site_id):
        if succ_id not in self._successors:
            self._successors[succ_id] = edges.CallGraphEdge(succ_id)
        edge = self._successors[succ_id]
        edge.add_call_site(call_site_id)



class SuperBlock(Vertex):
    def __init__ (self, vertex_id, header_id):
        Vertex.__init__(self, vertex_id)
        self.header_id            = header_id
        self.program_points       = []
        self.representative       = None
        self.successor_partitions = collections.OrderedDict()
        self.exit_edge            = False
        