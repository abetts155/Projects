from . import vertices
from . import edges

import collections


class DirectedGraph:        
    def __init__ (self):
        self._the_vertices = collections.OrderedDict()
        self._name         = None
        
    @property
    def name(self):
        return self._name
    
    @name.setter
    def name(self, value):
        self._name = value
        
    def add_vertex(self, v):
        assert isinstance(v, vertices.Vertex)
        assert v.get_vertex_id() not in self._the_vertices,\
            "Adding vertex %d which is already in graph" % v.get_vertex_id()
        self._the_vertices[v.get_vertex_id()] = v
        
    def get_next_vertex_id(self):
        next_id = 1
        while next_id in self._the_vertices.keys():
            next_id += 1 
        return next_id
    
    def get_vertex(self, vertex_id):
        assert vertex_id in self._the_vertices,\
            "Vertex %d is not in the graph" % vertex_id
        return self._the_vertices[vertex_id]
    
    def has_vertex(self, vertex_id):
        return vertex_id in self._the_vertices
    
    def add_edge(self, pred_id, succ_id):
        pred_v = self.get_vertex(pred_id)
        succ_v = self.get_vertex(succ_id)
        pred_v.add_successor(succ_id, edges.Edge.dummy_id)
        succ_v.add_predecessor(pred_id, edges.Edge.dummy_id)
        
    def has_edge(self, pred_id, succ_id):
        pred_v = self.get_vertex(pred_id)
        succ_v = self.get_vertex(succ_id)
        return pred_v.has_successor(succ_id) and succ_v.has_predecessor(pred_id)
    
    def remove_edge(self, pred_id, succ_id):
        pred_v = self.get_vertex(pred_id)
        succ_v = self.get_vertex(succ_id)
        pred_v.remove_successor(succ_id)
        succ_v.remove_predecessor(pred_id)
    
    def number_of_vertices(self):
        return len(self._the_vertices)
    
    def number_of_edges(self):
        total = 0
        for v in self._the_vertices.values():
            total += v.number_of_successors()
        return total
        
    def set_edge_ids(self):
        edge_id = 1
        for v in self:
            edge_id = max(edge_id, v.get_vertex_id())
        edge_id += 1
        for v in self:
            for succ_id, succ_e in v.successors_iterator():
                succ_e.edge_id = edge_id
                succ_v = self.get_vertex(succ_id)
                pred_e = succ_v.get_predecessor_edge(v.get_vertex_id())
                pred_e.edge_id = edge_id
                edge_id += 1
    
    def __iter__(self):
        return self._the_vertices.values().__iter__()
    