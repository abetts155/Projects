"""
This module includes all edge types that appear in graphs. 
"""


class Edge:    
    def __init__(self, vertex_id, edge_id):
        self._vertex_id = vertex_id
        self._edge_id = edge_id
    
        
    @property
    def vertex_id(self):
        return self._vertex_id
    
    
    @property
    def edge_id(self):
        return self._edge_id
    
    
    @edge_id.setter
    def edge_id(self, value):
        self._edge_id = value
        
    
    def __repr__(self):
        return '%s(id=%r edge_id=%r)' % (self.__class__.__name__, 
                                         self._vertex_id, 
                                         self._edge_id)



class CallGraphEdge(Edge):
    def __init__(self, vertex_id, edge_id):
        Edge.__init__(self, vertex_id, edge_id)
        self._call_sites = set()
        
    
    @property 
    def call_sites(self):
        return self._call_sites


    def __repr__(self):
        return '%s(id=%r edge_id=%r call_sites=%r)' % (self.__class__.__name__,
                                                       self._vertex_id,
                                                       self._edge_id,
                                                       self._call_sites)


class TransitionEdge(Edge):
    def __init__(self, 
                 vertex_id, 
                 edge_id,
                 path_expression):
        Edge.__init__(self, vertex_id, edge_id)
        self._path_expression = path_expression
        self._loopback = False
        
    
    @property
    def path_expression(self):
        return self._path_expression
    
    
    @path_expression.setter
    def path_expression(self, value):
        self._path_expression = value
        
    
    @property
    def loopback(self):
        return self._loopback
    
    
    @loopback.setter
    def loopback(self, value):
        self._loopback = value
    
    
    def __repr__(self):
        return '{}(id={} edge_id={} path_expression={})'.\
            format(self.__class__.__name__,
                   self._vertex_id,
                   self._edge_id,
                   self._path_expression)
    
    
    
