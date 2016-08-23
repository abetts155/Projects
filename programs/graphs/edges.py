

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
        return 'Edge(%r: edge id=%r)' % (self._vertex_id, self._edge_id)



class CallGraphEdge(Edge):
    def __init__(self, vertex_id, edge_id):
        Edge.__init__(self, vertex_id, edge_id)
        self._call_sites = set()
        
    
    @property 
    def call_sites(self):
        return self._call_sites



class TransitionEdge(Edge):
    def __init__(self, 
                 vertex_id, 
                 edge_id,
                 the_program_point, 
                 is_collapsed_loop=False):
        Edge.__init__(self, vertex_id, edge_id)
        self._the_program_point = the_program_point
        self._is_collapsed_loop = is_collapsed_loop
