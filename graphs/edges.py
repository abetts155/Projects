

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
        return 'Edge(%r: edge id =%r)' % (self._vertex_id, self._edge_id)



class CallGraphEdge(Edge):
    def __init__(self, vertex_id):
        Edge.__init__(self, vertex_id)
        self._call_sites = set()
        
        
    def add_call_site(self, call_site_id):
        self._call_sites.add(call_site_id)
        
        
    def number_of_call_sites(self):
        return len(self._call_sites)
    
    
    def call_site_iterator(self):
        for call_site_id in self._call_sites:
            yield call_site_id



class TransitionEdge(Edge):
    def __init__(self, 
                 vertex_id, 
                 the_program_point, 
                 represents_collapsed_loop=False):
        Edge.__init__(self, vertex_id)
        self._the_program_point         = the_program_point
        self._represents_collapsed_loop = represents_collapsed_loop
    
    def __str__(self):
        if isinstance(self.the_program_point, set):
            return "%s" % self.the_program_point
        else:
            return "%s" % (self.the_program_point,)
