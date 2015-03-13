dummyID = 0

class Edge:    
    def __init__(self, vertexID, edgeID=None):        
        self.vertexID = vertexID
        if edgeID is None:
            self.edgeID = dummyID
        else:
            self.edgeID = edgeID
            
class CallGraphEdge(Edge):
    def __init__ (self, vertexID):
        Edge.__init__(self, vertexID)
        self.call_sites = set()
            
class TransitionEdge(Edge):
    def __init__(self, vertexID, the_program_point, represents_collapsed_loop=False):
        Edge.__init__(self, vertexID)
        self.the_program_point         = the_program_point
        self.represents_collapsed_loop = represents_collapsed_loop 
    
    def __str__(self):
        if isinstance(self.the_program_point, set):
            return "%s" % self.the_program_point
        else:
            return "%s" % (self.the_program_point,)
