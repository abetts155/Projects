dummyID = -1

class Edge ():    
    def __init__ (self, vertexID, edgeID=None):        
        self.vertexID = vertexID
        if edgeID is None:
            self.edgeID = dummyID
        else:
            self.edgeID = edgeID
            
    def get_vertexID (self):
        return self.vertexID
    
    def set_edgeID (self, edgeID):
        self.edgeID = edgeID
        
    def getEdgeID (self):
        assert self.edgeID != dummyID, "The edge ID has not been set"
        return self.edgeID
    
class IPGEdge (Edge):
    def __init__ (self, vertexID, edgeID=None, dummy_edge=False):
        Edge.__init__(self, vertexID, edgeID)
        self.edge_label     = set()
        self.iteration_edge = False
        self.dummy_edge     = dummy_edge
    