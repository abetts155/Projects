dummyID = 0

class Edge:    
    def __init__ (self, vertexID, edgeID=None):        
        self.vertexID = vertexID
        if edgeID is None:
            self.edgeID = dummyID
        else:
            self.edgeID = edgeID

class CallGraphEdge(Edge):
    def __init__ (self, vertexID):
        Edge.__init__(self, vertexID)
        self.call_sites = set()
