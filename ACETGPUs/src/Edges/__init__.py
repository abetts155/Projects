class Edge ():
    dummyEdgeID = -1
    
    def __init__ (self, vertexID):
        self.vertexID = vertexID
        self.edgeID = Edge.dummyEdgeID
        
    def getVertexID (self):
        return self.vertexID
    
    def setEdgeID (self, edgeID):
        self.edgeID = edgeID
        
    def getEdgeID (self):
        assert self.edgeID != Edge.dummyEdgeID, "The edge ID has not been set"
        return self.edgeID