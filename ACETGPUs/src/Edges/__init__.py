class Edge ():
    dummyEdgeID = -1
    
    def __init__ (self, vertexID, edgeID=None):
        self._vertexID = vertexID
        if edgeID is None:
            self.__edgeID = Edge.dummyEdgeID
        else:
            self.__edgeID = edgeID
            
    def getVertexID (self):
        return self._vertexID
    
    def setEdgeID (self, edgeID):
        self.__edgeID = edgeID
        
    def getEdgeID (self):
        assert self.__edgeID != Edge.dummyEdgeID, "The edge ID has not been set"
        return self.__edgeID
    
class IPGEdge (Edge):
    def __init__ (self, vertexID, edgeID=None):
        Edge.__init__(self, vertexID, edgeID)
        self.__edgeLabel = set([])
        
    def addToEdgeLabel (self, vertices):
        self.__edgeLabel.update(set(vertices))
        
    def getEdgeLabelSize (self):
        return len(self.__edgeLabel)
        
        
    