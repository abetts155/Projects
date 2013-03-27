import Utils

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
    
class CallGraphEdge (Edge):
    def __init__ (self, vertexID):
        Edge.__init__(self, vertexID)
        self.__callSites = set([])
    
    def addCallSite (self, vertexID):
        self.__callSites.add(vertexID)
        
    def getCallSites (self):
        return self.__callSites
    
    def numberOfCallSites (self):
        return len(self.__callSites)
    
class SuperBlockControlFlowEdge (Edge):
    def __init__ (self, vertexID, bbID):
        Edge.__init__(self, vertexID)
        self.__bbID = bbID
        
    def getBasicBlockID (self):
        return self.__bbID
    
class SuperBlockLoopEdge (SuperBlockControlFlowEdge):
    def __init__ (self, vertexID, headerID):
        SuperBlockControlFlowEdge.__init__(self, vertexID, headerID)
        self.__bbID = headerID
    
class IPGEdge (Edge):
    def __init__ (self, vertexID, edgeID=None, dummyEdge=False):
        Edge.__init__(self, vertexID, edgeID)
        self.__edgeLabel = set()
        self.__iterationEdge = False
        self.__dummyEdge = dummyEdge
        
    def addToEdgeLabel (self, vertices):
        self.__edgeLabel.update(set(vertices))
        
    def getEdgeLabelSize (self):
        return len(self.__edgeLabel)
    
    def getEdgeLabel (self):
        return self.__edgeLabel
    
    def setIterationEdge (self):
        self.__iterationEdge = True
    
    def isIterationEdge (self):
        return self.__iterationEdge
    
    def isDummyEdge (self):
        return self.__dummyEdge
    