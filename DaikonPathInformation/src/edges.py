import sys

class Edge ():    
    dummyEdgeID = -1
    
    def __init__ (self, vertexID, edgeID=None):        
        self.vertexID = vertexID
        if edgeID is None:
            self.edgeID = Edge.dummyEdgeID
        else:
            self.edgeID = edgeID
    
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
    
class PathInformationEdgeType:
    INCLUSION       = 0
    EXCLUSION       = 1
    LOOP_BOUNDS     = 2
    CAPACITY_BOUNDS = 3
    
class PathInformationEdge (Edge):    
    def __init__ (self, vertexID, infoType):
        Edge.__init__(self, vertexID)
        self.__infoType = infoType
    
    def getInformationType (self):
        return self.__infoType
    
class LoopBoundEdge (PathInformationEdge):
    def __init__ (self, vertexID):
        PathInformationEdge.__init__(self, vertexID, PathInformationEdgeType.LOOP_BOUNDS)
        self.relative = 0
        self.upper    = 0
    
class CapacityBoundEdge (PathInformationEdge):
    def __init__ (self, vertexID):
        PathInformationEdge.__init__(self, vertexID, PathInformationEdgeType.CAPACITY_BOUNDS)
        self.lower = sys.maxint
        self.upper = 0
    
    
