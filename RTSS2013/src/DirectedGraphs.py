from Vertices import Vertex, dummyVertexID

class DirectedGraph ():        
    def __init__ (self):
        self.vertices = {}
        self.__name = None
    
    def setName (self, name):
        self.__name = name
        
    def getName (self):
        return self.__name
    
    def getVertex (self, vertexID):
        assert vertexID in self.vertices, "Vertex " + str(vertexID) + " is not in the graph"
        return self.vertices[vertexID]
    
    def removeVertex (self, vertexID):
        assert vertexID in self.vertices, "Vertex " + str(vertexID) + " is not in the graph"
        del self.vertices[vertexID]
    
    def hasVertex (self, vertexID):
        return vertexID in self.vertices
    
    def addEdge (self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        predv.addSuccessor(succID)
        succv.addPredecessor(predID)
    
    def removeEdge (self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        predv.removeSuccessor(succID)
        succv.removePredecessor(predID)
        
    def addPredecessorEdges (self):
        for v in self:
            vertexID = v.getVertexID()
            for succID in v.getSuccessorIDs():
                succv = self.getVertex(succID)
                if not succv.hasPredecessor(vertexID):
                    succv.addPredecessor(vertexID)
    
    def getNextVertexID (self):
        nextID = 1
        while nextID in self.vertices.keys():
            nextID = nextID + 1 
        return nextID
    
    def numOfVertices (self):
        return len(self.vertices)
    
    def numOfEdges(self):
        total = 0
        for v in self.vertices.values():
            total += v.numberOfSuccessors()
        return total
    
    def getReverseGraph (self):
        reverseg = DirectedGraph() 
        # Add vertices
        for v in self:
            vertexID = v.getVertexID()
            copyv    = Vertex(vertexID)
            reverseg.vertices[vertexID] = copyv
        # Add edges
        for v in self:
            predID = v.getVertexID()
            for succID in v.getSuccessorIDs():
                reverseg.addEdge(succID, predID)
        return reverseg
    
    def __iter__ (self):
        return self.vertices.values().__iter__()
    
    def __str__ (self):
        string = "*" * 40 + "\n"
        for v in self.vertices.values():
            string += v.__str__()
        return string

class FlowGraph (DirectedGraph):
    def __init__ (self):
        DirectedGraph.__init__(self)
        self._entryID = dummyVertexID
        self._exitID = dummyVertexID
        
    def getEntryID (self):
        assert self._entryID != dummyVertexID, "Entry to flow graph not found"
        return self._entryID
    
    def getExitID (self):
        assert self._exitID != dummyVertexID, "Exit to flow graph not found"
        return self._exitID
    
    def __str__ (self):
        string = "*" * 40 + "\n" + \
        "Entry ID = %s\n" % str(self._entryID) + \
        "Exit ID  = %s\n" % str(self._exitID) + "\n"
        for v in self.vertices.values():
            string += v.__str__()
        string += "*" * 40 + "\n"
        return string
    