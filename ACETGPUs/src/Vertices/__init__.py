import Edges, DirectedGraph

class Vertex ():
    def __init__ (self, vertexID):
        self.vertexID = vertexID
        self.predecessors = {}
        self.successors = {}
        
    def getVertexID (self):
        return self.vertexID
    
    def addPredecessor (self, predID):
        assert predID not in self.predecessors, "Vertex " + str(self.vertexID) + " already has predecessor " + str(predID)
        e = Edges.Edge(predID)
        self.predecessors[predID] = e
            
    def removePredecessor (self, predID):
        assert predID in self.predecessors, "Cannot remove %s as it is not in predecessor of %s" % (predID, self.vertexID)
        del self.predecessors[predID]
    
    def getPredecessorIDs (self):
        return self.predecessors.keys()
    
    def getPredecessorEdges (self):
        return self.predecessors.values()
    
    def numberOfPredecessors (self):
        return len(self.predecessors)
    
    def addSuccessor (self, succID):
        assert succID not in self.successors, "Vertex " + str(self.vertexID) + " already has successor " + str(succID) 
        e = Edges.Edge(succID)
        self.successors[succID] = e
        
    def removeSuccessor (self, succID):
        assert succID in self.successors, "Cannot remove %s as it is not in successors of %s" % (succID, self.vertexID)
        del self.successors[succID]
        
    def getSuccessorIDs (self):
        return self.successors.keys()
    
    def getSuccessorEdges (self):
        return self.successors.values()
    
    def numberOfSuccessors (self):
        return len(self.successors)
    
    def predecessorStr (self):
        string = "pred = {"
        count = 1
        for predID in sorted(self.predecessors.keys()):
            string += str(predID)
            if count < len(self.predecessors):
                string += ","
                count = count + 1
        string += "}\n"
        return string
    
    def successorStr (self):        
        string = "succ = {"
        count = 1
        for succID in sorted(self.successors.keys()):
            string += str(succID)
            if count < len(self.successors):
                string += ","
                count = count + 1
        string += "}\n"
        return string
    
class TreeVertex (Vertex):
    def __init__ (self, vertexID):
        Vertex.__init__(self, vertexID)
        self.parentID = DirectedGraph.dummyVertexID
        
    def setParentID (self, parentID):
        self.parentID = parentID
        
    def getParentID (self):
        assert self.parentID != DirectedGraph.dummyVertexID, "Parent ID of %s has not been set" % self.parentID
        return self.parentID
    
    def __str__ (self):
        if self.parentID == DirectedGraph.dummyVertexID:
            return "parent(%s) = <>\n" % self.vertexID
        else:
            return "parent(%s) = %s\n" % (self.vertexID, self.parentID)
    
class HeaderVertex (TreeVertex):
    def __init__ (self, vertexID, headerID):
        TreeVertex.__init__(self, vertexID)
        self.headerID = headerID
        
    def getHeaderID (self):
        return self.headerID
    
    def __str__ (self):
        return TreeVertex.__str__(self)[:-1] + " (" + "*" * 3 + " HEADER " + "*" * 3 + ")\n" 
    
class Ipoint (Vertex):
    def __init__ (self, vertexID, IpointID):
        Vertex.__init__(self, vertexID)
        self.IpointID = IpointID
        
    def getIpointID (self):
        return self.IpointID
    
    def __str__ (self):
        string = "Vertex ID = " + str(self.vertexID) + "\n"
        string += "\tIpoint ID = " + str(self.IpointID) + "\n"
        string += "\t" + Vertex.predecessorStr(self)
        string += "\t" + Vertex.successorStr(self)    
        return string