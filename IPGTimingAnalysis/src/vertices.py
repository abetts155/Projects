import edges

dummyID = -1

class UndirectedVertex ():
    def __init__ (self, vertexID):
        self.vertexID = vertexID
        self.edges = []
    
    def get_vertexID (self):
        return self.vertexID
    
    def add_edge (self, vertexID, edgeID=None):
        self._edges.append(vertexID)
    
    def degree (self):
        return len(self.edges)
    
    def __str__ (self):
        string = "Vertex ID = " + str(self.vertexID) + "\n"
        string += "pred = {"
        count = 1
        for vertexID in sorted(self.edges):
            string += str(vertexID)
            if count < len(self.edges):
                string += ","
                count = count + 1
        string += "}\n"
        return string

class Vertex ():
    def __init__ (self, vertexID):
        self.vertexID = vertexID
        self.predecessors = {}
        self.successors = {}
    
    def addPredecessor (self, predID, edgeID=None):
        assert predID not in self.predecessors, "Vertex %d already has predecessor %d" % (self.vertexID, predID)
        e = edges.Edge(predID, edgeID)
        self.predecessors[predID] = e
        
    def addPredecessorEdge (self, prede):
        assert prede.vertexID not in self.predecessors, "Vertex %d already has predecessor %d" % (self.vertexID, prede.vertexID)
        self.predecessors[prede.vertexID] = prede
            
    def removePredecessor (self, predID):
        assert predID in self.predecessors, "Cannot remove %d as it is not in predecessor of %d" % (predID, self.vertexID)
        del self.predecessors[predID]
    
    def getPredecessorIDs (self):
        return self.predecessors.keys()
    
    def getPredecessorEdges (self):
        return self.predecessors.values()
    
    def numberOfPredecessors (self):
        return len(self.predecessors)
    
    def hasPredecessor (self, predID):
        return predID in self.predecessors.keys()
    
    def getPredecessorEdge (self, predID):
        assert predID in self.predecessors, "Vertex %d is not a predecessor of %d" % (predID, self.vertexID)
        return self.predecessors[predID]
    
    def addSuccessor (self, succID,edgeID=None):
        assert succID not in self.successors, "Vertex %d already has successor %d" % (self.vertexID, succID)
        e = edges.Edge(succID, edgeID)
        self.successors[succID] = e
        
    def addSuccessorEdge (self, succe):
        assert succe.vertexID not in self.successors, "Vertex %d already has successor %d" % (self.vertexID, succe.vertexID)
        self.successors[succe.vertexID] = succe
        
    def removeSuccessor (self, succID):
        assert succID in self.successors, "Cannot remove %d as it is not in successors of %d" % (succID, self.vertexID)
        del self.successors[succID]
        
    def getSuccessorIDs (self):
        return self.successors.keys()
    
    def getSuccessorEdges (self):
        return self.successors.values()
    
    def numberOfSuccessors (self):
        return len(self.successors)
    
    def hasSuccessor (self, succID):
        return succID in self.successors.keys()
    
    def getSuccessorEdge (self, succID):
        assert succID in self.successors, "Vertex %d is not a successor of %d" % (succID, self.vertexID)
        return self.successors[succID]
    
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
        self.parentID = dummyID
        self.level    = -1
        
    def getParentID (self):
        assert self.parentID != dummyID, "Parent ID of %d has not been set" % self._parentID
        return self.parentID     
    
    def __str__ (self):
        if self.parentID == dummyID:
            return "parent(%d) = <>\n" % self.vertexID
        else:
            return "parent(%d) = %d\n" % (self.vertexID, self.parentID)
    
class HeaderVertex (TreeVertex):
    def __init__ (self, vertexID, headerID):
        TreeVertex.__init__(self, vertexID)
        self.headerID = headerID
    
    def __str__ (self):
        return TreeVertex.__str__(self)[:-1] + " (" + "*" * 3 + " HEADER " + "*" * 3 + ")\n" 
    
class Ipoint (Vertex):
    def __init__ (self, vertexID, ipointID, realID=None):
        Vertex.__init__(self, vertexID)
        self.ipointID = ipointID
        self.isGhost = False
        if realID:
            self.realID = realID
        else:
            self.realID = vertexID
        self.__succIpointIDToVertexID = {}
    
    def addIpointSuccessor (self, succIpointID, succID):
        self.__succIpointIDToVertexID[succIpointID] = succID
    
    def getIpointSuccessor (self, succIpointID):
        assert succIpointID in self.__succIpointIDToVertexID, \
        "Unable to find successor of %s with Ipoint ID 0x%04X" % (self.vertexID, succIpointID)
        return self.__succIpointIDToVertexID[succIpointID]
    
    def __str__ (self):
        string = "Vertex ID = " + str(self.vertexID) + "\n"
        string += "\tIpoint ID = " + str(self.ipointID) + "\n"
        string += "\t" + Vertex.predecessorStr(self)
        string += "\t" + Vertex.successorStr(self)    
        return string
    
class Enum(set):
    def __getattr__(self, name):
        if name in self:
            return name
        raise AttributeError
    
class BasicBlock (Vertex):
    IpointPosition = Enum(['start', 'end'])
    
    def __init__ (self, vertexID):
        Vertex.__init__(self, vertexID)
        self.__dummy = False
        self.__ipointPosition = None
    
    def setIpoint (self, position):
        assert position == BasicBlock.IpointPosition.start or position == BasicBlock.IpointPosition.end, "Unable to ascertain position of Ipoint from '%s'" % position
        self.__ipointPosition = position
        
    def hasIpoint (self):
        return self.__ipointPosition
    
    def ipointPosition (self):
        assert self.__ipointPosition, "You are requesting an Ipoint position from %d but that does not have an Ipoint set" % self.vertexID
        return self.__ipointPosition
       
    def setDummy (self):
        self.__dummy = True
        
    def isDummy (self):
        return self.__dummy
        
    def __str__ (self):
        string = "Vertex ID = " + str(self.vertexID) + "\n"
        string += "\t" + Vertex.predecessorStr(self)
        string += "\t" + Vertex.successorStr(self)
        string += "\t" + 40 * "=" + "\n"      
        return string
    