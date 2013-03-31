from Edges import Edge, CallGraphEdge, SuperBlockLoopEdge

dummyVertexID = -1

class Vertex ():
    def __init__ (self, vertexID):
        self._vertexID = vertexID
        self._predecessors = {}
        self._successors = {}
        
    def setVertexID (self, vertexID):
        self._vertexID = vertexID
    
    def getVertexID (self):
        return self._vertexID
    
    def addPredecessor (self, predID, edgeID=None):
        e = Edge(predID, edgeID)
        self._predecessors[predID] = e
        
    def addPredecessorEdge (self, prede):
        predID = prede.getVertexID()
        self._predecessors[predID] = prede
            
    def removeAllPredecessors (self):
        self._predecessors = {}
    
    def removePredecessor (self, predID):
        assert predID in self._predecessors, "Cannot remove %d as it is not in predecessor of %d" % (predID, self._vertexID)
        del self._predecessors[predID]
    
    def getPredecessorIDs (self):
        return self._predecessors.keys()
    
    def getPredecessorEdges (self):
        return self._predecessors.values()
    
    def numberOfPredecessors (self):
        return len(self._predecessors)
    
    def hasPredecessor (self, predID):
        return predID in self._predecessors.keys()
    
    def getPredecessorEdge (self, predID):
        assert predID in self._predecessors, "Vertex %d is not a predecessor of %d" % (predID, self._vertexID)
        return self._predecessors[predID]
    
    def addSuccessor (self, succID,edgeID=None):
        e = Edge(succID, edgeID)
        self._successors[succID] = e
        
    def addSuccessorEdge (self, succe):
        succID = succe.getVertexID()
        self._successors[succID] = succe
    
    def removeAllSuccessors (self):
        self._successors = {}
        
    def removeSuccessor (self, succID):
        assert succID in self._successors, "Cannot remove %d as it is not in _successors of %d" % (succID, self._vertexID)
        del self._successors[succID]
        
    def getSuccessorIDs (self):
        return self._successors.keys()
    
    def getSuccessorEdges (self):
        return self._successors.values()
    
    def numberOfSuccessors (self):
        return len(self._successors)
    
    def hasSuccessor (self, succID):
        return succID in self._successors.keys()
    
    def getSuccessorEdge (self, succID):
        assert succID in self._successors, "Vertex %d is not a successor of %d" % (succID, self._vertexID)
        return self._successors[succID]
    
    def __str__ (self):
        string = "Vertex ID = " + str(self._vertexID) + "\n"
        string += "pred     = {%s}\n" % ', '.join(str(predID) for predID in self._predecessors.keys())
        string += "succ     = {%s}\n" % ', '.join(str(succID) for succID in self._successors.keys())
        return string
    
class TreeVertex (Vertex):
    def __init__ (self, vertexID):
        Vertex.__init__(self, vertexID)
        self._parentID = dummyVertexID
        self._level    = -1
        
    def setParentID (self, parentID):
        self._parentID = parentID
        
    def getParentID (self):
        assert self._parentID != dummyVertexID, "Parent ID of %d has not been set" % self._parentID
        return self._parentID
    
    def setLevel (self, level):
        assert level >= 0, "The level of a vertex cannot be less than 0. You gave %d" % level
        self._level = level
    
    def getLevel (self):
        return self._level     
    
    def __str__ (self):
        if self._parentID == dummyVertexID:
            return "parent(%d) = <>\n" % self._vertexID
        else:
            return "parent(%d) = %d\n" % (self._vertexID, self._parentID)
    
class HeaderVertex (TreeVertex):
    def __init__ (self, vertexID, headerID):
        TreeVertex.__init__(self, vertexID)
        self.headerID = headerID
        
    def getHeaderID (self):
        return self.headerID
    
    def __str__ (self):
        return TreeVertex.__str__(self)[:-1] + " (" + "*" * 3 + " HEADER " + "*" * 3 + ")\n" 
    
class Enum(set):
    def __getattr__(self, name):
        if name in self:
            return name
        raise AttributeError

class BasicBlock (Vertex):
    IpointPosition = Enum(['start', 'end'])
    
    def __init__ (self, vertexID, name=None):
        Vertex.__init__(self, vertexID)
        self.__name = name
        self.__dummy = False
        self.__ipointPosition = None
        self.__instructions = []
        self.__addresses = set([])
    
    def getName (self):
        return self.__name
    
    def setIpoint (self, position):
        assert position == BasicBlock.IpointPosition.start or position == BasicBlock.IpointPosition.end, "Unable to ascertain position of Ipoint from '%s'" % position
        self.__ipointPosition = position
        
    def hasIpoint (self):
        return self.__ipointPosition
    
    def ipointPosition (self):
        assert self.__ipointPosition, "You are requesting an Ipoint position from %d but that does not have an Ipoint set" % self._vertexID
        return self.__ipointPosition
       
    def setDummy (self):
        self.__dummy = True
        
    def isDummy (self):
        return self.__dummy
    
    def addInstruction (self, instruction):
        self.__instructions.append(instruction)
        self.__addresses.add(instruction.getAddress())
        
    def getFirstInstruction (self):
        assert self.__instructions, "Basic block %d does not have instructions" % self._vertexID
        return self.__instructions[0]
        
    def getLastInstruction (self):
        assert self.__instructions, "Basic block %d does not have instructions" % self._vertexID
        return self.__instructions[-1]
    
    def hasInstructions (self):
        return len(self.__instructions) != 0
    
    def getInstructions (self):
        return self.__instructions
    
    def hasAddress (self, address):
        return address in self.__addresses
        
    def __str__ (self):
        string =  "Vertex ID = " + str(self._vertexID) + "\n"
        string += "pred      = {%s}\n" % ', '.join(str(predID) for predID in self._predecessors.keys())
        string += "succ      = {%s}\n" % ', '.join(str(succID) for succID in self._successors.keys())
        for instruction in self.__instructions:
            string += instruction.__str__() + '\n'    
        return string
    
class CallGraphVertex (Vertex):
    def __init__ (self, vertexID, name):
        Vertex.__init__(self, vertexID)
        self.__name = name
        self.__callSiteSuccessors = {}
        
    def getName (self):
        return self.__name
    
    def addPredecessor (self, predID, callSiteID):
        if predID not in self._predecessors:
            e = CallGraphEdge(predID)
            self._predecessors[predID] = e
        e = self._predecessors[predID]
        e.addCallSite(callSiteID)
    
    def addSuccessor (self, succID, callSiteID):
        if succID not in self._predecessors:
            e = CallGraphEdge(succID)
            self._successors[succID] = e
        e = self._successors[succID]
        e.addCallSite(callSiteID)
        self.__callSiteSuccessors[callSiteID] = e.getVertexID()
        
    def getSuccessorEgdeWithCallSiteID (self, callSiteID):
        assert callSiteID in self.__callSiteSuccessors, "Unable to find call site %d in function '%s'" % (callSiteID, self.__name)
        return self._successors[self.__callSiteSuccessors[callSiteID]]
        
    def isCallSite (self, vertexID):
        return vertexID in self.__callSiteSuccessors
    
    def __str__ (self):
        return "%s (vertex id=%d)" % (self.__name, self._vertexID)
    
class SuperBlock (Vertex):
    def __init__ (self, vertexID):
        Vertex.__init__(self, vertexID)
        self.__unstructuredMerge = False
        self.__basicBlocks       = set([])
        
    def setUnstructuredMerge (self):
        self.__unstructuredMerge = True
        
    def isUnstructuredMerge (self):
        return self.__unstructuredMerge
    
    def addBasicBlock (self, vertexID):
        self.__basicBlocks.add(vertexID)
        
    def addBasicBlocks (self, basicBlocks):
        self.__basicBlocks.update(basicBlocks)
        
    def containsBasicBlock (self, vertexID):
        return vertexID in self.__basicBlocks
    
    def numberOfBasicBlocks (self):
        return len(self.__basicBlocks)
    
    def getBasicBlockIDs (self):
        return self.__basicBlocks
    
    def getRepresentativeID (self):
        assert self.__basicBlocks, "Trying to return a representative ID for a super block without basic blocks"
        return list(self.__basicBlocks)[0]
    
    def getBranchPartitions (self):
        partitions = {}
        for succe in self._successors.values():
            if not isinstance(succe, SuperBlockLoopEdge):
                branchID = succe.getBasicBlockID()
                if branchID not in partitions:
                    partitions[branchID] = set([])
                partitions[branchID].add(succe)
        return partitions
    
    def getLoopPartition (self):
        partition = set([])
        for succe in self._successors.values():
            if isinstance(succe, SuperBlockLoopEdge):
                partition.add(succe)
        return partition
    
    def __str__ (self):
        string =  "Vertex ID    = %d\n" % self._vertexID
        string += "Basic blocks = {%s}\n" % ', '.join(str(id) for id in self.__basicBlocks)
        string += "pred         = {%s}\n" % ', '.join(str(predID) for predID in self._predecessors.keys())
        string += "succ         = {%s}\n" % ', '.join(str(succID) for succID in self._successors.keys())
        return string
    
class Ipoint (Vertex):
    def __init__ (self, vertexID, IpointID, realID=None):
        Vertex.__init__(self, vertexID)
        self.__IpointID = IpointID
        self.__succIpointIDToVertexID = {}
        self.__isGhost = False
        if realID:
            self.__realID = realID
        else:
            self.__realID = vertexID
    
    def setGhost (self):
        self.__isGhost = True
        
    def isGhost (self):
        return self.__isGhost
        
    def getIpointID (self):
        return self.__IpointID
    
    def getRealID (self):
        return self.__realID
    
    def addIpointSuccessor (self, succIpointID, succID):
        self.__succIpointIDToVertexID[succIpointID] = succID
    
    def getIpointSuccessor (self, succIpointID):
        assert succIpointID in self.__succIpointIDToVertexID, \
        "Unable to find successor of %s with Ipoint ID 0x%04X" % (self._vertexID, succIpointID)
        return self.__succIpointIDToVertexID[succIpointID]
    
    def __str__ (self):
        string = "Vertex ID = " + str(self._vertexID) + "\n"
        string += "\tIpoint ID = " + str(self.__IpointID) + "\n"
        string += "\t" + Vertex.predecessorStr(self)
        string += "\t" + Vertex.successorStr(self)    
        return string
    