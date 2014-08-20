import edges

dummyVertexID = -1

class PathInformationVertex ():
    def __init__ (self, vertexID, programPoint, headerID):
        self._vertexID = vertexID
        self._programPoint = programPoint
        self._headerID = headerID
        self._counterForHeaderIDs = set([])
        self._successors = {}
        self._successors[edges.PathInformationEdgeType.CAPACITY_BOUNDS] = []
        self._successors[edges.PathInformationEdgeType.LOOP_BOUNDS]  = []
        self._successors[edges.PathInformationEdgeType.EXCLUSION] = set([])
        self._successors[edges.PathInformationEdgeType.INCLUSION] = set([])
        
    def vertexID (self):
        return self._vertexID
        
    def getProgramPoint (self):
        return self._programPoint

    def getHeaderID (self):
        return self._headerID
        
    def setCounterForHeaders (self, headerIDs):
        assert isinstance(headerIDs, set)
        self._counterForHeaderIDs = headerIDs
        
    def getHeaderIDsForWhichToCount (self):
        return self._counterForHeaderIDs
    
    def isEffectiveHeaderCounter (self):
        return len(self._counterForHeaderIDs) > 0
               
    def addSuccessorEdge (self, succID, edgeType):
        if edgeType == edges.PathInformationEdgeType.LOOP_BOUNDS:
            succe = edges.LoopBoundEdge(succID)
            self._successors[edgeType].append(succe)
        elif edgeType == edges.PathInformationEdgeType.CAPACITY_BOUNDS:
            succe = edges.CapacityBoundEdge(succID)
            self._successors[edgeType].append(succe)
        else:
            succe = edges.PathInformationEdge(succID, edgeType)
            self._successors[edgeType].add(succe)
        
    def removeSuccessorEdge (self, succID, edgeType):
        theEdge = None
        for succe in self._successors[edgeType]:
            if succe.vertexID == succID:
                theEdge = succe
                break
        if theEdge:
            self._successors[edgeType].remove(succe)
        
    def hasSuccessorEdge (self, succID, edgeType):
        for succe in self._successors[edgeType]:
            if succe.vertexID == succID:
                return True
        return False
    
    def hasSuccessoredges (self, edgeType):
        return len(self._successors[edgeType]) > 0
    
    def getSuccessorEdge (self, succID, edgeType):
        for succe in self._successors[edgeType]:
            if succe.vertexID == succID:
                return succe
        assert False
    
    def getSuccessoredges (self, edgeType):
        return self._successors[edgeType]
    
    def numberOfSuccessors (self, edgeType=None):
        if edgeType:
            return len(self._successors[edgeType])
        return len(self._successors)
    
    def removeAllSuccessors (self):
        self._successors[edges.PathInformationEdgeType.EXCLUSION] = set([])
        self._successors[edges.PathInformationEdgeType.INCLUSION] = set([])
    
    def __str__ (self):
        return str(self._programPoint)
    
class Vertex:
    def __init__ (self, vertexID):
        self.vertexID = vertexID
        self._predecessors = {}
        self._successors = {}
    
    def addPredecessor (self, predID, edgeID=None):
        e = edges.Edge(predID, edgeID)
        self._predecessors[predID] = e
        
    def addPredecessorEdge (self, prede):
        predID = prede.vertexID
        self._predecessors[predID] = prede
            
    def removeAllPredecessors (self):
        self._predecessors = {}
    
    def removePredecessor (self, predID):
        if predID in self._predecessors:
            del self._predecessors[predID]
    
    def getPredecessorIDs (self):
        return self._predecessors.keys()
    
    def getPredecessoredges (self):
        return self._predecessors.values()
    
    def numberOfPredecessors (self):
        return len(self._predecessors)
    
    def hasPredecessor (self, predID):
        return predID in self._predecessors.keys()
    
    def getPredecessorEdge (self, predID):
        assert predID in self._predecessors, "Vertex %d is not a predecessor of %d" % (predID, self._vertexID)
        return self._predecessors[predID]
    
    def addSuccessor (self, succID,edgeID=None):
        e = edges.Edge(succID, edgeID)
        self._successors[succID] = e
        
    def addSuccessorEdge (self, succe):
        succID = succe.vertexID
        self._successors[succID] = succe
    
    def removeAllSuccessors (self):
        self._successors = {}
        
    def removeSuccessor (self, succID):
        if succID in self._successors:
            del self._successors[succID]
        
    def getSuccessorIDs (self):
        return self._successors.keys()
    
    def getSuccessoredges (self):
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
    
class CFGVertex (Vertex):
    def __init__ (self, vertexID, name=None):
        Vertex.__init__(self, vertexID)
        self.__name  = name
        self.__dummy = False
           
    def setDummy (self):
        self.__dummy = True
        
    def isDummy (self):
        return self.__dummy
    
    def getName (self):
        return self.__name
    
class CFGEdge (CFGVertex):
    def __init__ (self, vertexID, predID, succID):
        CFGVertex.__init__(self, vertexID)
        self.__edge  = (predID, succID)
        
    def getEdge (self):
        return self.__edge
        
    def __str__ (self):
        string = "Vertex ID = %d (edge = %s)\n" % (self._vertexID, self.__edge)
        string += "pred     = {%s}\n" % ', '.join(str(predID) for predID in self._predecessors.keys())
        string += "succ     = {%s}\n" % ', '.join(str(succID) for succID in self._successors.keys())
        return string 

class BasicBlock (CFGVertex):    
    def __init__ (self, vertexID, name=None):
        CFGVertex.__init__(self, vertexID, name)
        self.__instructions = []
        self.__addresses = set([])
        self.__originalID = self._vertexID
        
    def setOriginalVertexID (self, originalID):
        self.__originalID = originalID
        
    def getOriginalVertexID (self):
        return self.__originalID
    
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
        string =  "Vertex ID   = " + str(self._vertexID) + "\n"
        string += "Original ID = " + str(self.__originalID) + "\n"
        string += "pred        = {%s}\n" % ', '.join(str(predID) for predID in self._predecessors.keys())
        string += "succ        = {%s}\n" % ', '.join(str(succID) for succID in self._successors.keys())
        for instruction in self.__instructions:
            string += instruction.__str__() + '\n'    
        return string
    
class CallGraphVertex (Vertex):
    def __init__ (self, vertexID, name):
        Vertex.__init__(self, vertexID)
        self.__name = name
        
    def getName (self):
        return self.__name
    
    def addPredecessor (self, predID, callSiteID):
        if predID not in self._predecessors:
            e = edges.CallGraphEdge(predID)
            self._predecessors[predID] = e
        e = self._predecessors[predID]
        e.addCallSite(callSiteID)
    
    def addSuccessor (self, succID, callSiteID):
        if succID not in self._successors:
            e = edges.CallGraphEdge(succID)
            self._successors[succID] = e
        e = self._successors[succID]
        e.addCallSite(callSiteID)
        
    def getSuccessorWithCallSite (self, callSiteID):
        for succe in self.getSuccessoredges():
            if callSiteID in succe.getCallSites():
                return succe.vertexID
        assert False, "Unable to find successor of context %d with call site %d" % (self._vertexID, callSiteID)
    
    def __str__ (self):
        return "%s\n%s" % (self.__name, Vertex.__str__(self))
    
class SuperBlock (Vertex):
    def __init__ (self, vertexID):
        Vertex.__init__(self, vertexID)
        self.__unstructuredMerge = False
        self.__basicBlocks       = set([])
        self.__edges             = set([])
        self.__loopHeader        = None
        self.__repID             = None
        self.__dummy             = False
    
    def setLoopHeader (self, headerID):
        self.__loopHeader = headerID
        
    def getLoopHeader (self):
        return self.__loopHeader
    
    def setUnstructuredMerge (self):
        self.__unstructuredMerge = True
        
    def isUnstructuredMerge (self):
        return self.__unstructuredMerge
    
    def addBasicBlock (self, vertexID):
        self.__basicBlocks.add(vertexID)
        
    def addBasicBlocks (self, basicBlocks):
        self.__basicBlocks.update(basicBlocks)
        
    def addEdge (self, edge):
        self.__edges.add(edge)
        
    def containsBasicBlock (self, vertexID):
        return vertexID in self.__basicBlocks
    
    def numberOfBasicBlocks (self):
        return len(self.__basicBlocks)
    
    def numberOfedges(self):
        return len(self.__edges)
    
    def getBasicBlockIDs (self):
        return self.__basicBlocks
    
    def getedges (self):
        return self.__edges
    
    def setRepresentativeID (self, vertexID):
        assert vertexID in self.__basicBlocks
        self.__repID = vertexID
        
    def hasRepresentativeID (self):
        return self.__repID
    
    def getRepresentativeID (self):
        assert self.__repID, "Representative ID of super block %d not set" % self._vertexID
        return self.__repID
    
    def setDummy (self):
        self.__dummy = True
        
    def isDummy (self):
        return self.__dummy
    
    def getBranchPartitions (self):
        partitions = {}
        for succe in self._successors.values():
            branchID = succe.getBasicBlockID()
            if branchID not in partitions:
                partitions[branchID] = set([])
            partitions[branchID].add(succe)
        return partitions
    
    def __str__ (self):
        string =  "Vertex ID    = %d\n" % self._vertexID
        string += "Basic blocks = {%s}\n" % ', '.join(str(id) for id in self.__basicBlocks)
        string += "edges        = {%s}\n" % ', '.join(str(edge) for edge in self.__edges)
        string += "pred         = {%s}\n" % ', '.join(str(predID) for predID in self._predecessors.keys())
        string += "succ         = {%s}\n" % ', '.join(str(succID) for succID in self._successors.keys())
        return string
    
