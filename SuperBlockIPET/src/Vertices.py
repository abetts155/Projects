from Edges import Edge, CallGraphEdge

dummyVertexID = -1

class Vertex ():
    def __init__ (self, vertexID):
        self._vertexID = vertexID
        self._predecessors = {}
        self._successors = {}
        self._dummy = False
        
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
        if predID in self._predecessors:
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
        if succID in self._successors:
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
           
    def setDummy (self):
        self._dummy = True
        
    def isDummy (self):
        return self._dummy
    
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
    
class CFGVertex (Vertex):
    def __init__ (self, vertexID, name=None):
        Vertex.__init__(self, vertexID)
        self.__name  = name
    
    def getName (self):
        return self.__name
    
class CFGEdge (Vertex):
    def __init__ (self, vertexID, predID, succID):
        Vertex.__init__(self, vertexID)
        self.edge = (predID, succID)    

class BasicBlock (CFGVertex):
    def __init__ (self, vertexID, name=None, label=None):
        CFGVertex.__init__(self, vertexID, name)
        self._label = label
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
            e = CallGraphEdge(predID)
            self._predecessors[predID] = e
        e = self._predecessors[predID]
        e.addCallSite(callSiteID)
    
    def addSuccessor (self, succID, callSiteID):
        if succID not in self._successors:
            e = CallGraphEdge(succID)
            self._successors[succID] = e
        e = self._successors[succID]
        e.addCallSite(callSiteID)
        
    def getSuccessorWithCallSite (self, callSiteID):
        for succe in self.getSuccessorEdges():
            if callSiteID in succe.getCallSites():
                return succe.getVertexID()
        assert False, "Unable to find successor of context %d with call site %d" % (self._vertexID, callSiteID)
    
    def __str__ (self):
        return "%s\n%s" % (self.__name, Vertex.__str__(self))
    
class SuperBlock (Vertex):
    def __init__ (self, vertexID):
        Vertex.__init__(self, vertexID)
        self.repID       = None
        self.basicBlocks = set([])
        self.outOfScope  = set([])
        self.edges       = set([])
        self.loopHeader  = None
        self.unstructuredMerge = False
    
    def getBranchPartitions (self):
        partitions = {}
        for succe in self._successors.values():
            branchID = succe.getBasicBlockID()
            if branchID not in partitions:
                partitions[branchID] = []
            partitions[branchID].append(succe)
        return partitions
    
    def __str__ (self):
        string =  "Vertex ID    = %d\n" % self._vertexID
        string += "Rep ID       = %s\n" % self.repID
        string += "Basic blocks = {%s}\n" % ', '.join(str(bbID) for bbID in self.basicBlocks)
        string += "Inner blocks = {%s}\n" % ', '.join(str(bbID) for bbID in self.outOfScope)
        string += "Edges        = {%s}\n" % ', '.join(str(edge) for edge in self.edges)
        string += "pred         = {%s}\n" % ', '.join(str(predID) for predID in self._predecessors.keys())
        string += "succ         = {%s}\n" % ', '.join(str(succID) for succID in self._successors.keys())
        return string

class ArithmeticOperatorVertex (Vertex):
    def __init__ (self, vertexID, headerID, operator, acyclicRegion):
        Vertex.__init__(self, vertexID)
        self._headerID = headerID
        self._operator = operator
        self._bound = 0
        self._acyclicRegion = acyclicRegion
        self._wcet = 0
    
    def setWCET (self, wcet):
        self._wcet = wcet
        
    def getWCET (self):
        return self._wcet
    
    def getHeaderID (self):
        return self._headerID
    
    def getOperator (self):
        return self._operator

    def setBound (self, bound):
        self._bound = bound

    def getBound (self):
        return self._bound
    
    def isAcyclicRegion (self):
        return self._acyclicRegion
    
class MultiplicationVertex (ArithmeticOperatorVertex):
    def __init__ (self, vertexID, headerID, acyclicRegion):
        ArithmeticOperatorVertex.__init__(self, vertexID, headerID, '*', acyclicRegion)

class AdditionVertex (ArithmeticOperatorVertex):
    def __init__ (self, vertexID, headerID, acyclicRegion):
        ArithmeticOperatorVertex.__init__(self, vertexID, headerID, '+', acyclicRegion)
        
class MaximumVertex (ArithmeticOperatorVertex):
    def __init__ (self, vertexID, headerID, acyclicRegion):
        ArithmeticOperatorVertex.__init__(self, vertexID, headerID, 'max', acyclicRegion)

class UnionVertex (ArithmeticOperatorVertex):
    def __init__ (self, vertexID, headerID, acyclicRegion):
        ArithmeticOperatorVertex.__init__(self, vertexID, headerID, 'union', acyclicRegion)
        