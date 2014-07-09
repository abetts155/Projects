import edges

dummyID = 0

class Vertex ():
    def __init__ (self, vertexID):
        self.vertexID = vertexID
        self.predecessors = {}
        self.successors   = {}
        self.dummy        = False
    
    def add_predecessor(self, predID, edgeID=None):
        assert predID not in self.predecessors, "Vertex %d already has predecessor %d" % (self.vertexID, predID)
        e = edges.Edge(predID, edgeID)
        self.predecessors[predID] = e
        
    def add_predecessor_edge(self, prede):
        assert prede.vertexID not in self.predecessors, "Vertex %d already has predecessor %d" % (self.vertexID, prede.vertexID)
        self.predecessors[prede.vertexID] = prede
            
    def remove_predecessor(self, predID):
        assert predID in self.predecessors, "Cannot remove %d as it is not in predecessor of %d" % (predID, self.vertexID)
        del self.predecessors[predID]
    
    def number_of_predecessors(self):
        return len(self.predecessors)
    
    def has_predecessor(self, predID):
        return predID in self.predecessors.keys()
    
    def get_predecessor_edge(self, predID):
        assert predID in self.predecessors, "Vertex %d is not a predecessor of %d" % (predID, self.vertexID)
        return self.predecessors[predID]
    
    def add_successor(self, succID,edgeID=None):
        assert succID not in self.successors, "Vertex %d already has successor %d" % (self.vertexID, succID)
        e = edges.Edge(succID, edgeID)
        self.successors[succID] = e
        
    def add_successor_edge(self, succe):
        assert succe.vertexID not in self.successors, "Vertex %d already has successor %d" % (self.vertexID, succe.vertexID)
        self.successors[succe.vertexID] = succe
        
    def remove_successor(self, succID):
        assert succID in self.successors, "Cannot remove %d as it is not in successors of %d" % (succID, self.vertexID)
        del self.successors[succID]
    
    def number_of_successors(self):
        return len(self.successors)
    
    def has_successor(self, succID):
        return succID in self.successors.keys()
    
    def get_successor_edge(self, succID):
        assert succID in self.successors, "Vertex %d is not a successor of %d" % (succID, self.vertexID)
        return self.successors[succID]
    
    def predecessor_string(self):
        string = "pred = {"
        count = 1
        for predID in sorted(self.predecessors.keys()):
            string += str(predID)
            if count < len(self.predecessors):
                string += ","
                count = count + 1
        string += "}"
        return string
    
    def successor_string(self):        
        string = "succ = {"
        count = 1
        for succID in sorted(self.successors.keys()):
            string += str(succID)
            if count < len(self.successors):
                string += ","
                count = count + 1
        string += "}"
        return string
    
    def __str__(self):
        return "%d: %s %s\n" % (self.vertexID, self.successor_string(), self.predecessor_string())
    
class TreeVertex (Vertex):
    def __init__ (self, vertexID):
        Vertex.__init__(self, vertexID)
        self.parentID = dummyID
        self.level    = -1
    
class HeaderVertex (TreeVertex):
    def __init__ (self, vertexID, headerID):
        TreeVertex.__init__(self, vertexID)
        self.headerID = headerID
    
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
        for succe in self.getSuccessorEdges():
            if callSiteID in succe.getCallSites():
                return succe.getVertexID()
        assert False, "Unable to find successor of context %d with call site %d" % (self._vertexID, callSiteID)
    
    def __str__ (self):
        return "%s\n%s" % (self.__name, Vertex.__str__(self))
    
class SuperBlock (Vertex):
    def __init__ (self, vertexID):
        Vertex.__init__(self, vertexID)
        self.repBasicBlock    = None
        self.repEdge          = None
        self.basicBlocks      = set([])
        self.edges            = set([])
        self.outOfScopeBlocks = set([])
        self.loopExitEdges    = set([])
    
    def getBranchPartitions (self):
        partitions = {}
        for succe in self._successors.values():
            if isinstance(succe, edges.BranchControlFlowEdge):
                if succe.branchID not in partitions:
                    partitions[succe.branchID] = []
                partitions[succe.branchID].append(succe)
        return partitions
    
    def __str__ (self):
        string =  "Vertex ID       = %d\n" % self._vertexID
        string += "Rep basic block = %s\n" % self.repBasicBlock
        string += "Basic blocks    = {%s}\n" % ', '.join(str(bbID) for bbID in self.basicBlocks)
        string += "Loop blocks     = {%s}\n" % ', '.join(str(bbID) for bbID in self.outOfScopeBlocks)
        string += "Edges           = {%s}\n" % ', '.join(str(edge) for edge in self.edges)
        string += "Loop-exit edges = {%s}\n" % ', '.join(str(edge) for edge in self.loopExitEdges)
        string += "pred            = {%s}\n" % ', '.join(str(predID) for predID in self._predecessors.keys())
        string += "succ            = {%s}\n" % ', '.join(str(succID) for succID in self._successors.keys())
        return string
        