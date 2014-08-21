import edges
import shlex

dummyID = 0

class Vertex:
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
    
    def add_successor(self, succID, edgeID=None):
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
    
class TreeVertex(Vertex):
    def __init__ (self, vertexID):
        Vertex.__init__(self, vertexID)
        self.parentID = dummyID
        self.level    = -1
    
class HeaderVertex(TreeVertex):
    def __init__ (self, vertexID, headerID):
        TreeVertex.__init__(self, vertexID)
        self.headerID = headerID
    
class CFGVertex(Vertex):
    def __init__(self, vertexID, name=None):
        Vertex.__init__(self, vertexID)
        self.name = name
    
class CFGEdge(Vertex):
    def __init__(self, vertexID, predID, succID, name=None):
        Vertex.__init__(self, vertexID)
        self.edge = (predID, succID)
        self.name = name
    
    def __str__(self):
        return "%s%s\n" % (Vertex.__str__(self), self.edge) 

class BasicBlock(CFGVertex):
    class Instruction:   
        @staticmethod 
        def get_instruction(line):
            lexemes         = shlex.split(line.strip())
            address         = None
            the_instruction = []
            opcode_found    = False
            for index, lex in enumerate(lexemes):
                if index == 0:
                    address = int(lex[:-1], 16)
                else:
                    if lex == ';':
                        # Comment signifies end of parsing
                        break
                    if not opcode_found:
                        if len(lex) != 4:
                            # Some instruction ops can be hexadecimal numbers
                            opcode_found = True
                        else:
                            try:
                                int(lex, 16)
                            except ValueError:
                                opcode_found = True
                    if opcode_found:
                        the_instruction.append(lex)
            return BasicBlock.Instruction(address, the_instruction)
        
        def __init__ (self, address, the_instruction):
            self.address         = address
            self.the_instruction = the_instruction
         
        def __str__(self):
            return "%s : %s" % (hex(self.address), ' '.join(self.the_instruction))
        
    def __init__ (self, vertexID, name=None):
        CFGVertex.__init__(self, vertexID, name)
        self.instructions = []
        self.originalID   = self.vertexID
    
class CallGraphVertex (Vertex):
    def __init__ (self, vertexID, name):
        Vertex.__init__(self, vertexID)
        self.name = name
    
    def add_predecessor(self, predID, call_siteID):
        if predID not in self.predecessors:
            the_edge = edges.CallGraphEdge(predID)
            self.predecessors[predID] = the_edge
        the_edge = self.predecessors[predID]
        the_edge.call_sites.add(call_siteID)
    
    def add_successor(self, succID, call_siteID):
        if succID not in self.successors:
            the_edge = edges.CallGraphEdge(succID)
            self.successors[succID] = the_edge
        the_edge = self.successors[succID]
        the_edge.call_sites.add(call_siteID)
        
    def get_successor_with_call_site(self, call_siteID):
        for succe in self.getSuccessoredges():
            if call_siteID in succe.getCallSites():
                return succe.vertexID
        assert False, "Unable to find successor of context %d with call site %d" % (self._vertexID, call_siteID)
    
    def __str__ (self):
        return "%s\n%s" % (self.name, Vertex.__str__(self))
    
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
    
class PathInformationVertex ():
    def __init__ (self, vertexID, programPoint, headerID):
        self.vertexID = vertexID
        self._programPoint = programPoint
        self._headerID = headerID
        self._counterForHeaderIDs = set([])
        self._successors = {}
        self._successors[edges.PathInformationEdgeType.CAPACITY_BOUNDS] = []
        self._successors[edges.PathInformationEdgeType.LOOP_BOUNDS]  = []
        self._successors[edges.PathInformationEdgeType.EXCLUSION] = set([])
        self._successors[edges.PathInformationEdgeType.INCLUSION] = set([])
        
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