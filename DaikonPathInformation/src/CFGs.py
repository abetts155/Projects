from DirectedGraphs import FlowGraph, DirectedGraph
from Trees import DepthFirstSearch, Dominators
from Vertices import dummyVertexID, Vertex, CFGEdge
from SuperBlocks import DominatorGraph
from Edges import PathInformationEdge, PathInformationEdgeType
import Debug
import copy

class Instruction ():    
    def __init__ (self, address, instruction):
        self.__address     = address
        self.__instruction = instruction
        
    def getAddress (self):
        return self.__address
    
    def getInstructionFields (self):
        return self.__instruction
    
    def getOp (self):
        return self.__instruction[0]
     
    def __str__(self):
        return "%s : %s" % (hex(self.__address), ' '.join(self.__instruction))
    
class PathInformationGraph (DirectedGraph):
    def __init__ (self, cfg):
        DirectedGraph.__init__(self)
        self._name         = cfg.getName()
        self.__enhancedCFG = EnhancedCFG(cfg)
        predomTree         = Dominators(self.__enhancedCFG, self.__enhancedCFG.getEntryID())
        reverseEnhancedCFG = self.__enhancedCFG.getReverseGraph()
        postdomTree        = Dominators(reverseEnhancedCFG, reverseEnhancedCFG.getEntryID())
        self.__dominatorg  = DominatorGraph(self.__enhancedCFG, predomTree, postdomTree)
        monitoredCFGEdges  = self.__dominatorg.pinpointMonitoredCFGEdges()
        reachability       = self.__computeReachability(self.__enhancedCFG, monitoredCFGEdges)
        self.__monitoredEdges = {}
        self.__addVertices(self.__enhancedCFG, monitoredCFGEdges)
        self.__addEdges(reachability)
        self.__neverExecutes = set([])
        
    def getEnhancedCFG (self):
        return self.__enhancedCFG
    
    def getDominatorGraph (self):
        return self.__dominatorg

    def setNeverExecutes (self, vertexID):
        self.__neverExecutes.add(vertexID)
        
    def neverExecutes (self, vertexID):
        return vertexID in self.__neverExecutes
                            
    def isMonitoredEdge (self, predID, succID):
        if (predID, succID) in self.__monitoredEdges:
            return self.__monitoredEdges[(predID, succID)]
    
    def __addVertices (self, enhancedCFG, monitoredCFGEdges):
        for v in enhancedCFG:
            if isinstance(v, CFGEdge) and v.getEdge() in monitoredCFGEdges:
                copyv = copy.deepcopy(v)
                copyv.removeAllSuccessors()
                copyv.removeAllPredecessors()
                self.vertices[copyv.getVertexID()] = copyv
                self.__monitoredEdges[v.getEdge()] = copyv
    
    def __computeReachability (self, enhancedCFG, monitoredCFGEdges):
        # Initialise data flow information
        reachability = {}
        for v in enhancedCFG:
            reachability[v] = set([])
        # Do data-flow analysis
        dfs     = DepthFirstSearch(enhancedCFG, enhancedCFG.getEntryID())
        changed = True
        while changed:
            changed = False
            for vertexID in reversed(dfs.getPostorder()):
                v       = enhancedCFG.getVertex(vertexID)
                oldSize = len(reachability[v])
                for predID in v.getPredecessorIDs():
                    if predID != enhancedCFG.getExitID() and vertexID != enhancedCFG.getEntryID():
                        predv = enhancedCFG.getVertex(predID)
                        reachability[v].update(reachability[predv])
                        if isinstance(predv, CFGEdge): 
                            edge = predv.getEdge() 
                            if edge in monitoredCFGEdges:
                                reachability[v].add(predv)
                if len(reachability[v]) != oldSize:
                    changed = True
        return reachability
    
    def __addEdges (self, reachabilityInformation):
        for v, reachable in reachabilityInformation.iteritems():
            vertexID = v.getVertexID()
            if self.hasVertex(v.getVertexID()):
                for predv in reachable:
                    predID = predv.getVertexID()
                    # Avoid self-loops
                    if predID != vertexID:
                        predv = self.getVertex(predID)
                        succv = self.getVertex(vertexID)
                        prede = PathInformationEdge(predID, PathInformationEdgeType.INCLUSION)
                        succe = PathInformationEdge(vertexID, PathInformationEdgeType.INCLUSION)
                        predv.addSuccessorEdge(succe)
                        succv.addPredecessorEdge(prede)

class EnhancedCFG (FlowGraph):
    def __init__ (self, cfg=None):
        FlowGraph.__init__(self)
        if cfg:
            for v in cfg:
                newVertexID = v.getVertexID()
                newv        = Vertex(newVertexID)
                self.vertices[newVertexID] = newv
                if newVertexID == cfg.getEntryID():
                    self._entryID = newVertexID
            for v in cfg:
                vertexID = v.getVertexID()
                for succID in v.getSuccessorIDs():
                    newVertexID = self.getNextVertexID()
                    newv        = CFGEdge(newVertexID, vertexID, succID)
                    self.vertices[newVertexID] = newv
                    self.addEdge(vertexID, newVertexID)
                    self.addEdge(newVertexID, succID)
                    if vertexID == cfg.getExitID():
                        self._exitID = newVertexID
            assert self._entryID != dummyVertexID
            assert self._exitID != dummyVertexID
        
    def getReverseGraph (self):
        reverseg = EnhancedCFG() 
        # Add vertices
        for v in self:
            copyv = copy.copy(v)
            copyv.removeAllSuccessors()
            copyv.removeAllPredecessors()
            reverseg.vertices[copyv.getVertexID()] = copyv
        # Add edges
        for v in self:
            predID = v.getVertexID()
            predv  = reverseg.getVertex(predID)
            for succID in v.getSuccessorIDs():
                succv = reverseg.getVertex(succID)
                predv.addPredecessor(succID)
                succv.addSuccessor(predID)
        # Set the entry and exit IDs
        reverseg._entryID = self.getExitID()
        reverseg._exitID  = self.getEntryID()
        return reverseg 
                
    def __str__ (self):
        string = "*" * 20 + " Enhanced CFG Output " + "*" * 20 + "\n" + \
        "Entry ID = %s\n" % str(self._entryID) + \
        "Exit ID  = %s\n" % str(self._exitID) + "\n"
        for v in self.vertices.values():
            string += v.__str__() + "\n"
        return string
        
class CFG (FlowGraph):    
    def __init__ (self):
        FlowGraph.__init__(self)
        self.__addressToVertex = {}
        self.__callSites = {}
        
    def addCallSite (self, vertexID, calleeName):
        assert vertexID in self.vertices, "Vertex %d does not belong to the CFG of '%s'" % (vertexID, self._name)
        self.__callSites[vertexID] = calleeName
    
    def isCallSite (self, vertexID):
        return vertexID in self.__callSites
    
    def dumpCallSites (self):
        print self.__callSites
    
    def removeCallSite (self, vertexID):
        assert vertexID in self.__callSites, "Vertex %d is not a call site of '%s'" % (vertexID, self._name)
        del self.__callSites[vertexID]
        
    def getCalleeName (self, vertexID):
        assert vertexID in self.__callSites, "Vertex %d is not a call site of '%s'" % (vertexID, self._name)
        return self.__callSites[vertexID]
    
    def getReverseCFG (self):
        reverseg = CFG()
        # Add vertices
        for v in self:
            copyv = copy.copy(v)
            copyv.removeAllSuccessors()
            copyv.removeAllPredecessors()
            reverseg.addVertex(copyv)
        # Add edges
        for v in self:
            predID = v.getVertexID()
            predv  = reverseg.getVertex(predID)
            for succID in v.getSuccessorIDs():
                succv = reverseg.getVertex(succID)
                predv.addPredecessor(succID)
                succv.addSuccessor(predID)
        # Set the entry and exit IDs
        reverseg.setEntryID(self.getExitID())
        reverseg.setExitID(self.getEntryID())
        return reverseg
        
    def addVertex (self, bb):
        bbID = bb.getVertexID()
        assert bbID not in self.vertices, \
        "Adding basic block %d which is already in graph" % bbID
        self.vertices[bbID] = bb
        
    def getVertex (self, bbID):
        return DirectedGraph.getVertex(self, bbID)
    
    def getVertexWithAddress (self, address):
        if address in self.__addressToVertex:
            return self.__addressToVertex[address]
        for v in self:
            if v.hasAddress(address):
                self.__addressToVertex[address] = v
                return v
        assert False, "Unable to find basic block with address %s" % hex(address) 
        
    def setEntryID (self, entryID=None):
        if entryID is None:
            candidates = []
            toRemove   = []
            for bb in self.vertices.values():
                if bb.numberOfPredecessors() == 0:
                    candidates.append(bb)
            for bb in candidates:
                bbID  = bb.getVertexID()
                if self._entryID != dummyVertexID:
                    Debug.warningMessage("The entry ID has already been set to %d. Found another entry candidate %d" % (self._entryID, bbID))
                    currentEntryv = self.getVertex(self._entryID)
                    entryAddress  = currentEntryv.getFirstInstruction().getAddress()
                    firstAddress  = bb.getFirstInstruction().getAddress()
                    if firstAddress < entryAddress:
                        self._entryID = bbID
                        Debug.warningMessage("Resetting entry vertex to %d" % bbID)
                        toRemove.append(currentEntryv)
                    else:
                        toRemove.append(bb)
                else:
                    self._entryID = bbID
            for bb in toRemove:
                bbID = bb.getVertexID()
                for predID in bb.getPredecessorIDs():
                    predv = self.getVertex(predID)
                    predv.removeSuccessor(bbID)
                for succID in bb.getSuccessorIDs():
                    succv = self.getVertex(succID)
                    succv.removePredecessor(bbID)
                self.removeVertex(bbID)
            assert self._entryID != dummyVertexID, "Unable to find a vertex without predecessors to set as the exit in '%s'" % self._name
        else:
            assert entryID in self.vertices, "Cannot find vertex " + str(entryID) + " in vertices"
            assert entryID > dummyVertexID, "Entry ID " + str(entryID) + " is not positive"
            self._entryID = entryID
        
    def setExitID (self, exitID=None):
        if exitID is None:
            for bb in self.vertices.values():
                if bb.numberOfSuccessors() == 0:
                    bbID = bb.getVertexID()
                    assert self._exitID == dummyVertexID, "The exit ID has already been set to %d. Found another entry candidate %d" % (self._entryID, bbID)
                    self._exitID = bbID
            if self._exitID == dummyVertexID:
                Debug.warningMessage("Unable to find a vertex without successors to set as the exit in '%s'" % self._name)
        else:
            assert exitID in self.vertices, "Cannot find vertex " + str(exitID) + " in vertices"
            assert exitID > dummyVertexID, "Exit ID " + str(exitID) + " is not positive"
            self._exitID = exitID
            
    def addExitEntryEdge (self):
        if self._exitID != dummyVertexID:
            entryv = self.getVertex(self._entryID)
            exitv = self.getVertex(self._exitID)
            entryv.addPredecessor(self._exitID)
            exitv.addSuccessor(self._entryID)
            
    def getFirstInstruction (self):
        v = self.getVertex(self._entryID)
        return v.getFirstInstruction()
    
    def getLastInstruction (self):
        v = self.getVertex(self._exitID)
        return v.getLastInstruction()
        
    def setEdgeIDs (self):
        edgeID = 1
        for v in self:
            for succID in v.getSuccessorIDs():
                succe = v.getSuccessorEdge(succID)
                succe.setEdgeID(edgeID)
                succv = self.getVertex(succID)
                prede = succv.getPredecessorEdge(v.getVertexID())
                prede.setEdgeID(edgeID)
                edgeID += 1
        
    def __str__ (self):
        string = "*" * 20 + " CFG Output " + "*" * 20 + "\n" + \
        "Entry ID = %s\n" % str(self._entryID) + \
        "Exit ID  = %s\n" % str(self._exitID) + "\n"
        for bb in self.vertices.values():
            string += bb.__str__() + "\n"
        return string
    