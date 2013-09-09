from DirectedGraphs import FlowGraph, DirectedGraph
from Vertices import dummyVertexID
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
                    assert self._exitID == dummyVertexID, "The exit ID has already been set to %d. Found another entry candidate %d" % (self._exitID, bbID)
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
    