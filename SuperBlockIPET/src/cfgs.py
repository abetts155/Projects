from DirectedGraphs import FlowGraph, DirectedGraph
from Vertices import dummyVertexID, CFGVertex, CFGEdge
import Debug

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

class EnhancedCFG (FlowGraph):
    def __init__ (self, cfg=None):
        FlowGraph.__init__(self)
        self.__edgeToVertex = {}
        if cfg:
            self._name = cfg.getName()
            for v in cfg:
                newVertexID = v.getVertexID()
                newv        = CFGVertex(newVertexID)
                self.vertices[newVertexID] = newv
                newv._dummy = v._dummy
                if newVertexID == cfg.getEntryID():
                    self._entryID = newVertexID
                if newVertexID == cfg.getExitID():
                    self._exitID = newVertexID
            assert self._entryID != dummyVertexID
            assert self._exitID != dummyVertexID
            for v in cfg:
                vertexID = v.getVertexID()
                for succID in v.getSuccessorIDs():
                    newVertexID = self.getNextVertexID()
                    newv        = CFGEdge(newVertexID, vertexID, succID)
                    self.__edgeToVertex[(vertexID, succID)] = newv
                    self.vertices[newVertexID] = newv
                    self.addEdge(vertexID, newVertexID)
                    self.addEdge(newVertexID, succID)
                    
    def getVertexForCFGEdge (self, programPoint):
        assert programPoint in self.__edgeToVertex
        return self.__edgeToVertex[programPoint]
        
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
    
    