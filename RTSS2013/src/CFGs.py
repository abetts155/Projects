from DirectedGraphs import FlowGraph, DirectedGraph
from Vertices import Vertex, Ipoint, dummyVertexID
import Debug

# Class to mode instructions inside basic blocks
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
        self.__instructions = []
        self.__addresses = set([])
    
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
        string = "Vertex ID = " + str(self._vertexID) + "\n"
        for instruction in self.__instructions:
            string += instruction.__str__() + '\n'
        string += "\t" + Vertex.predecessorStr(self)
        string += "\t" + Vertex.successorStr(self)
        string += "\t" + 40 * "=" + "\n"      
        return string
        
class CFG (FlowGraph):    
    def __init__ (self):
        FlowGraph.__init__(self)
        self.__addressToVertex = {}
        
    def getReverseCFG (self):
        reverseg = CFG() 
        if isinstance(self, ICFG):
            reverseg = ICFG()
        # Add vertices
        for v in self:
            copyv = None
            if isinstance(v, BasicBlock):
                copyv = BasicBlock(v.getVertexID())
            else:
                assert isinstance(v, Ipoint)
                copyv = Ipoint(v.getVertexID(), v.getIpointID())
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
            for bb in self.vertices.values():
                if bb.numberOfPredecessors() == 0:
                    bbID = bb.getVertexID()
                    assert self._entryID == dummyVertexID, "The entry ID has already been set to %s. Found another entry candidate %s" % (self._entryID, bbID)
                    self._entryID = bbID
            assert self._entryID != dummyVertexID, "Unable to find a vertex without predecessors to set as the entry"
        else:
            assert entryID in self.vertices, "Cannot find vertex " + str(entryID) + " in vertices"
            assert entryID > dummyVertexID, "Entry ID " + str(entryID) + " is not positive"
            self._entryID = entryID
        
    def setExitID (self, exitID=None):
        if exitID is None:
            for bb in self.vertices.values():
                if bb.numberOfSuccessors() == 0:
                    bbID = bb.getVertexID()
                    assert self._exitID == dummyVertexID, "The exit ID has already been set to %s. Found another entry candidate %s" % (self._entryID, bbID)
                    self._exitID = bbID
            if self._exitID == dummyVertexID:
                Debug.warningMessage("Unable to find a vertex without successors to set as the exit in %s" % self._name)
        else:
            assert exitID in self.vertices, "Cannot find vertex " + str(exitID) + " in vertices"
            assert exitID > dummyVertexID, "Exit ID " + str(exitID) + " is not positive"
            self._exitID = exitID
            
    def addExitEntryEdge (self):
        assert self._exitID != dummyVertexID, "Exit ID not set"
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
            string += bb.__str__()
        return string
    
class ICFG (CFG):
    def __init__ (self):
        CFG.__init__(self)
        
    def getIpoints (self):
        for v in self:
            if isinstance(v, Ipoint):
                yield v
        
    def numberOfIpoints (self):
        count = 0
        for v in self:
            if isinstance(v, Ipoint):
                count+=1
        return count
    
    def addBasicBlock (self, bb):
        bbID = bb.getVertexID()
        assert bbID not in self.vertices, \
        "Adding basic block %d which is already in graph" % bbID
        self.vertices[bbID] = bb

    def addIpoint (self, ipoint):
        vertexID = ipoint.getVertexID()
        assert vertexID not in self.vertices, \
        "Adding Ipoint %d which is already in graph" % vertexID
        self.vertices[vertexID] = ipoint
        
    def isIpoint (self, vertexID):
        return isinstance(self.getVertex(vertexID), Ipoint)
    
    def isBasicBlock (self, vertexID):
        return isinstance(self.getVertex(vertexID), BasicBlock)
    
    def addIpointEdges (self):
        bbToIpoint = {}
        bbToPosition = {}
        for v in self:
            if isinstance(v, BasicBlock):
                if v.hasIpoint():
                    vertexID = self.getNextVertexID()
                    ipoint   = Ipoint(vertexID, vertexID)
                    self.vertices[ipoint.getVertexID()] = ipoint
                    bbToIpoint[v] = ipoint
                    bbToPosition[v] = v.ipointPosition()
                    Debug.debugMessage("Adding Ipoint %d for basic block %d" % (vertexID, v.getVertexID()), 10)
        for bb, ipoint in bbToIpoint.items():
            if bbToPosition[bb] == BasicBlock.IpointPosition.start:
                for predID in bb.getPredecessorIDs():
                    self.addEdge(predID, ipoint.getVertexID())
                    self.removeEdge(predID, bb.getVertexID())
                self.addEdge(ipoint.getVertexID(), bb.getVertexID())
            else:
                for succID in bb.getSuccessorIDs():
                    self.addEdge(ipoint.getVertexID(), succID)
                    self.removeEdge(bb.getVertexID(), succID)
                self.addEdge(bb.getVertexID(), ipoint.getVertexID())
    