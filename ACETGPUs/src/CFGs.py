from DirectedGraphs import dummyVertexID, DirectedGraph
from Vertices import Vertex
from hgext.graphlog import revset

# Class to mode instructions inside basic blocks
class Instruction ():    
    def __init__ (self, address, instrString):
        self.address     = address
        self.instrString = instrString
        
    def getAddress (self):
        return self.address
    
    def getString (self):
        return self.instrString
    
    def containsLabel (self, label):
        import shlex
        lexemes = shlex.split(self.instrString)
        if lexemes[0].startswith('l0x'):
            assert lexemes[0].endswith(':'), "Instruction '%s' does not contain a valid label" % self
            return label == lexemes[0][:-1]
        return False
     
    def __str__(self):
        return self.address + " : " + self.instrString

class BasicBlock (Vertex):
    def __init__ (self, vertexID):
        Vertex.__init__(self, vertexID)
        self.instructions = {}
    
    def addInstruction (self, instr):
        address = instr.getAddress()
        assert address not in self.instructions, "Basic block " + str(self._vertexID) + " already has instruction with address " + str(address)
        self.instructions[address] = instr
        
    def getFirstInstruction (self):
        return sorted(self.instructions.iteritems())[0]
    
    def getLastInstruction (self):
        return sorted(self.instructions.iteritems())[len(self.instructions) - 1]
    
    def numberOfInstructions (self):
        return len(self.instructions)
    
    def getInstructions (self):
        return sorted(self.instructions.iteritems())
        
    def __str__ (self):
        string = "Vertex ID = " + str(self._vertexID) + "\n"
        string += "\t" + Vertex.predecessorStr(self)
        string += "\t" + Vertex.successorStr(self)
        string += "\t" + 40 * "=" + "\n"   
        for address, instr in sorted(self.instructions.iteritems()):
            string += "\t" + instr.__str__() + "\n"   
        string += "\t" + 40 * "=" + "\n"      
        return string
        
class CFG (DirectedGraph):    
    def __init__ (self):
        DirectedGraph.__init__(self)
        self.__entryID = dummyVertexID
        self.__exitID = dummyVertexID
        
    def getReverseCFG (self):
        reverseg = CFG()
        
        # Add vertices
        for v in self:
            copyv = BasicBlock(v.getVertexID())
            copyv.instructions = v.instructions
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
        reverseg.setExitID(self.getExitID())
        return reverseg
        
    def addVertex (self, bb):
        bbID = bb.getVertexID()
        assert bbID not in self.vertices, \
        "Adding basic block %d which is already in graph" % bbID
        self.vertices[bbID] = bb
        
    def getVertex (self, bbID):
        return DirectedGraph.getVertex(self, bbID)
    
    def getEntryID (self):
        assert self.__entryID != dummyVertexID, "Entry ID not set" 
        return self.__entryID
        
    def setEntryID (self, entryID=None):
        if entryID is None:
            for bb in self.vertices.values():
                if bb.numberOfPredecessors() == 0:
                    bbID = bb.getVertexID()
                    assert self.__entryID == dummyVertexID, "The entry ID has already been set to %s. Found another entry candidate %s" % (self.__entryID, bbID)
                    self.__entryID = bbID
            assert self.__entryID != dummyVertexID, "Unable to find a vertex without predecessors to set as the entry"
        else:
            assert entryID in self.vertices, "Cannot find vertex " + str(entryID) + " in vertices"
            assert entryID > dummyVertexID, "Entry ID " + str(entryID) + " is not positive"
            self.__entryID = entryID
            
    def getExitID (self):
        assert self.__exitID != dummyVertexID, "Exit ID not set" 
        return self.__exitID
        
    def setExitID (self, exitID=None):
        if exitID is None:
            for bb in self.vertices.values():
                if bb.numberOfSuccessors() == 0:
                    bbID = bb.getVertexID()
                    assert self.__exitID == dummyVertexID, "The exit ID has already been set to %s. Found another entry candidate %s" % (self.__entryID, bbID)
                    self.__exitID = bbID
            assert self.__exitID != dummyVertexID, "Unable to find a vertex without successors to set as the entry"
        else:
            assert exitID in self.vertices, "Cannot find vertex " + str(exitID) + " in vertices"
            assert exitID > dummyVertexID, "Exit ID " + str(exitID) + " is not positive"
            self.__exitID = exitID
            
    def addExitEntryEdge (self):

        assert self.__exitID != dummyVertexID, "Exit ID not set"
        entryv = self.getVertex(self.__entryID)
        exitv = self.getVertex(self.__exitID)
        entryv.addPredecessor(self.__exitID)
        exitv.addSuccessor(self.__entryID)
        
    def __str__ (self):
        string = "*" * 20 + " CFG Output " + "*" * 20 + "\n" + \
        "Entry ID = %s\n" % str(self.__entryID) + \
        "Exit ID  = %s\n" % str(self.__exitID) + "\n"
        for bb in self.vertices.values():
            string += bb.__str__()
        return string
    
class Program():
    def __init__(self):
        self.__cfgs = []
        self.__icfgs = []
        self.__ipgs = []
        self.__lnts = []
        
    def addCFG (self, cfg):
        self.__cfgs.append(cfg)
       
    def addICFG (self, icfg):
        self.__icfgs.append(icfg)
        
    def addIPG (self, ipg):
        self.__ipgs.append(ipg)
        
    def addLNT (self, lnt):
        self.__lnts.append(lnt)
     
    def getCFGs (self):
        return self.__cfgs.__iter__()   

    def getICFGs (self):
        return self.__icfgs.__iter__() 

    def getIPGs (self):
        return self.__ipgs.__iter__() 
    
    def getLNTs (self):
        return self.__lnts.__iter__() 
    