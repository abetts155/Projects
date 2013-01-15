from DirectedGraphs import dummyVertexID, DirectedGraph
from Vertices import Vertex, Ipoint

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
    
class Enum(set):
    def __getattr__(self, name):
        if name in self:
            return name
        raise AttributeError

class BasicBlock (Vertex):
    IpointPosition = Enum(['start', 'end'])
    
    def __init__ (self, vertexID):
        Vertex.__init__(self, vertexID)
        self.instructions = {}
        self.__dummy = False
        self.__ipointPosition = None
    
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
        self._entryID = dummyVertexID
        self._exitID = dummyVertexID
        
    def getReverseCFG (self):
        reverseg = CFG()
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
    
    def getEntryID (self):
        assert self._entryID != dummyVertexID, "Entry ID not set" 
        return self._entryID
        
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
            
    def getExitID (self):
        assert self._exitID != dummyVertexID, "Exit ID not set" 
        return self._exitID
        
    def setExitID (self, exitID=None):
        if exitID is None:
            for bb in self.vertices.values():
                if bb.numberOfSuccessors() == 0:
                    bbID = bb.getVertexID()
                    assert self._exitID == dummyVertexID, "The exit ID has already been set to %s. Found another entry candidate %s" % (self._entryID, bbID)
                    self._exitID = bbID
            assert self._exitID != dummyVertexID, "Unable to find a vertex without successors to set as the entry"
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
        
    def __str__ (self):
        string = "*" * 20 + " CFG Output " + "*" * 20 + "\n" + \
        "Entry ID = %s\n" % str(self._entryID) + \
        "Exit ID  = %s\n" % str(self._exitID) + "\n"
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
    