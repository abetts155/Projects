import directed_graphs
import vertices
import debug

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
        
class CFG (directed_graphs.FlowGraph):    
    def __init__ (self):
        directed_graphs.FlowGraph.__init__(self)
        
    def getReverseCFG (self):
        reverseg = CFG() 
        if isinstance(self, ICFG):
            reverseg = ICFG()
        # Add vertices
        for v in self:
            copyv = None
            if isinstance(v, vertices.vertices.BasicBlock):
                copyv = vertices.vertices.BasicBlock(v.vertexID)
            else:
                assert isinstance(v, vertices.vertices.Ipoint)
                copyv = vertices.vertices.Ipoint(v.vertexID, v.getIpointID())
            reverseg.addVertex(copyv)
    
        # Add edges
        for v in self:
            predID = v.vertexID
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
        assert bb.vertexID not in self.the_vertices, "Adding basic block %d which is already in graph" % bb.vertexID
        self.the_vertices[bb.vertexID] = bb
        
    def getVertex (self, bbID):
        return directed_graphs.DirectedGraph.getVertex(self, bbID)
        
    def setEntryID (self, entryID=None):
        if entryID is None:
            for bb in self.the_vertices.values():
                if bb.numberOfPredecessors() == 0:
                    bbID = bb.vertexID
                    assert self._entryID == vertices.dummyID, "The entry ID has already been set to %s. Found another entry candidate %s" % (self._entryID, bbID)
                    self._entryID = bbID
            assert self._entryID != vertices.dummyID, "Unable to find a vertex without predecessors to set as the entry"
        else:
            assert entryID in self.the_vertices, "Cannot find vertex " + str(entryID) + " in vertices"
            assert entryID > vertices.dummyID, "Entry ID " + str(entryID) + " is not positive"
            self._entryID = entryID
        
    def setExitID (self, exitID=None):
        if exitID is None:
            for bb in self.the_vertices.values():
                if bb.numberOfSuccessors() == 0:
                    assert self._exitID == vertices.dummyID, "The exit ID has already been set to %s. Found another entry candidate %s" % (self._entryID, bb.vertexID)
                    self._exitID = bb.vertexID
            assert self._exitID != vertices.dummyID, "Unable to find a vertex without successors to set as the entry"
        else:
            assert exitID in self.the_vertices, "Cannot find vertex " + str(exitID) + " in vertices"
            assert exitID > vertices.dummyID, "Exit ID " + str(exitID) + " is not positive"
            self._exitID = exitID
            
    def addExitEntryEdge (self):
        assert self._exitID != vertices.dummyID, "Exit ID not set"
        entryv = self.getVertex(self._entryID)
        exitv = self.getVertex(self._exitID)
        entryv.addPredecessor(self._exitID)
        exitv.addSuccessor(self._entryID)
        
    def setEdgeIDs (self):
        edgeID = 1
        for v in self:
            for succID in v.getSuccessorIDs():
                succe = v.getSuccessorEdge(succID)
                succe.set_edgeID(edgeID)
                succv = self.getVertex(succID)
                prede = succv.getPredecessorEdge(v.vertexID)
                prede.set_edgeID(edgeID)
                edgeID += 1
        
    def __str__ (self):
        string = "*" * 20 + " CFG Output " + "*" * 20 + "\n" + \
        "Entry ID = %s\n" % str(self._entryID) + \
        "Exit ID  = %s\n" % str(self._exitID) + "\n"
        for bb in self.the_vertices.values():
            string += bb.__str__()
        return string
    
class ICFG (CFG):
    def getIpoints (self):
        for v in self:
            if isinstance(v, vertices.Ipoint):
                yield v
        
    def numberOfIpoints (self):
        count = 0
        for v in self:
            if isinstance(v, vertices.Ipoint):
                count+=1
        return count
    
    def addBasicBlock (self, bb):
        assert bb.vertexID not in self.the_vertices, "Adding basic block %d which is already in graph" % bb.vertexID
        self.the_vertices[bb.vertexID] = bb

    def addIpoint (self, ipoint):
        assert ipoint.vertexID not in self.the_vertices, "Adding vertices.Ipoint %d which is already in graph" % ipoint.vertexID
        self.the_vertices[ipoint.vertexID] = ipoint
        
    def isIpoint (self, vertexID):
        return isinstance(self.getVertex(vertexID), vertices.Ipoint)
    
    def isBasicBlock (self, vertexID):
        return isinstance(self.getVertex(vertexID), vertices.BasicBlock)
    
    def addIpointEdges (self):
        bbToIpoint = {}
        bbToPosition = {}
        for v in self:
            if isinstance(v, vertices.BasicBlock):
                if v.hasIpoint():
                    vertexID = self.getNextVertexID()
                    ipoint   = vertices.Ipoint(vertexID, vertexID)
                    self.the_vertices[ipoint.vertexID] = ipoint
                    bbToIpoint[v] = ipoint
                    bbToPosition[v] = v.ipointPosition()
                    debug.debug_message("Adding Ipoint %d for basic block %d" % (vertexID, v.vertexID), __name__, 10)
        for bb, ipoint in bbToIpoint.items():
            if bbToPosition[bb] == vertices.BasicBlock.IpointPosition.start:
                for predID in bb.getPredecessorIDs():
                    self.addEdge(predID, ipoint.vertexID)
                    self.removeEdge(predID, bb.vertexID)
                self.addEdge(ipoint.vertexID, bb.vertexID)
            else:
                for succID in bb.getSuccessorIDs():
                    self.addEdge(ipoint.vertexID, succID)
                    self.removeEdge(bb.vertexID, succID)
                self.addEdge(bb.vertexID, ipoint.vertexID)
                
    def isPathReconstructible (self):
        # First create a subgraph without Ipoints
        subgraph = ICFG()
        for v in self:
            if not self.isIpoint(v.vertexID):
                bb = vertices.BasicBlock(v.vertexID)
                subgraph.the_vertices[v.vertexID] = bb
        for v in self:
            if not self.isIpoint(v.vertexID):
                v = self.getVertex(v.vertexID)
                for succID in v.getSuccessorIDs():
                    if not self.isIpoint(succID):
                        subgraph.addEdge(v.vertexID, succID)
        # Now do a depth-first search to try and find DFS back edges
        reconstructible = True
        the_vertices = []
        for v in subgraph:
            if v.numberOfPredecessors() == 0:
                the_vertices.append(v.vertexID)            
        for vertexID in the_vertices:
            visited  = {}
            for v in subgraph:
                visited[v.vertexID] = False
            stack = []
            stack.append(vertexID)
            while stack:
                poppedID          = stack.pop()
                visited[poppedID] = True
                poppedv           = subgraph.getVertex(poppedID)
                for succID in poppedv.getSuccessorIDs():
                    if not visited[succID]:
                        stack.append(succID)
                    else:
                        debug.debug_message("Visiting %d, which has already been visited. ICFG is NOT path reconstructible" % succID, __name__, 1)
                        reconstructible = False
        return reconstructible
            
      