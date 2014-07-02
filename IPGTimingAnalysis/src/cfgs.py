import directed_graphs
import vertices
import debug
        
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
            for succID in v.successors.keys():
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
                if bb.number_of_predecessors() == 0:
                    bbID = bb.vertexID
                    assert self.entryID == vertices.dummyID, "The entry ID has already been set to %s. Found another entry candidate %s" % (self.entryID, bbID)
                    self.entryID = bbID
            assert self.entryID != vertices.dummyID, "Unable to find a vertex without predecessors to set as the entry"
        else:
            assert entryID in self.the_vertices, "Cannot find vertex " + str(entryID) + " in vertices"
            assert entryID > vertices.dummyID, "Entry ID " + str(entryID) + " is not positive"
            self.entryID = entryID
        
    def setExitID (self, exitID=None):
        if exitID is None:
            for bb in self.the_vertices.values():
                if bb.numberOfSuccessors() == 0:
                    assert self.exitID == vertices.dummyID, "The exit ID has already been set to %s. Found another entry candidate %s" % (self.entryID, bb.vertexID)
                    self.exitID = bb.vertexID
            assert self.exitID != vertices.dummyID, "Unable to find a vertex without successors to set as the entry"
        else:
            assert exitID in self.the_vertices, "Cannot find vertex " + str(exitID) + " in vertices"
            assert exitID > vertices.dummyID, "Exit ID " + str(exitID) + " is not positive"
            self.exitID = exitID
            
    def addExitEntryEdge (self):
        assert self.exitID != vertices.dummyID, "Exit ID not set"
        entryv = self.getVertex(self.entryID)
        exitv = self.getVertex(self.exitID)
        entryv.addPredecessor(self.exitID)
        exitv.addSuccessor(self.entryID)
        
    def __str__ (self):
        string = "*" * 20 + " CFG Output " + "*" * 20 + "\n" + \
        "Entry ID = %s\n" % str(self.entryID) + \
        "Exit ID  = %s\n" % str(self.exitID) + "\n"
        for bb in self.the_vertices.values():
            string += bb.__str__()
        return string
    
class EnhancedCFG (CFG):
    def __init__ (self, cfg):
        directed_graphs.FlowGraph.__init__(self)
        self.name = cfg.name
        self.CFG_edge_to_vertex = {}
        for v in cfg:
            newv = vertices.CFGVertex(v.vertexID, v.is_ipoint)
            self.the_vertices[v.vertexID] = newv
            if v.vertexID == cfg.get_entryID():
                self.entryID = v.vertexID
            if v.vertexID == cfg.get_exitID():
                self.exitID = v.vertexID
        assert self.entryID != vertices.dummyID
        assert self.exitID != vertices.dummyID
        newVertexID = 0
        for v in cfg:
            for succID in v.successors.keys():
                newVertexID -= 1
                newv        = vertices.CFGEdge(newVertexID, v.vertexID, succID)
                self.CFG_edge_to_vertex[(v.vertexID, succID)] = newv
                self.the_vertices[newVertexID] = newv
                self.addEdge(v.vertexID, newVertexID)
                self.addEdge(newVertexID, succID)
    
class ICFG(CFG):
    def __init__(self):
        CFG.__init__(self)
        self.ipoint_positions = {}
    
    def add_edges_between_ipoints(self):
        bb_to_ipoint = {}
        for v in self:
            if v.vertexID in self.ipoint_positions:
                new_vertexID = self.getNextVertexID()
                newv         = vertices.CFGVertex(new_vertexID, True)
                self.the_vertices[new_vertexID] = newv
                bb_to_ipoint[v]                 = newv
        for bb, ipoint in bb_to_ipoint.items():
            if self.ipoint_positions[bb.vertexID] == vertices.ipoint_at_start:
                for predID in bb.predecessors.keys():
                    self.addEdge(predID, ipoint.vertexID)
                    self.removeEdge(predID, bb.vertexID)
                self.addEdge(ipoint.vertexID, bb.vertexID)
            else:
                for succID in bb.successors.keys():
                    self.addEdge(ipoint.vertexID, succID)
                    self.removeEdge(bb.vertexID, succID)
                self.addEdge(bb.vertexID, ipoint.vertexID)
