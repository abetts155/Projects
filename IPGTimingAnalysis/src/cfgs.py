import directed_graphs
import vertices

class CFG (directed_graphs.FlowGraph):    
    def __init__ (self):
        directed_graphs.FlowGraph.__init__(self)
        
    def set_entryID(self, entryID=None):
        if entryID is None:
            for bb in self.the_vertices.values():
                if bb.number_of_predecessors() == 0:
                    bbID = bb.vertexID
                    assert self.entryID == vertices.dummyID, "The entry ID has already been set to %s. Found another entry candidate %s" % (self.entryID, bbID)
                    self.entryID = bbID
            assert self.entryID != vertices.dummyID, "Unable to find a vertex without predecessors to set as the entry"
        else:
            assert entryID in self.the_vertices, "Cannot find vertex " + str(entryID) + " in vertices"
            assert entryID != vertices.dummyID, "Entry ID " + str(entryID) + " is not positive"
            self.entryID = entryID
        
    def set_exitID(self, exitID=None):
        if exitID is None:
            for bb in self.the_vertices.values():
                if bb.numberOfSuccessors() == 0:
                    assert self.exitID == vertices.dummyID, "The exit ID has already been set to %s. Found another entry candidate %s" % (self.entryID, bb.vertexID)
                    self.exitID = bb.vertexID
            assert self.exitID != vertices.dummyID, "Unable to find a vertex without successors to set as the entry"
        else:
            assert exitID in self.the_vertices, "Cannot find vertex " + str(exitID) + " in vertices"
            assert exitID != vertices.dummyID, "Exit ID " + str(exitID) + " is not positive"
            self.exitID = exitID
        
    def __str__ (self):
        string = "*" * 20 + " CFG Output " + "*" * 20 + "\n" + \
        "Entry ID = %s\n" % str(self.entryID) + \
        "Exit ID  = %s\n" % str(self.exitID) + "\n"
        for bb in self.the_vertices.values():
            string += bb.__str__()
        return string
    
class EnhancedCFG (CFG):
    def __init__ (self, cfg=None):
        directed_graphs.FlowGraph.__init__(self)
        self.name = cfg.name
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
