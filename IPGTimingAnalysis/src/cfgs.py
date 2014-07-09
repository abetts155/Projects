import directed_graphs
import trees
import vertices
import debug

class EnhancedICFG (directed_graphs.FlowGraph):
    def __init__ (self, cfg):
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
    
class ICFG(directed_graphs.FlowGraph):
    def __init__(self):
        directed_graphs.FlowGraph.__init__(self)
        self.ipoint_positions = {}
        
    def set_entry_and_exit(self):
        without_predecessors = []
        without_successors   = []
        for v in self:
            if v.number_of_successors() == 0:
                without_successors.append(v.vertexID)
            if v.number_of_predecessors() == 0:
                without_predecessors.append(v.vertexID)
                
        entryID = None
        if len(without_predecessors) == 0:
            debug.exit_message("CFG '%s' does not have an entry point" % self.name)
        elif len(without_predecessors) > 1:
            debug_info = ""
            for bbID in without_predecessors:
                bb = self.getVertex(bbID)
                debug_info += bb.__str__()
            debug.exit_message("CFG '%s' has too many entry points: %s" % (self.name, debug_info))
        else:
            entryID = self.getNextVertexID()
            entryv  = vertices.CFGVertex(entryID, True)
            self.addVertex(entryv)
            self.set_entryID(entryID)
            self.addEdge(entryID, without_predecessors[0])
        
        exitID = None
        if len(without_successors) == 0:
            debug.exit_message("CFG '%s' does not have an exit point" % self.name)
        elif len(without_successors) > 1:
            debug_info = ""
            for bbID in without_successors:
                bb = self.getVertex(bbID)
                debug_info += bb.__str__()
            debug.exit_message("CFG '%s' has too many exit points: %s" % (self.name, debug_info))
        else:
            exitID = self.getNextVertexID()
            exitv  = vertices.CFGVertex(exitID, True)
            self.addVertex(exitv)
            self.set_exitID(exitID)
            self.addEdge(without_successors[0], exitID)
        assert entryID, "Unable to set entry ID"
        assert exitID, "Unable to set exit ID"
        self.addEdge(exitID, entryID)
        
    def set_entryID(self, entryID):
        assert entryID in self.the_vertices, "Cannot find vertex " + str(entryID) + " in vertices"
        assert entryID != vertices.dummyID, "Entry ID " + str(entryID) + " is not positive"
        self.entryID = entryID
        
    def set_exitID(self, exitID):
        assert exitID in self.the_vertices, "Cannot find vertex " + str(exitID) + " in vertices"
        assert exitID != vertices.dummyID, "Exit ID " + str(exitID) + " is not positive"
        self.exitID = exitID
    
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
                
    def instrument_using_depth_first_spanning_tree(self):
        dfs = trees.DepthFirstSearch(self, self.entryID)
        edges_to_remove = set()
        for v in self:
            for succID in v.successors.keys():
                if not dfs.hasEdge(v.vertexID, succID) \
                and (v.vertexID, succID) != (self.exitID, self.entryID):
                    edges_to_remove.add((v.vertexID, succID))
                    new_vertexID = self.getNextVertexID()
                    newv         = vertices.CFGVertex(new_vertexID, True)
                    self.the_vertices[new_vertexID] = newv
                    self.addEdge(v.vertexID, new_vertexID)
                    self.addEdge(new_vertexID, succID)
        for predID, succID in edges_to_remove:
            self.removeEdge(predID, succID)
