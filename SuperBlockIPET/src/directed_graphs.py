import vertices
import debug
import copy

class DirectedGraph:        
    def __init__ (self):
        self.the_vertices = {}
        self.name = None
        
    def addVertex(self, v):
        assert v.vertexID not in self.the_vertices, "Adding vertex %d which is already in graph" % v.vertexID
        self.the_vertices[v.vertexID] = v
    
    def getVertex(self, vertexID):
        assert vertexID in self.the_vertices, "Vertex " + str(vertexID) + " is not in the graph"
        return self.the_vertices[vertexID]
    
    def removeVertex(self, vertexID):
        assert vertexID in self.the_vertices, "Vertex " + str(vertexID) + " is not in the graph"
        del self.the_vertices[vertexID]
    
    def hasVertex(self, vertexID):
        return vertexID in self.the_vertices
    
    def addEdge(self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        predv.add_successor(succID)
        succv.add_predecessor(predID)
        
    def hasEdge(self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        return predv.has_successor(succID) or succv.hasPredecessor(predID)
    
    def removeEdge(self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        predv.remove_successor(succID)
        succv.remove_predecessor(predID)
        
    def get_reverse_graph(self):
        reverseg = DirectedGraph() 
        self.add_vertices_to_reverse_graph(reverseg)
        self.add_edges_to_reverse_graph(reverseg)
        return reverseg
    
    def add_vertices_to_reverse_graph(self, reverseg):
        for v in self:
            copyv = copy.copy(v)
            copyv.successors   = {}
            copyv.predecessors = {}
            reverseg.the_vertices[copyv.vertexID] = copyv
            
    def add_edges_to_reverse_graph(self, reverseg):
        for v in self:
            predID = v.vertexID
            predv  = reverseg.getVertex(predID)
            for succID in v.successors.keys():
                succv = reverseg.getVertex(succID)
                predv.add_predecessor(succID)
                succv.add_successor(predID)
        
    def add_predecessor_edges(self):
        for v in self:
            for succID in v.successors.keys():
                succv = self.getVertex(succID)
                if not succv.has_predecessor(v.vertexID):
                    succv.add_predecessor(v.vertexID)
    
    def get_next_vertexID(self):
        nextID = 1
        while nextID in self.the_vertices.keys():
            nextID += 1 
        return nextID
    
    def number_of_vertices(self):
        return len(self.the_vertices)
    
    def number_of_edges(self):
        total = 0
        for v in self.the_vertices.values():
            total += v.number_of_successors()
        return total
    
    def __iter__ (self):
        return self.the_vertices.values().__iter__()
    
    def __str__ (self):
        string = "*" * 40 + "\n"
        for v in self.the_vertices.values():
            string += v.__str__()
        return string

class FlowGraph(DirectedGraph):
    def __init__ (self):
        DirectedGraph.__init__(self)
        self.entryID = vertices.dummyID
        self.exitID  = vertices.dummyID
        
    def get_entryID (self):
        assert self.entryID != vertices.dummyID, "Entry to flow graph not found"
        return self.entryID
    
    def get_exitID (self):
        assert self.exitID != vertices.dummyID, "Exit to flow graph not found"
        return self.exitID
        
    def get_reverse_graph(self):
        reverseg = FlowGraph() 
        self.add_vertices_to_reverse_graph(reverseg)
        self.add_edges_to_reverse_graph(reverseg)
        reverseg.entryID = self.get_exitID()
        reverseg.exitID  = self.get_entryID()
        return reverseg
            
    def set_edgeIDs (self):
        edgeID = 1
        for v in self:
            for succID in v.successors.keys():
                succe = v.get_successor_edge(succID)
                succe.set_edgeID(edgeID)
                succv = self.getVertex(succID)
                prede = succv.get_predecessor_edge(v.vertexID)
                prede.set_edgeID(edgeID)
                edgeID += 1
        
class EnhancedCFG(FlowGraph):
    def __init__ (self):
        FlowGraph.__init__(self)
        
    def create_from_CFG(self, cfg):
        for v in cfg:
            newv = copy.deepcopy(v) 
            newv.successors   = {}
            newv.predecessors = {}
            self.the_vertices[v.vertexID] = newv
            if v.vertexID == cfg.get_entryID():
                self.entryID = v.vertexID
            if v.vertexID == cfg.get_exitID():
                self.exitID = v.vertexID
        assert self.entryID != vertices.dummyID
        assert self.exitID != vertices.dummyID
        for v in cfg:
            for succID in v.successors.keys():
                newID = self.get_next_edge_vertexID()
                newv  = vertices.CFGEdge(newID, v.vertexID, succID)
                if self.getVertex(v.vertexID).dummy or self.getVertex(succID).dummy:
                    newv.dummy = True
                self.the_vertices[newID] = newv
                self.addEdge(v.vertexID, newID)
                self.addEdge(newID, succID)
                
    def get_next_edge_vertexID(self):
        nextID = -1
        while nextID in self.the_vertices.keys():
            nextID -= 1 
        return nextID
                
    def patch_back_edges_with_correct_header(self, loop_tails, headerID):
        for v in self:
            if isinstance(v, vertices.CFGEdge) and v.dummy:
                pred_basic_block = self.getVertex(v.edge[0])
                succ_basic_block = self.getVertex(v.edge[1])
                if pred_basic_block.vertexID in loop_tails and succ_basic_block.dummy:
                    v.edge  = (pred_basic_block.vertexID, headerID)
                    v.dummy = False                    
        
class CFG(FlowGraph):    
    def __init__ (self):
        FlowGraph.__init__(self)
        
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
            entryID = self.get_next_vertexID()
            entryv  = vertices.CFGVertex(entryID)
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
            exitID = self.get_next_vertexID()
            exitv  = vertices.CFGVertex(exitID)
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
    