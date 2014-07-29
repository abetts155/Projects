import directed_graphs
import vertices
import trees
import debug
import udraw
import copy

class ReachabilityInformation:
    def __init__(self, forward_flow_graph, reverse_flow_graph):
        self.unified = {}
        self.forward = {}
        self.reverse = {}
        self.initialise(forward_flow_graph)
        self.compute(forward_flow_graph, reverse_flow_graph)
    
    def initialise(self, cfg):
        for v in cfg:
            self.unified[v.vertexID] = set()
            self.forward[v.vertexID] = set()
            self.reverse[v.vertexID] = set()
            self.forward[v.vertexID].add(v.vertexID)
            self.reverse[v.vertexID].add(v.vertexID)
            
    def compute(self, forward_flow_graph, reverse_flow_graph):
        dfs = forward_flow_graph.get_depth_first_search_tree()
        for vertexID in reversed(dfs.post_order):
            v = forward_flow_graph.getVertex(vertexID)
            for predID in v.predecessors.keys():
                self.forward[v.vertexID].update(self.forward[predID])
        dfs = reverse_flow_graph.get_depth_first_search_tree()
        for vertexID in reversed(dfs.post_order):
            v = reverse_flow_graph.getVertex(vertexID)
            for predID in v.predecessors.keys():
                self.reverse[v.vertexID].update(self.reverse[predID])
            self.unified[v.vertexID] = self.forward[v.vertexID].union(self.reverse[v.vertexID])

class FlowGraph(directed_graphs.DirectedGraph):
    def __init__ (self):
        directed_graphs.DirectedGraph.__init__(self)
        self.entryID          = vertices.dummyID
        self.exitID           = vertices.dummyID
        self.reverse_graph    = None 
        self.depth_first_tree = None
        self.dominator_tree   = None
        
    def get_entryID (self):
        assert self.entryID != vertices.dummyID, "Entry to flow graph not found"
        return self.entryID
    
    def get_exitID (self):
        assert self.exitID != vertices.dummyID, "Exit to flow graph not found"
        return self.exitID
            
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
    
    def get_reverse_graph(self):
        if self.reverse_graph is None:
            self.reverse_graph = FlowGraph() 
            self.add_vertices_to_reverse_graph(self.reverse_graph)
            self.add_edges_to_reverse_graph(self.reverse_graph)
            self.reverse_graph.entryID = self.get_exitID()
            self.reverse_graph.exitID  = self.get_entryID()
        return self.reverse_graph
    
    def get_depth_first_search_tree(self):
        if self.depth_first_tree is None:
            self.depth_first_tree = trees.DepthFirstSearch(self, self.entryID)
        return self.depth_first_tree            
    
    def get_dominator_tree(self):
        if self.dominator_tree is None:
            self.dominator_tree = trees.Dominators(self, self.entryID)
        return self.dominator_tree
        
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
        
class CFG(FlowGraph):    
    def __init__ (self):
        FlowGraph.__init__(self)
        self.lnt                   = None
        self.enhanced_CFGs         = {}
        self.reverse_enhanced_CFGs = {}
        self.reachability_info     = {}
        
    def get_LNT(self):
        if self.lnt is None:
            self.lnt = trees.LoopNests(self, self.entryID)
        return self.lnt
    
    def create_per_loop_reachability_info(self):
        lnt = self.get_LNT()
        for the_vertices in lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    enhanced_CFG         = lnt.induced_loop_subgraph(treev)
                    enhanced_CFG_reverse = enhanced_CFG.get_reverse_graph()
                    udraw.make_file(enhanced_CFG, "%s.header_%d.enhanced_CFG" % (self.name, treev.headerID))
                    self.enhanced_CFGs[treev.headerID]         = enhanced_CFG
                    self.reverse_enhanced_CFGs[treev.headerID] = enhanced_CFG_reverse
                    self.reachability_info[treev.headerID]     = ReachabilityInformation(enhanced_CFG, 
                                                                                         enhanced_CFG_reverse)
                    
    def create_induced_subgraph(self, entry_vertexID, exit_vertexID):
        lnt = self.get_LNT()
        assert lnt.getVertex(entry_vertexID).parentID == lnt.getVertex(exit_vertexID).parentID
        headerv           = lnt.getVertex(lnt.getVertex(entry_vertexID).parentID)
        enhanced_CFG      = self.enhanced_CFGs[headerv.headerID]
        reachability_info = self.reachability_info[headerv.headerID]
        pair_subset       = set([entry_vertexID, exit_vertexID])
        edges             = set()
        visited           = set()
        stack             = []
        stack.append(exit_vertexID)
        while stack:
            vertexID = stack.pop()
            visited.add(vertexID)
            if vertexID != entry_vertexID:
                v = enhanced_CFG.getVertex(vertexID)
                for predID in v.predecessors.keys():
                    if pair_subset.issubset(reachability_info.unified[predID]):
                        edges.add((predID, vertexID))
                        if not predID in visited:
                            stack.append(predID)
        induced_CFG = EnhancedCFG()
        for an_edge in edges:
            if not induced_CFG.hasVertex(an_edge[0]):
                v = enhanced_CFG.getVertex(an_edge[0])
                newv = copy.deepcopy(v)
                newv.predecessors = {}
                newv.successors   = {}
                induced_CFG.addVertex(newv)
            if not induced_CFG.hasVertex(an_edge[1]):
                v = enhanced_CFG.getVertex(an_edge[1])
                newv = copy.deepcopy(v)
                newv.predecessors = {}
                newv.successors   = {}
                induced_CFG.addVertex(newv)
            induced_CFG.addEdge(an_edge[0], an_edge[1])
        induced_CFG.entryID = entry_vertexID
        induced_CFG.exitID = exit_vertexID
        return induced_CFG
                        
    def set_entry_and_exit(self):
        without_predecessors = []
        without_successors   = []
        for v in self:
            if v.number_of_successors() == 0:
                without_successors.append(v.vertexID)
            if v.number_of_predecessors() == 0:
                without_predecessors.append(v.vertexID)
                
        if len(without_predecessors) == 0:
            debug.exit_message("CFG '%s' does not have an entry point" % self.name)
        elif len(without_predecessors) > 1:
            debug_info = ""
            for bbID in without_predecessors:
                bb = self.getVertex(bbID)
                debug_info += bb.__str__()
            debug.exit_message("CFG '%s' has too many entry points: %s" % (self.name, debug_info))
        else:
            self.set_entryID(without_predecessors[0])
        
        if len(without_successors) == 0:
            debug.exit_message("CFG '%s' does not have an exit point" % self.name)
        elif len(without_successors) > 1:
            debug_info = ""
            for bbID in without_successors:
                bb = self.getVertex(bbID)
                debug_info += bb.__str__()
            debug.exit_message("CFG '%s' has too many exit points: %s" % (self.name, debug_info))
        else:
            self.set_exitID(without_successors[0])
        assert self.entryID, "Unable to set entry ID"
        assert self.exitID, "Unable to set exit ID"
        self.addEdge(self.exitID, self.entryID)
        
    def set_entryID(self, entryID):
        assert entryID in self.the_vertices, "Cannot find vertex " + str(entryID) + " in vertices"
        assert entryID != vertices.dummyID, "Entry ID " + str(entryID) + " is not positive"
        self.entryID = entryID
        
    def set_exitID(self, exitID):
        assert exitID in self.the_vertices, "Cannot find vertex " + str(exitID) + " in vertices"
        assert exitID != vertices.dummyID, "Exit ID " + str(exitID) + " is not positive"
        self.exitID = exitID
        