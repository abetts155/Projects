import directed_graphs
import trees
import vertices
import edges
import debug
import udraw
import cfgs

edge_ID = 0

class IPG (directed_graphs.FlowGraph):
    def __init__(self, icfg):
        directed_graphs.FlowGraph.__init__(self)
        self.name = icfg.name
        self.add_vertices(icfg)
        self.add_edges(icfg)
        self.set_entry_and_exit(icfg)
    
    def add_vertices(self, icfg):
        for v in icfg:
            if v.is_ipoint:
                newv = vertices.CFGVertex(v.vertexID, True)
                self.the_vertices[v.vertexID] = newv
                
    def add_edges (self, icfg):
        vertex_to_reachable = {}
        for v in icfg:
            vertex_to_reachable[v.vertexID] = {}
        # Do data-flow analysis
        dfs     = trees.DepthFirstSearch(icfg, icfg.get_entryID())
        changed = True
        while changed:
            changed = False
            for vertexID in reversed(dfs.getPostorder()):
                v = icfg.getVertex(vertexID)
                for predID in v.predecessors.keys():
                    predv = icfg.getVertex(predID)
                    if predv.is_ipoint:
                        if predID not in vertex_to_reachable[vertexID]:
                            changed = True
                            vertex_to_reachable[vertexID][predID] = set()
                            if not v.is_ipoint:
                                vertex_to_reachable[vertexID][predID].add(v)
                    else:
                        for keyID in vertex_to_reachable[predID]:
                            if keyID not in vertex_to_reachable[vertexID]:
                                changed = True
                                vertex_to_reachable[vertexID][keyID] = set()
                                vertex_to_reachable[vertexID][keyID].update(vertex_to_reachable[predID][keyID])
                                if not v.is_ipoint:
                                    vertex_to_reachable[vertexID][keyID].add(v)
                            else:
                                oldSize = len(vertex_to_reachable[vertexID][keyID])
                                vertex_to_reachable[vertexID][keyID].update(vertex_to_reachable[predID][keyID])
                                newSize = len(vertex_to_reachable[vertexID][keyID])
                                if newSize != oldSize:
                                    changed = True
        # Now add the edges 
        for vertexID, ipointToVertexSet in vertex_to_reachable.iteritems():
            v = icfg.getVertex(vertexID)
            if v.is_ipoint:
                for predID, vertex_set in ipointToVertexSet.iteritems():
                    self.add_edge(predID, vertexID, vertex_set)
    
    def add_edge (self, predID, succID, vertex_set):
        global edge_ID
        edge_ID += 1
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        prede = edges.IPGEdge(predID, edge_ID)
        succe = edges.IPGEdge(succID, edge_ID)
        prede.edge_label.update(vertex_set)
        succe.edge_label.update(vertex_set)
        predv.add_successor_edge(succe)
        succv.add_predecessor_edge(prede) 
                    
    def set_entry_and_exit(self, icfg):
        entryv = self.the_vertices[icfg.get_entryID()]
        self.entryID = entryv.vertexID
        assert entryv.number_of_predecessors() == 1, "Entry vertex %s of IPG has multiple predecessors" % self._entryID
        for predID in entryv.predecessors.keys():
            self.exitID = predID
            
class LoopByLoopIPGInformation():    
    def __init__ (self, icfg, lnt, ipg):
        self.name      = icfg.name
        self.loopICFGs = {}
        self.loopIPGs  = {}
        self.iteration_edges  = {}
        self.loop_entry_edges = {}
        self.loop_exit_edges  = {}
        self.initialise(lnt)
        self.construct_loop_IPGs(icfg, lnt, ipg)
    
    def initialise(self, lnt):
        for treev in lnt:
            if isinstance(treev, vertices.HeaderVertex):
                self.iteration_edges[treev.headerID]  = set()
                self.loop_entry_edges[treev.headerID] = set()
                self.loop_exit_edges[treev.headerID]  = set()
                
    def construct_loop_IPGs(self, icfg, lnt, ipg):
        for level, the_vertices in lnt.levelIterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    debug.debug_message("Analysing header %d" % treev.headerID, __name__, 1)
                    loopICFG  = lnt.induce_subgraph(treev)
                    self.loopICFGs[treev.headerID] = loopICFG
                    loopICFG.set_edgeIDs()
                    udraw.make_file(loopICFG, "%s.header_%d.%s" % (icfg.name, treev.headerID, "icfg"))
                    self.loopIPGs[treev.headerID] = LoopIPG(self, treev.headerID, icfg, loopICFG, lnt, ipg)
            
    def get_loop_ICFG(self, headerID):
        assert headerID in self.loopICFGs, "Unable to find the loop ICFG for header %d" % (headerID)
        return self.loopICFGs[headerID]
    
    def get_loop_IPG(self, headerID):
        assert headerID in self.loopIPGs, "Unable to find the loop IPG for header %d" % (headerID)
        return self.loopIPGs[headerID]

class LoopIPG(directed_graphs.FlowGraph):    
    def __init__ (self, loop_by_loop_info, headerID, global_icfg, icfg, lnt, ipg):
        directed_graphs.FlowGraph.__init__(self)
        self.loop_by_loop_info           = loop_by_loop_info
        self.__headerID                  = headerID
        self.__global_icfg               = global_icfg
        self.__icfg                      = icfg
        self.__lnt                       = lnt
        self.__ipg                       = ipg
        self.inner_loop_ipoints          = {}
        self.vertex_to_reachable         = {}
        self.iteration_edge_destinations = set()
        self.iteration_edge_sources      = set()
        self.entry_edge_sources          = set()
        self.add_ipoints()
        self.initialise()
        self.add_acyclic_edges()
        self.add_iteration_edges()
            
    def add_ipoints (self):    
        for v in self.__icfg:
            self.vertex_to_reachable[v.vertexID] = set()
            if v.vertexID == self.__icfg.get_entryID() and not v.is_ipoint:
                self.vertex_to_reachable[v.vertexID].add(v.vertexID)
            if v.is_ipoint:
                self.the_vertices[v.vertexID] = vertices.CFGVertex(v.vertexID, True)
                
    def initialise(self):
        headerv = self.__lnt.getInternalHeaderVertex(self.__lnt.getVertex(self.__headerID).get_parentID())
        for succID in headerv.successors.keys():
            succv = self.__lnt.getVertex(succID)
            if isinstance(succv, vertices.HeaderVertex):
                loopIPG = self.loop_by_loop_info.loopIPGs[succv.headerID]
                for vertexID in loopIPG.iteration_edge_destinations:
                    if not self.hasVertex(vertexID):
                        v       = self.__ipg.getVertex(vertexID)
                        ipointv = vertices.CFGVertex(vertexID, True)
                        self.the_vertices[vertexID] = ipointv
                        self.inner_loop_ipoints[vertexID] = succv.headerID
                for exitID in self.__lnt.getLoopExits(succv.headerID):
                    exitv = self.__global_icfg.getVertex(exitID)
                    if exitv.is_ipoint:
                        if not self.hasVertex(exitID):
                            self.the_vertices[exitID] = vertices.CFGVertex(exitID, True)
                            self.inner_loop_ipoints[exitID] = succv.headerID
                    else:
                        for keyID in loopIPG.getReachableSet(exitID):
                            keyv = self.__global_icfg.getVertex(keyID)
                            if not self.hasVertex(keyID) and keyv.is_ipoint:
                                self.the_vertices[keyID] = vertices.CFGVertex(keyID, True)
                                self.inner_loop_ipoints[keyID] = succv.headerID
                
    def add_acyclic_edges (self):
        # Compute a topological sort on the ICFG
        dfs = trees.DepthFirstSearch(self.__icfg, self.__icfg.get_entryID())
        # If the header of the loop is an ipoint, that is the only destination of an iteration edge
        entryv = self.__global_icfg.getVertex(self.__icfg.get_entryID())
        if entryv.is_ipoint:
            self.iteration_edge_destinations.add(entryv.vertexID)
        # Perform data-flow analysis
        changed = True
        while changed:
            changed = False
            for vertexID in reversed(dfs.getPostorder()):
                debug.debug_message("At vertex %d" % vertexID, __name__, 1)
                v = self.__icfg.getVertex(vertexID)                
                if self.__lnt.isLoopHeader(vertexID) and vertexID != self.__icfg.get_entryID():
                    self.add_loop_entry_edges(v)
                    self.add_ipoints_to_abstract_vertex(v)
                else:
                    for predID in v.predecessors.keys():
                        predv = self.__global_icfg.getVertex(predID)
                        if predv.is_ipoint:
                            if v.is_ipoint:
                                self.add_edge(predID, vertexID)
                            else:
                                self.vertex_to_reachable[vertexID].add(predID)
                        else:
                            for keyID in self.vertex_to_reachable[predID]:
                                keyv = self.__global_icfg.getVertex(keyID)
                                if keyv.is_ipoint and v.is_ipoint:
                                    self.add_edge(keyID, vertexID)
                                elif not keyv.is_ipoint and v.is_ipoint:
                                    self.iteration_edge_destinations.add(vertexID)
                                else:
                                    self.vertex_to_reachable[vertexID].add(keyID)
                if vertexID in self.__lnt.getLoopTails(self.__headerID):
                    if v.is_ipoint:
                        self.iteration_edge_sources.add(vertexID)
                    else:
                        for keyID in self.vertex_to_reachable[vertexID]:
                            keyv = self.__global_icfg.getVertex(keyID)
                            if keyv.is_ipoint:
                                self.iteration_edge_sources.add(keyID)
                            
    def add_loop_entry_edges (self, v):
        debug.debug_message("Inner header %d detected" % v.vertexID, __name__, 1)
        loopIPG = self.loop_by_loop_info.loopIPGs[v.vertexID]        
        for predID in v.predecessors.keys():
            predv = self.__global_icfg.getVertex(predID)
            if predv.is_ipoint:
                self.entry_edge_sources.add(predID)
                for succID in loopIPG.iteration_edge_destinations:
                    self.add_edge(predID, succID)
            else:
                for keyID in self.vertex_to_reachable[predID]:
                    keyv = self.__global_icfg.getVertex(keyID)
                    if keyv.is_ipoint:
                        self.entry_edge_sources.add(keyID)
                        for succID in loopIPG.iteration_edge_destinations:
                            self.add_edge(keyID, succID)
                    else:
                        # The key is a header vertex. This means that all
                        # the destinations of iteration edges of the inner loop are
                        # also destinations of iterations edges of the outer loop 
                        for succID in loopIPG.iteration_edge_destinations: 
                            self.iteration_edge_destinations.add(succID)
        
    def add_ipoints_to_abstract_vertex (self, v):
        loopIPG = self.loop_by_loop_info.loopIPGs[v.vertexID]               
        for exitID in self.__lnt.getLoopExits(v.vertexID):
            exitv = self.__global_icfg.getVertex(exitID)
            if exitv.is_ipoint:
                self.vertex_to_reachable[v.vertexID].add(exitID)
            else:
                for keyID in loopIPG.getReachableSet(exitID):
                    keyv = self.__global_icfg.getVertex(keyID)
                    if keyv.is_ipoint:
                        self.vertex_to_reachable[v.vertexID].add(keyID)
        if not v.is_ipoint:
            for predID in v.predecessors.keys():
                predv = self.__global_icfg.getVertex(predID)
                if not predv.is_ipoint:
                    if self.__headerID in self.vertex_to_reachable[predID]:
                        self.vertex_to_reachable[v.vertexID].add(self.__headerID)
                            
    def add_iteration_edges (self):
        for predID in self.iteration_edge_sources:
            for succID in self.iteration_edge_destinations:
                self.add_edge(predID, succID)
                self.loop_by_loop_info.iteration_edges[self.__headerID].add((predID, succID))
                            
    def add_edge(self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        if not predv.has_successor(succID):
            assert not succv.has_predecessor(predID), "Vertex %d has predecessor %d although vertex %d does not have successor %d" % (succID, predID, predID, succID)
            originalpredv = self.__ipg.getVertex(predID)
            originaledge  = originalpredv.get_successor_edge(succID)
            succe = edges.IPGEdge(succID, originaledge.get_edgeID())
            prede = edges.IPGEdge(predID, originaledge.get_edgeID())
            predv.add_successor_edge(succe)
            succv.add_predecessor_edge(prede)
            if predID in self.inner_loop_ipoints:
                innerheaderID = self.inner_loop_ipoints[predID]
                debug.debug_message("(%d, %d) is a loop-exit edge for loop with header %d" % (predID, succID, innerheaderID), __name__, 1)
                self.loop_by_loop_info.loop_exit_edges[innerheaderID].add((predID, succID))
            elif succID in self.inner_loop_ipoints:
                innerheaderID = self.inner_loop_ipoints[succID]
                debug.debug_message("(%d, %d) is a loop-entry edge for loop with header %d" % (predID, succID, innerheaderID), __name__, 1)
                self.loop_by_loop_info.loop_entry_edges[innerheaderID].add((predID, succID))
            
    def getReachableSet(self, vertexID):
        assert vertexID in self.vertex_to_reachable, "Unable to find %d in the ICFG" % vertexID
        return self.vertex_to_reachable[vertexID]
    