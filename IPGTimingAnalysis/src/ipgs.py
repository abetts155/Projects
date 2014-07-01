import directed_graphs
import trees
import vertices
import edges
import debug
import udraw

edge_ID = 1

class IPG (directed_graphs.FlowGraph):
    def __init__(self, icfg):
        directed_graphs.FlowGraph.__init__(self)
        self.name                = icfg.name
        self.__icfg              = icfg
        self.__ipointIDToVertex  = {}
        self.__vertexToReachable = {}
        self.__initialise()
        self.add_edges()
        self.__setEntryAndExit()
        
    def getIpointVertex (self, ipointID):
        assert ipointID in self.__ipointIDToVertex, "Unable to find Ipoint with trace ID 0x%04X" % ipointID
        return self.__ipointIDToVertex[ipointID]
    
    def __initialise (self):
        for v in self.__icfg:
            self.__vertexToReachable[v.vertexID] = {}
            if self.__icfg.isIpoint(v.vertexID):
                ipointv = vertices.Ipoint(v.vertexID, v.ipointID)
                self.the_vertices[v.vertexID] = ipointv
                self.__ipointIDToVertex[v.ipointID] = ipointv 
                
    def add_edges (self):
        changed = True
        dfs     = trees.DepthFirstSearch(self.__icfg, self.__icfg.get_entryID())
        # Do data-flow analysis
        while changed:
            changed = False
            for vertexID in reversed(dfs.getPostorder()):
                v = self.__icfg.getVertex(vertexID)
                for predID in v.predecessors.keys():
                    if self.__icfg.isIpoint(predID):
                        if predID not in self.__vertexToReachable[vertexID]:
                            changed = True
                            self.__vertexToReachable[vertexID][predID] = set([])
                            if not self.__icfg.isIpoint(vertexID):
                                self.__vertexToReachable[vertexID][predID].add(vertexID)
                    else:
                        for keyID in self.__vertexToReachable[predID]:
                            if keyID not in self.__vertexToReachable[vertexID]:
                                changed = True
                                self.__vertexToReachable[vertexID][keyID] = set([])
                                self.__vertexToReachable[vertexID][keyID].update(self.__vertexToReachable[predID][keyID])
                                if not self.__icfg.isIpoint(vertexID):
                                    self.__vertexToReachable[vertexID][keyID].add(vertexID)
                            else:
                                oldSize = len(self.__vertexToReachable[vertexID][keyID])
                                self.__vertexToReachable[vertexID][keyID].update(self.__vertexToReachable[predID][keyID])
                                newSize = len(self.__vertexToReachable[vertexID][keyID])
                                if newSize != oldSize:
                                    changed = True
        # Now add the edges 
        for vertexID, ipointToVertexSet in self.__vertexToReachable.iteritems():
            if self.__icfg.isIpoint(vertexID):
                for predID, vertex_set in ipointToVertexSet.iteritems():
                    self.add_edge(predID, vertexID, vertex_set)
    
    def add_edge (self, predID, succID, vertex_set):
        global edge_ID
        debug.debug_message("Adding IPG Edge %s => %s" % (predID, succID), __name__, 10)
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        prede = edges.IPGEdge(predID, edge_ID)
        succe = edges.IPGEdge(succID, edge_ID)
        prede.edge_label.update(vertex_set)
        succe.edge_label.update(vertex_set)
        predv.add_successor_edge(succe)
        succv.add_predecessor_edge(prede) 
        edge_ID += 1
                    
    def __setEntryAndExit (self):
        entryv = self.the_vertices[self.__icfg.get_entryID()]
        self._entryID = entryv.vertexID
        assert entryv.number_of_predecessors() == 1, "Entry vertex %s of IPG has multiple predecessors" % self._entryID
        for predID in entryv.predecessors.keys():
            self._exitID = predID
            
class LoopByLoopIPGs():    
    def __init__ (self, icfg, lnt, ipg):
        self.name = icfg.name
        self.__headerToMiniIPG         = {}
        self.__headerIterationEdgeIDs  = {}
        self.__headerRelativeEdgeIDs   = {}
        self.__headerToReconstructible = {}
        for level, the_vertices in lnt.levelIterator(True):
            for v in the_vertices:
                if isinstance(v, vertices.HeaderVertex):
                    headerID = v.headerID
                    debug.debug_message("Analysing header %d" % headerID, 
                                        __name__, 
                                        1)
                    forwardICFG = lnt.induceSubgraph(v, self.__headerToReconstructible)
                    forwardICFG.setEdgeIDs()
                    self.__headerToReconstructible[headerID] = forwardICFG.isPathReconstructible()
                    debug.debug_message("Region in %d is %spath reconstructible" % (headerID, "" if self.__headerToReconstructible[headerID] else "NOT "), 
                                        __name__, 
                                        1)
                    udraw.make_file(forwardICFG, "%s.Header%d.%s" % (icfg.name, headerID, "icfg"))
                    miniIPG = LoopIPG(headerID, self.__headerToMiniIPG, forwardICFG, lnt, ipg)
                    self.__headerToMiniIPG[headerID] = miniIPG
            
    def isPathReconstructible (self, headerID):
        assert headerID in self.__headerToReconstructible, "Unable to find whether loop with header %d is path reconstructible" % (headerID)
        return self.__headerToReconstructible[headerID]
    
    def getMiniIPG (self, headerID):
        assert headerID in self.__headerToMiniIPG, "Unable to find the mini IPG of the loop with header %d" % (headerID)
        return self.__headerToMiniIPG[headerID]

class LoopIPG(directed_graphs.FlowGraph):    
    def __init__ (self, headerID, headerToMiniIPG, icfg, lnt, ipg):
        directed_graphs.FlowGraph.__init__(self)
        self.__headerID                  = headerID
        self.__headerToMiniIPG           = headerToMiniIPG
        self.__icfg                      = icfg
        self.__lnt                       = lnt
        self.__ipg                       = ipg
        self.__innerLoopIpoints          = {}
        self.__vertexToReachable         = {}
        self.__innerLoopEntryEdgeIDs     = {}
        self.__innerLoopExitEdgeIDs      = {}
        self.__iterationEdgeDestinations = set()
        self.__iterationEdgeSources      = set()
        self.__iterationEdgeIDs          = set()
        self.__entryEdgeSources          = set()
        self.add_ipoints()
        self.add_acyclic_edges()
        self.add_iteration_edges()
        self.output_entry_and_exit_edges()
            
    def add_ipoints (self):    
        for v in self.__icfg:
            self.__vertexToReachable[v.vertexID] = set([])  
            if v.vertexID == self.__icfg.get_entryID() and not self.__ipg.hasVertex(v.vertexID):
                self.__vertexToReachable[v.vertexID].add(v.vertexID)          
            # Ipoint actions
            if self.__ipg.hasVertex(v.vertexID):
                ipointv = vertices.Ipoint(v.vertexID, v.ipointID)
                self.the_vertices[v.vertexID] = ipointv
                
        headerv = self.__lnt.getInternalHeaderVertex(self.__lnt.getVertex(self.__headerID).get_parentID())
        for succID in headerv.successors.keys():
            succv = self.__lnt.getVertex(succID)
            if isinstance(succv, vertices.HeaderVertex):
                innerheaderID                               = succv.headerID
                self.__innerLoopEntryEdgeIDs[innerheaderID] = set()
                self.__innerLoopExitEdgeIDs[innerheaderID]  = set()
                innerMiniIPG                                = self.__headerToMiniIPG[innerheaderID]
                for vertexID in innerMiniIPG.getIterationEdgeDestinations():
                    if not self.hasVertex(vertexID):
                        v       = self.__ipg.getVertex(vertexID)
                        ipointv = vertices.Ipoint(vertexID, v.ipointID)
                        self.the_vertices[vertexID] = ipointv
                        self.__innerLoopIpoints[vertexID] = innerheaderID
                for exitID in self.__lnt.getLoopExits(innerheaderID):
                    if self.__ipg.hasVertex(exitID):
                        if not self.hasVertex(exitID):
                            v       = self.__ipg.getVertex(exitID)
                            ipointv = vertices.Ipoint(exitID, v.ipointID)
                            self.the_vertices[exitID] = ipointv
                            self.__innerLoopIpoints[exitID] = innerheaderID
                    else:
                        for keyID in innerMiniIPG.getReachableSet(exitID):
                            if not self.hasVertex(keyID) and self.__ipg.hasVertex(keyID):
                                v       = self.__ipg.getVertex(keyID)
                                ipointv = vertices.Ipoint(keyID, v.ipointID)
                                self.the_vertices[keyID] = ipointv
                                self.__innerLoopIpoints[keyID] = innerheaderID
    
    def output_entry_and_exit_edges (self):   
        headerv = self.__lnt.getInternalHeaderVertex(self.__lnt.getVertex(self.__headerID).get_parentID())
        for succID in headerv.successors.keys():
            succv = self.__lnt.getVertex(succID)
            if isinstance(succv, vertices.HeaderVertex):
                innerheaderID = succv.headerID 
                debug.debug_message("Loop-entry edges of %d = %s" % (innerheaderID, self.__innerLoopEntryEdgeIDs[innerheaderID]), 
                                    __name__,
                                    1)
                debug.debug_message("Loop-exit edges of %d = %s" % (innerheaderID, self.__innerLoopExitEdgeIDs[innerheaderID]), 
                                    __name__,
                                    1)
                
    def add_acyclic_edges (self):
        # Compute a topological sort on the ICFG
        dfs = trees.DepthFirstSearch(self.__icfg, self.__icfg.get_entryID())
        # If the header of the loop is an Ipoint, that is the only destination of an iteration edge
        if self.__ipg.hasVertex(self.__icfg.get_entryID()):
            self.__iterationEdgeDestinations.add(self.__icfg.get_entryID())
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
                        if self.__ipg.hasVertex(predID):
                            if self.__ipg.hasVertex(vertexID):
                                self.add_edge(predID, vertexID)
                            else:
                                self.__vertexToReachable[vertexID].add(predID)
                        else:
                            for keyID in self.__vertexToReachable[predID]:
                                if self.__ipg.hasVertex(keyID) and self.__ipg.hasVertex(vertexID):
                                    self.add_edge(keyID, vertexID)
                                elif not self.__ipg.hasVertex(keyID) and self.__ipg.hasVertex(vertexID):
                                    self.__iterationEdgeDestinations.add(vertexID)
                                else:
                                    self.__vertexToReachable[vertexID].add(keyID)
                if vertexID in self.__lnt.getLoopTails(self.__headerID):
                    # Loop tail detected.
                    # Any Ipoint that can reach a loop tail is an iteration edge source
                    if self.__ipg.hasVertex(vertexID):
                        self.__iterationEdgeSources.add(vertexID)
                    else:
                        for keyID in self.__vertexToReachable[vertexID]:
                            if self.__ipg.hasVertex(keyID):
                                self.__iterationEdgeSources.add(keyID)
                            
    def add_loop_entry_edges (self, v):
        debug.debug_message("Inner header %d detected" % v.vertexID, __name__, 1)
        innerMiniIPG = self.__headerToMiniIPG[v.vertexID]        
        for predID in v.predecessors.keys():
            if self.__ipg.hasVertex(predID):
                innerMiniIPG.addEntryEdgeSource(predID)
                for succID in innerMiniIPG.getIterationEdgeDestinations():
                    self.add_edge(predID, succID)
            else:
                for keyID in self.__vertexToReachable[predID]:
                    if self.__ipg.hasVertex(keyID):
                        innerMiniIPG.addEntryEdgeSource(keyID)
                        for succID in innerMiniIPG.getIterationEdgeDestinations():
                            self.add_edge(keyID, succID)
                    else:
                        # The key is a header vertex. This means that all
                        # the destinations of iteration edges of the inner loop are
                        # also destinations of iterations edges of the outer loop 
                        for succID in innerMiniIPG.getIterationEdgeDestinations(): 
                            self.__iterationEdgeDestinations.add(succID)
        
    def add_ipoints_to_abstract_vertex (self, v):
        innerMiniIPG = self.__headerToMiniIPG[v.vertexID]       
        for exitID in self.__lnt.getLoopExits(v.vertexID):
            if self.__ipg.hasVertex(exitID):
                self.__vertexToReachable[v.vertexID].add(exitID)
            else:
                for keyID in innerMiniIPG.getReachableSet(exitID):
                    if self.__ipg.hasVertex(keyID):
                        self.__vertexToReachable[v.vertexID].add(keyID)
        if not self.__icfg.isIpoint(v.vertexID):
            for predID in v.predecessors.keys():
                if not self.__ipg.hasVertex(predID):
                    if self.__headerID in self.__vertexToReachable[predID]:
                        self.__vertexToReachable[v.vertexID].add(self.__headerID)
                            
    def add_iteration_edges (self):
        debug.debug_message("Iteration edge SOURCEs      = %s" % self.__iterationEdgeSources, __name__, 1)
        debug.debug_message("Iteration edge DESTINATIONS = %s" % self.__iterationEdgeDestinations, __name__, 1)
        for predID in self.__iterationEdgeSources:
            for succID in self.__iterationEdgeDestinations:
                self.add_edge(predID, succID)
                originalpredv = self.__ipg.getVertex(predID)
                originaledge  = originalpredv.get_successor_edge(succID)
                edgeID        = originaledge.get_edgeID()
                self.__iterationEdgeIDs.add(edgeID)
        debug.debug_message("Iteration edges for %d is %s" % (self.__headerID, self.__iterationEdgeIDs), __name__, 1)
                            
    def add_edge (self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        if not predv.has_successor(succID):
            debug.debug_message("Adding edge (%d, %d)" % (predID, succID), __name__, 1)
            assert not succv.has_predecessor(predID), "Vertex %d has predecessor %d although vertex %d does not have successor %d" % (succID, predID, predID, succID)
            originalpredv = self.__ipg.getVertex(predID)
            originaledge  = originalpredv.get_successor_edge(succID)
            succe = edges.IPGEdge(succID, originaledge.get_edgeID())
            prede = edges.IPGEdge(predID, originaledge.get_edgeID())
            predv.add_successor_edge(succe)
            succv.add_predecessor_edge(prede)
            if predID in self.__innerLoopIpoints:
                innerheaderID = self.__innerLoopIpoints[predID]
                debug.debug_message("(%d, %d) is a loop-exit edge for loop with header %d" % (predID, succID, innerheaderID), __name__, 1)
                self.__innerLoopExitEdgeIDs[innerheaderID].add(originaledge.get_edgeID())
            elif succID in self.__innerLoopIpoints:
                innerheaderID = self.__innerLoopIpoints[succID]
                debug.debug_message("(%d, %d) is a loop-entry edge for loop with header %d" % (predID, succID, innerheaderID), __name__, 1)
                self.__innerLoopEntryEdgeIDs[innerheaderID].add(originaledge.get_edgeID())
            
    def getReachableSet (self, vertexID):
        assert vertexID in self.__vertexToReachable, "Unable to find %d in the ICFG" % vertexID
        return self.__vertexToReachable[vertexID]
    
    def addEntryEdgeSource (self, vertexID):
        self.__entryEdgeSources.add(vertexID)
        
    def getEntryEdgeSources (self):
        return self.__entryEdgeSources
    
    def getIterationEdgeSources (self):
        return self.__iterationEdgeSources
    
    def getIterationEdgeDestinations (self):
        return self.__iterationEdgeDestinations
    
    def getIterationEdgeIDs (self):
        return self.__iterationEdgeIDs
    
    def isIterationEdgeID (self, edgeID):
        return edgeID in self.__iterationEdgeIDs
    
    def getInnerLoopEntryEdgeIDs (self, innerheaderID):
        assert innerheaderID in self.__innerLoopEntryEdgeIDs, "Unable to find loop-entry edges for header %d" % innerheaderID
        return self.__innerLoopEntryEdgeIDs[innerheaderID]

    def getInnerLoopExitEdgeIDs (self, innerheaderID):
        assert innerheaderID in self.__innerLoopExitEdgeIDs, "Unable to find loop-exit edges for header %d" % innerheaderID
        return self.__innerLoopExitEdgeIDs[innerheaderID]

    