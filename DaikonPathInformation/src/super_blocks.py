import directed_graphs
import vertices
import edges
import debug
import utils

class PathInformationGraph(directed_graphs.DirectedGraph):
    def __init__ (self, cfg, lnt, enhancedCFG):
        directed_graphs.DirectedGraph.__init__(self)
        self.name                         = cfg.name
        self.__enhancedCFG                = enhancedCFG
        self.__monitoredLoopProgramPoints = {}
        self.__loopToSCCs                 = {}
        self.__effectiveLoopCounters      = {}
        self.__monitoredProgramPoints     = set([])
        for the_vertices in lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    debug.debug_message("Analysing header %d" % treev.headerID, 1)
                    forwardCFG, bodyvertices, bodyedges = lnt.induceSubgraph(treev)
                    enhancedCFG           = directed_graphs.EnhancedCFG(forwardCFG)
                    predomTree            = directed_graphs.Dominators(enhancedCFG, enhancedCFG.get_entryID())
                    reverseEnhancedCFG    = enhancedCFG.getReverseGraph()
                    postdomTree           = directed_graphs.Dominators(reverseEnhancedCFG, reverseEnhancedCFG.get_entryID())
                    dominatorg            = DominatorGraph(predomTree, postdomTree)
                    self.__monitoredLoopProgramPoints[treev.headerID] = self.__pinpointMonitoredCFGedges(treev.headerID, dominatorg, lnt, cfg, enhancedCFG, bodyvertices, bodyedges)
                    self.__pinpointRelativeLoopBoundProgramPoints(treev, dominatorg, lnt, enhancedCFG, reverseEnhancedCFG)
                    self.__monitoredProgramPoints.update(self.__monitoredLoopProgramPoints[treev.headerID])
                    print "==========>", self._name, ": header", treev.headerID
                    for programPoint in sorted(self.__monitoredLoopProgramPoints[treev.headerID].keys()):
                        if isinstance(programPoint, tuple):
                            print "int __count_%d_%d = 0;" % (programPoint[0], programPoint[1])
                        else:
                            print "int __count_%d = 0;" % programPoint
                    if treev.headerID != cfg.get_entryID():
                        loopBody = lnt.getLoopBody(treev.headerID)
                        v        = cfg.getVertex(treev.headerID)
                        for succID in v.getSuccessorIDs():
                            if succID in loopBody:
                                print "int __count_%d_%d = 0; //Loop counter" % (treev.headerID, succID)                                
                    assert self.__monitoredLoopProgramPoints[treev.headerID], "No program points identified for loop with header %d" % treev.headerID
        reachability = self.__computeReachability(self.__enhancedCFG, self.__monitoredProgramPoints)
        self.__programPointToVertex = {}
        self.__addvertices()
        self.__addedges(lnt, reachability)
        
    def __addHeaderIDs (self, lnt, enhancedCFG, monitoredProgramPoints, scc, programPoint):
        for vertexID in scc:
            enhancedv = enhancedCFG.getVertex(vertexID)
            if isinstance(enhancedv, vertices.CFGEdge):
                edge     = enhancedv.getEdge()
                headerID = lnt.isLoopExitEdge(edge[0], edge[1])
                if headerID:
                    monitoredProgramPoints[programPoint].add(headerID)
    
    def __pinpointMonitoredCFGedges (self, headerID, dominatorg, lnt, cfg, enhancedCFG, bodyvertices, bodyedges): 
        self.__loopToSCCs[headerID] = StrongComponents(dominatorg)
        monitoredProgramPoints = {}
        for sccID in xrange(1, self.__loopToSCCs[headerID].numberOfSCCs()+1):
            scc           = self.__loopToSCCs[headerID].vertexIDs(sccID)
            externaledges = False
            for vertexID in scc:
                v = dominatorg.getVertex(vertexID)
                for succID in v.getSuccessorIDs():
                    if succID not in scc:
                        externaledges = True
            if not externaledges:
                programPoint = None
                for vertexID in scc:
                    enhancedv = enhancedCFG.getVertex(vertexID)
                    if isinstance(enhancedv, vertices.CFGEdge):
                        edge = enhancedv.getEdge()
                        if edge in bodyedges:
                            if lnt.isLoopExitEdge(edge[0], edge[1]):
                                programPoint = edge
                                break
                            elif not programPoint:
                                programPoint = edge
                if not programPoint:
                    for vertexID in scc:
                        enhancedv = enhancedCFG.getVertex(vertexID)
                        if not isinstance(enhancedv, vertices.CFGEdge):
                            bbID = enhancedv.vertexID
                            if bbID in bodyvertices:
                                programPoint = bbID
                                break
                assert programPoint
                monitoredProgramPoints[programPoint] = set([])
                self.__addHeaderIDs(lnt, enhancedCFG, monitoredProgramPoints, scc, programPoint)         
        if headerID == cfg.get_entryID() and cfg.get_exitID() not in monitoredProgramPoints:
            # Ensure that the exit vertex is always an analysed program point
            exitID = cfg.get_exitID()
            monitoredProgramPoints[exitID] = set([])
        return monitoredProgramPoints
    
    def __doVisit (self, targetID, headerID, enhancedCFG):
        added   = set([])
        visited = set([])
        stack   = []
        stack.append(targetID)
        while stack:
            vertexID = stack.pop()
            visited.add(vertexID)
            enhancedv = enhancedCFG.getVertex(vertexID)
            if isinstance(enhancedv, vertices.CFGEdge):
                programPoint = enhancedv.getEdge()
            else:
                programPoint = vertexID
            if programPoint in self.__monitoredLoopProgramPoints[headerID]:
                self.__monitoredLoopProgramPoints[headerID][programPoint].add(targetID)
                added.add(programPoint)
            else:
                for succID in enhancedv.getSuccessorIDs():
                    if succID not in visited:
                        stack.append(succID)
        return added
    
    def __pinpointRelativeLoopBoundProgramPoints (self, headerv, dominatorg, lnt, enhancedCFG, reverseEnhancedCFG):
        headerID = headerv.getHeaderID()
        # Work out which program points in this loop region will contribute to the relative bound
        if headerv.getLevel() > 0:
            self.__doVisit(headerID, headerID, enhancedCFG)           
        
        for succID in headerv.getSuccessorIDs():
            succv = lnt.getVertex(succID)
            if isinstance(succv, vertices.HeaderVertex):
                innerHeaderID = succv.getHeaderID()
                added = self.__doVisit(innerHeaderID, headerID, enhancedCFG)
                if not added:
                    added = self.__doVisit(innerHeaderID, headerID, reverseEnhancedCFG) 
                    assert added                        
                
    def getEnhancedCFG (self):
        return self.__enhancedCFG
    
    def getLoopMonitoredProgramPoints (self, headerID):
        assert headerID in self.__monitoredLoopProgramPoints, "No program points found for header %d" % headerID
        return self.__monitoredLoopProgramPoints[headerID]
    
    def getMonitoredProgramPoints (self):
        return self.__monitoredProgramPoints
    
    def isExecutedFunction (self):
        for v in self:
            succe = v.getSuccessoredges(edges.PathInformationEdgeType.CAPACITY_BOUNDS)[0]
            if succe.upper > 0:
                return True
        return False
    
    def numOfAlwaysExecuteedges (self):
        count = 0
        for v in self:
            succe = v.getSuccessoredges(edges.PathInformationEdgeType.CAPACITY_BOUNDS)[0]
            if succe.lower > 0:
                count += 1
        return count
    
    def numOfNeverExecuteedges (self):
        count = 0
        for v in self:
            succe = v.getSuccessoredges(edges.PathInformationEdgeType.CAPACITY_BOUNDS)[0]
            if succe.lower == 0 and succe.upper == 0:
                count += 1
        return count
    
    def mutualExclusionPairs (self):
        the_edges = set([])
        for v in self:
            vertexID = v.vertexID
            for succe in v.getSuccessoredges(edges.PathInformationEdgeType.EXCLUSION):
                succID = succe.vertexID
                if (vertexID, succID) not in the_edges and (succID, vertexID) not in the_edges:
                    the_edges.add((vertexID, succID))
        return sorted(the_edges)
    
    def executionDependencies (self):
        the_edges = set([])
        for v in self:
            vertexID = v.vertexID
            for succe in v.getSuccessoredges(edges.PathInformationEdgeType.INCLUSION):
                succID = succe.vertexID
                succv  = self.getVertex(succID)
                if not succv.hasSuccessorEdge(vertexID, edges.PathInformationEdgeType.INCLUSION):
                    the_edges.add((vertexID, succID))
        return sorted(the_edges)
    
    def mutualInclusionPairs (self):
        the_edges = set([])
        for v in self:
            vertexID = v.vertexID
            for succe in v.getSuccessoredges(edges.PathInformationEdgeType.INCLUSION):
                succID = succe.vertexID
                succv  = self.getVertex(succID)
                if succv.hasSuccessorEdge(vertexID, edges.PathInformationEdgeType.INCLUSION):
                    if (vertexID, succID) not in the_edges and (succID, vertexID) not in the_edges:
                        the_edges.add((vertexID, succID))
        return sorted(the_edges)
    
    def isMonitoredVertex (self, vertexID):
        if vertexID in self.__programPointToVertex:
            return self.__programPointToVertex[vertexID]
                            
    def isMonitoredEdge (self, predID, succID):
        if (predID, succID) in self.__programPointToVertex:
            return self.__programPointToVertex[(predID, succID)]
    
    def getProgramPointVertex (self, programPoint):
        return self.__programPointToVertex[programPoint]
    
    def __addvertices (self):
        for headerID, programPoints in self.__monitoredLoopProgramPoints.iteritems():
            for programPoint in programPoints.keys():
                if isinstance(programPoint, tuple):
                    enhancedv = self.__enhancedCFG.getVertexForCFGEdge(programPoint)
                    vertexID  = enhancedv.vertexID
                    pathv     = vertices.PathInformationVertex(vertexID, programPoint, headerID)
                    if programPoints[programPoint]:
                        pathv.setCounterForHeaders(programPoints[programPoint])
                    self.vertices[vertexID] = pathv
                    self.__programPointToVertex[programPoint] = pathv
                else:
                    vertexID  = programPoint
                    pathv     = vertices.PathInformationVertex(vertexID, programPoint, headerID)
                    if programPoints[programPoint]:
                        pathv.setCounterForHeaders(programPoints[programPoint])
                    self.vertices[vertexID] = pathv
                    self.__programPointToVertex[vertexID] = pathv
    
    def __computeReachability (self, enhancedCFG, monitoredProgramPoints):
        # Initialise data flow information
        reachability = {}
        for v in enhancedCFG:
            reachability[v] = set([])
        # Do data-flow analysis
        dfs     = directed_graphs.DepthFirstSearch(enhancedCFG, enhancedCFG.get_entryID())
        changed = True
        while changed:
            changed = False
            for vertexID in reversed(dfs.getPostorder()):
                v       = enhancedCFG.getVertex(vertexID)
                oldSize = len(reachability[v])
                for predID in v.getPredecessorIDs():
                    if predID != enhancedCFG.get_exitID() and vertexID != enhancedCFG.get_entryID():
                        predv = enhancedCFG.getVertex(predID)
                        reachability[v].update(reachability[predv])
                        if isinstance(predv, vertices.Vertex): 
                            if predID in monitoredProgramPoints:
                                reachability[v].add(predv)
                        if isinstance(predv, vertices.CFGEdge): 
                            edge = predv.getEdge() 
                            if edge in monitoredProgramPoints:
                                reachability[v].add(predv)
                if len(reachability[v]) != oldSize:
                    changed = True
        return reachability
    
    def __addedges (self, lnt, reachabilityInformation):
        for v1CFG, reachable in reachabilityInformation.iteritems():
            vertexID1 = v1CFG.vertexID
            if self.hasVertex(vertexID1):
                # Add capacity-bound edge
                v1 = self.getVertex(vertexID1)
                v1.addSuccessorEdge(vertexID1, edges.PathInformationEdgeType.CAPACITY_BOUNDS)
                v1.addSuccessorEdge(vertexID1, edges.PathInformationEdgeType.LOOP_BOUNDS)
                for v2CFG in reachable:
                    vertexID2 = v2CFG.vertexID
                    # Add mutual-inclusion edge
                    if vertexID1 != vertexID2:
                        v2 = self.getVertex(vertexID2)
                        if not v1.hasSuccessorEdge(vertexID2, edges.PathInformationEdgeType.INCLUSION):
                            v1.addSuccessorEdge(vertexID2, edges.PathInformationEdgeType.INCLUSION)
                        if not v2.hasSuccessorEdge(vertexID1, edges.PathInformationEdgeType.INCLUSION):
                            v2.addSuccessorEdge(vertexID1, edges.PathInformationEdgeType.INCLUSION)    

class DominatorGraph (directed_graphs.DirectedGraph):
    def __init__ (self, predomTree, postdomTree):
        directed_graphs.DirectedGraph.__init__(self)
        self.__addvertices (predomTree, postdomTree)
        self.__addedges (predomTree, postdomTree)

    def __addvertices (self, predomTree, postdomTree):
        for v in predomTree:
            vertexID = v.vertexID
            assert postdomTree.hasVertex(vertexID), "Vertex %d in pre-dominator tree but not in post-dominator tree" % vertexID
            clonev   = vertices.Vertex(vertexID)
            self.vertices[vertexID] = clonev        

    def __addedges (self, predomTree, postdomTree):
        # Pre-dominator tree edges
        for v in predomTree:
            vertexID = v.vertexID
            if vertexID != predomTree.getRootID():
                self.addEdge(v.getParentID(), vertexID)
        # Post-dominator tree edges
        for v in postdomTree:
            vertexID = v.vertexID
            if vertexID != postdomTree.getRootID(): 
                parentID = v.getParentID()
                if not self.getVertex(vertexID).hasPredecessor(parentID):
                    self.addEdge(v.getParentID(), vertexID)

Colors = utils.enum('WHITE', 'BLACK', 'GRAY', 'BLUE', 'RED')

class StrongComponents ():
    def __init__ (self, directedg):
        self.__directedg      = directedg
        self.__vertexToColour = {}
        self.__vertexToSCC    = {}
        self.__SCCTovertices  = {}
        # Initialise
        for v in directedg:
            vertexID = v.vertexID
            self.__vertexToColour[vertexID] = Colors.WHITE
            self.__vertexToSCC[vertexID]    = 0
        # Depth-first search on forward graph
        self.__preCounter = 0
        vertexList        = []
        for v in directedg:
            vertexID = v.vertexID
            if self.__vertexToColour[vertexID] == Colors.WHITE:
                self.__visit1(v, vertexList)
        # Depth-first search on reverse graph
        self.__sccCounter = 0
        self.__reverseg   = directedg.getReverseGraph()
        for vertexID in reversed(vertexList):
            if self.__vertexToColour[vertexID] == Colors.BLACK:
                self.__sccCounter += 1
                self.__SCCTovertices[self.__sccCounter] = set([])
                # The vertex v is from the forward directed graph.
                # Need to get the vertex from the reverse graph instead
                self.__visit2(self.__reverseg.getVertex(vertexID))
    
    def __visit1 (self, v, vertexList):
        stack = []
        stack.append(v)
        while stack:
            poppedv  = stack.pop()
            vertexID = poppedv.vertexID
            if self.__vertexToColour[vertexID] == Colors.WHITE:
                self.__vertexToColour[vertexID] = Colors.GRAY
                stack.append(poppedv)
                for succID in poppedv.getSuccessorIDs():
                    if self.__vertexToColour[succID] == Colors.WHITE:
                        stack.append(self.__directedg.getVertex(succID))
            elif self.__vertexToColour[vertexID] == Colors.GRAY:  
                self.__vertexToColour[vertexID] = Colors.BLACK
                vertexList.append(vertexID)
                
    def __visit2 (self, v):
        stack = []
        stack.append(v)
        while stack:
            poppedv = stack.pop()
            vertexID = poppedv.vertexID
            self.__vertexToSCC[vertexID] = self.__sccCounter
            self.__SCCTovertices[self.__sccCounter].add(vertexID)
            debug.debug_message("Vertex %d is in SCC %d" % (vertexID, self.__sccCounter), 15)
            if self.__vertexToColour[vertexID] == Colors.BLACK:
                self.__vertexToColour[vertexID] = Colors.BLUE
                stack.append(poppedv)
                for succID in poppedv.getSuccessorIDs():
                    if self.__vertexToColour[succID] == Colors.BLACK:
                        stack.append(self.__reverseg.getVertex(succID))
            elif self.__vertexToColour[vertexID] == Colors.BLUE:
                self.__vertexToColour[vertexID] = Colors.RED
                
    def numberOfSCCs (self):
        return self.__sccCounter
    
    def getSCCID (self, vertexID):
        assert vertexID in self.__vertexToSCC, "Unable to find SCC of vertex %d" % vertexID
        return self.__vertexToSCC[vertexID]
    
    def vertexIDs (self, sccID):
        assert sccID in self.__SCCTovertices, "Unable to find set of vertices associated with SCC ID %d" % sccID
        return self.__SCCTovertices[sccID]
        
                        
                        
