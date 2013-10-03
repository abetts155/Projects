from DirectedGraphs import DirectedGraph
from CFGs import EnhancedCFG
from Trees import Dominators, DepthFirstSearch
from Vertices import Vertex, CFGEdge, HeaderVertex
from Edges import PathInformationEdge, PathInformationEdgeType
from Utils import enum
import Debug
import copy

class PathInformationGraph (DirectedGraph):
    def __init__ (self, cfg, lnt, enhancedCFG):
        DirectedGraph.__init__(self)
        self._name         = cfg.getName()
        self.__enhancedCFG = enhancedCFG
        monitoredCFGEdges = set([])
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, HeaderVertex):
                    headerID = v.getHeaderID()
                    Debug.debugMessage("Analysing header %d" % headerID, 1)
                    forwardCFG         = lnt.induceSubgraph(v)
                    enhancedCFG        = EnhancedCFG(forwardCFG)
                    predomTree         = Dominators(enhancedCFG, enhancedCFG.getEntryID())
                    reverseEnhancedCFG = enhancedCFG.getReverseGraph()
                    postdomTree        = Dominators(reverseEnhancedCFG, reverseEnhancedCFG.getEntryID())
                    dominatorg         = DominatorGraph(headerID, enhancedCFG, predomTree, postdomTree)
                    monitoredLoopEdges = dominatorg.pinpointMonitoredCFGEdges(cfg.getEntryID())
                    monitoredCFGEdges.update(monitoredLoopEdges)        
        reachability = self.__computeReachability(self.__enhancedCFG, monitoredCFGEdges)
        self.__monitoredEdges = {}
        self.__addVertices(self.__enhancedCFG, monitoredCFGEdges)
        self.__addEdges(reachability)
        
    def getEnhancedCFG (self):
        return self.__enhancedCFG
    
    def numOfAlwaysExecuteEdges (self):
        count = 0
        for v in self:
            if v.getLowerBound() > 0:
                count += 1
        return count
    
    def numOfNeverExecuteEdges (self):
        count = 0
        for v in self:
            if v.getLowerBound() == 0 and v.getUpperBound() == 0:
                count += 1
        return count
    
    def mutualExclusionPairs (self):
        edges = set([])
        for v in self:
            vertexID = v.getVertexID()
            for succe in v.getSuccessorEdges():
                succID = succe.getVertexID()
                if succe.getType() == PathInformationEdgeType.EXCLUSION:
                    if (vertexID, succID) not in edges and (succID, vertexID) not in edges:
                        edges.add((vertexID, succID))
        return sorted(edges)
    
    def executionDependencies (self):
        edges = set([])
        for v in self:
            vertexID = v.getVertexID()
            for succe in v.getSuccessorEdges():
                succID = succe.getVertexID()
                succv  = self.getVertex(succID)
                if succe.getType() == PathInformationEdgeType.INCLUSION and not succv.hasSuccessor(vertexID):
                    edges.add((vertexID, succID))
        return sorted(edges)
    
    def mutualInclusionPairs (self):
        edges = set([])
        for v in self:
            vertexID = v.getVertexID()
            for succe in v.getSuccessorEdges():
                succID = succe.getVertexID()
                succv  = self.getVertex(succID)
                if succe.getType() == PathInformationEdgeType.INCLUSION and succv.hasSuccessor(vertexID):
                    if (vertexID, succID) not in edges and (succID, vertexID) not in edges:
                        edges.add((vertexID, succID))
        return sorted(edges)
                            
    def isMonitoredEdge (self, predID, succID):
        if (predID, succID) in self.__monitoredEdges:
            return self.__monitoredEdges[(predID, succID)]
    
    def __addVertices (self, enhancedCFG, monitoredCFGEdges):
        for v in enhancedCFG:
            if isinstance(v, CFGEdge) and v.getEdge() in monitoredCFGEdges:
                copyv = copy.deepcopy(v)
                copyv.removeAllSuccessors()
                copyv.removeAllPredecessors()
                self.vertices[copyv.getVertexID()] = copyv
                self.__monitoredEdges[v.getEdge()] = copyv
    
    def __computeReachability (self, enhancedCFG, monitoredCFGEdges):
        # Initialise data flow information
        reachability = {}
        for v in enhancedCFG:
            reachability[v] = set([])
        # Do data-flow analysis
        dfs     = DepthFirstSearch(enhancedCFG, enhancedCFG.getEntryID())
        changed = True
        while changed:
            changed = False
            for vertexID in reversed(dfs.getPostorder()):
                v       = enhancedCFG.getVertex(vertexID)
                oldSize = len(reachability[v])
                for predID in v.getPredecessorIDs():
                    if predID != enhancedCFG.getExitID() and vertexID != enhancedCFG.getEntryID():
                        predv = enhancedCFG.getVertex(predID)
                        reachability[v].update(reachability[predv])
                        if isinstance(predv, CFGEdge): 
                            edge = predv.getEdge() 
                            if edge in monitoredCFGEdges:
                                reachability[v].add(predv)
                if len(reachability[v]) != oldSize:
                    changed = True
        return reachability
    
    def __addEdges (self, reachabilityInformation):
        for v1CFG, reachable in reachabilityInformation.iteritems():
            vertexID1 = v1CFG.getVertexID()
            if self.hasVertex(vertexID1):
                for v2CFG in reachable:
                    vertexID2 = v2CFG.getVertexID()
                    # Avoid self-loops
                    if vertexID1 != vertexID2:
                        v1     = self.getVertex(vertexID1)
                        v2     = self.getVertex(vertexID2)
                        succe1 = PathInformationEdge(vertexID2, PathInformationEdgeType.INCLUSION)
                        succe2 = PathInformationEdge(vertexID1, PathInformationEdgeType.INCLUSION)
                        v1.addSuccessorEdge(succe1)
                        v2.addSuccessorEdge(succe2)    

class DominatorGraph (DirectedGraph):
    def __init__ (self, headerID, enhancedCFG, predomTree, postdomTree):
        DirectedGraph.__init__(self)
        self._headerID     = headerID
        self.__enhancedCFG = enhancedCFG
        self.__addVertices (predomTree, postdomTree)
        self.__addEdges (predomTree, postdomTree)

    def __addVertices (self, predomTree, postdomTree):
        for v in predomTree:
            vertexID = v.getVertexID()
            assert postdomTree.hasVertex(vertexID), "Vertex %d in pre-dominator tree but not in post-dominator tree" % vertexID
            clonev   = Vertex(vertexID)
            self.vertices[vertexID] = clonev        

    def __addEdges (self, predomTree, postdomTree):
        # Pre-dominator tree edges
        for v in predomTree:
            vertexID = v.getVertexID()
            if vertexID != predomTree.getRootID():
                self.addEdge(v.getParentID(), vertexID)
        # Post-dominator tree edges
        for v in postdomTree:
            vertexID = v.getVertexID()
            if vertexID != postdomTree.getRootID(): 
                parentID = v.getParentID()
                if not self.getVertex(vertexID).hasPredecessor(parentID):
                    self.addEdge(v.getParentID(), vertexID)
    
    def pinpointMonitoredCFGEdges (self, entryID):
        monitoredCFGEdges = set([])
        sccs = StrongComponents(self)
        # Avoid the case where everything executes
        if sccs.numberOfSCCs() > 1 and self._headerID != entryID:
            for sccID in xrange(1, sccs.numberOfSCCs()+1):
                scc           = sccs.getVertexIDs(sccID)
                externalEdges = False
                edgev         = None
                for vertexID in scc:
                    v         = self.getVertex(vertexID)
                    enhancedv = self.__enhancedCFG.getVertex(vertexID)
                    if isinstance(enhancedv, CFGEdge):
                        edgev = enhancedv
                    for succID in v.getSuccessorIDs():
                        if succID not in scc:
                            externalEdges = True
                if not externalEdges:
                    assert edgev
                    monitoredCFGEdges.add(edgev.getEdge())
        return monitoredCFGEdges

Colors = enum('WHITE', 'BLACK', 'GRAY', 'BLUE', 'RED')

class StrongComponents ():
    def __init__ (self, directedg):
        self.__directedg      = directedg
        self.__vertexToColour = {}
        self.__vertexToSCC    = {}
        self.__SCCToVertices  = {}
        # Initialise
        for v in directedg:
            vertexID = v.getVertexID()
            self.__vertexToColour[vertexID] = Colors.WHITE
            self.__vertexToSCC[vertexID]    = 0
        # Depth-first search on forward graph
        self.__preCounter = 0
        vertexList        = []
        for v in directedg:
            vertexID = v.getVertexID()
            if self.__vertexToColour[vertexID] == Colors.WHITE:
                self.__visit1(v, vertexList)
        # Depth-first search on reverse graph
        self.__sccCounter = 0
        self.__reverseg   = directedg.getReverseGraph()
        for vertexID in reversed(vertexList):
            if self.__vertexToColour[vertexID] == Colors.BLACK:
                self.__sccCounter += 1
                self.__SCCToVertices[self.__sccCounter] = set([])
                # The vertex v is from the forward directed graph.
                # Need to get the vertex from the reverse graph instead
                self.__visit2(self.__reverseg.getVertex(vertexID))
    
    def __visit1 (self, v, vertexList):
        stack = []
        stack.append(v)
        while stack:
            poppedv  = stack.pop()
            vertexID = poppedv.getVertexID()
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
            vertexID = poppedv.getVertexID()
            self.__vertexToSCC[vertexID] = self.__sccCounter
            self.__SCCToVertices[self.__sccCounter].add(vertexID)
            Debug.debugMessage("Vertex %d is in SCC %d" % (vertexID, self.__sccCounter), 15)
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
    
    def getVertexIDs (self, sccID):
        assert sccID in self.__SCCToVertices, "Unable to find set of vertices associated with SCC ID %d" % sccID
        return self.__SCCToVertices[sccID]
        
                        
                        