from DirectedGraphs import DirectedGraph
from Vertices import Vertex, HeaderVertex, SuperBlock, PathInformationVertex, dummyVertexID
from Edges import SuperBlockControlFlowEdge, SuperBlockLoopEdge
from Trees import Dominators, DominanceFrontiers, DepthFirstSearch
from Utils import enum
import Debug

nextVertexID = 0
nextEdgeID   = 0

class SuperBlockGraph (DirectedGraph):
    
    def __init__ (self, icfg, lnt):
        DirectedGraph.__init__(self)
        self._name = icfg.getName()
        self.__lnt = lnt
        self.__headerToSuperBlockSubgraph = {}
        self.__headerToRootSuperBlock = {}
        self.__basicBlockToSuperBlock = {}
        self.__rootSuperv             = None
        self.__monitoredSuperBlocks   = set([])
        self.__monitoredEdges         = {}
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, HeaderVertex):
                    headerID = v.getHeaderID()
                    Debug.debugMessage("Analysing header %d" % headerID, 1)
                    forwardICFG        = lnt.induceSubgraph(v)
                    predomTree         = Dominators(forwardICFG, forwardICFG.getEntryID())
                    reverseICFG        = forwardICFG.getReverseCFG()
                    postdomTree        = Dominators(reverseICFG, reverseICFG.getEntryID())
                    postDF             = DominanceFrontiers(reverseICFG, postdomTree)    
                    dominatorg         = DominatorGraph(predomTree, postdomTree)  
                    sccs               = StrongComponents(dominatorg)
                    emptySuperBlocks   = []
                    self.__headerToSuperBlockSubgraph[headerID] = self.__addSuperBlocks(lnt, forwardICFG, postdomTree, sccs, emptySuperBlocks, headerID)
                    self.__headerToRootSuperBlock[headerID]     = self.__addEdges(lnt, forwardICFG, predomTree, postdomTree, postDF, emptySuperBlocks, headerID)
                    self.__headerToRootSuperBlock[headerID].setLoopHeader(headerID)
                    if v.getVertexID() == lnt.getRootID():
                        self.__rootSuperv = self.__headerToRootSuperBlock[headerID]
        self.__computeUnknownPathRelations(icfg)
                        
    def __addSuperBlocks (self, lnt, forwardICFG, postdomTree, sccs, emptySuperBlocks, headerID):
        global nextVertexID 
        subgraph      = DirectedGraph()
        sccIDToVertex = {}
        for sccID in xrange(1, sccs.numberOfSCCs()+1):
            nextVertexID += 1
            superVertexID                = nextVertexID
            superv                       = SuperBlock(superVertexID)
            self.vertices[superVertexID] = superv
            sccIDToVertex[sccID]         = superv
            subgraph.vertices[superVertexID] = superv
        for v in forwardICFG:
            if not v.isDummy():
                vertexID = v.getVertexID()     
                sccID    = sccs.getSCCID(vertexID)
                superv   = sccIDToVertex[sccID]
                superv.addBasicBlock(vertexID)
                self.__basicBlockToSuperBlock[vertexID] = superv
                if v.numberOfSuccessors() > 1:
                    ipostID = postdomTree.getVertex(vertexID).getParentID()
                    if v.hasSuccessor(ipostID):
                        nextVertexID += 1
                        superVertexID = nextVertexID
                        superv        = SuperBlock(superVertexID)
                        self.__monitoredEdges[(vertexID, ipostID)] = superv
                        self.vertices[superVertexID] = superv
                        subgraph.vertices[superVertexID] = superv
                        emptySuperBlocks.append(superv)
        return subgraph
                
    def __addEdges (self, lnt, forwardICFG, predomTree, postdomTree, postDF, emptySuperBlocks, headerID):
        rootSuperv = self.__basicBlockToSuperBlock[headerID]
        dfs        = DepthFirstSearch(forwardICFG, forwardICFG.getEntryID())
        branches   = set([])
        # Process vertices in topological order
        for vertexID in dfs.getPostorder():
            v = forwardICFG.getVertex(vertexID)
            # Found a nested loop header
            if not v.isDummy() and lnt.isLoopHeader(vertexID) and vertexID != headerID:
                sourcev      = self.__basicBlockToSuperBlock[vertexID]
                destinationv = self.__headerToRootSuperBlock[vertexID]
                self.__addEdge(sourcev, destinationv, headerID, SuperBlockLoopEdge)
            # Found a branch
            if v.numberOfSuccessors() > 1:
                branches.add(vertexID)
                sourcev = self.__basicBlockToSuperBlock[vertexID]                
                for succID in v.getSuccessorIDs():
                    if not postdomTree.isAncestor(succID, vertexID):
                        destinationv = self.__basicBlockToSuperBlock[succID]
                        if not sourcev.hasSuccessor(destinationv.getVertexID()):
                            self.__addEdge(sourcev, destinationv, vertexID)
                    else:
                        destinationv = emptySuperBlocks.pop()
                        self.__addEdge(sourcev, destinationv, vertexID)
            # Found a merge
            if v.numberOfPredecessors() > 1:
                ipreID = predomTree.getImmediateDominator(vertexID)
                if postdomTree.getImmediateDominator(ipreID) != vertexID:
                    destinationv = self.__basicBlockToSuperBlock[vertexID]
                    if postDF.size(vertexID) > 1:
                        destinationv.setUnstructuredMerge()
                    for predID in v.getPredecessorIDs():
                        sourcev = self.__basicBlockToSuperBlock[predID]
                        self.__addEdge(sourcev, destinationv, predID)
  
#        reachableSuperBlocks = {}
#        superBlocksToAnalyse = set([])
#        for vertexID in dfs.getPostorder():
#            v = forwardICFG.getVertex(vertexID)
#            reachableSuperBlocks[vertexID] = set([])
#            superv = self.__basicBlockToSuperBlock[vertexID]
#            for succID in v.getSuccessorIDs():
#                reachableSuperBlocks[vertexID].update(reachableSuperBlocks[succID])
#            if not branches.intersection(superv.getBasicBlockIDs()):
#                superBlocksToAnalyse.add(superv)
#                reachableSuperBlocks[vertexID].add(superv)
#        
#        # Add mutual exclusion and mutual inclusion
#        pathRelationEdges = set([])  
#        for superv1 in superBlocksToAnalyse:
#            vertexID = superv1.getRepresentativeID()
#            for superv2 in reachableSuperBlocks[vertexID]:
#                if superv1 != superv2:
#                    pathRelationEdges.add((superv1, superv2, PATHRELATION.MUTUAL_INCLUSION))
#                    pathRelationEdges.add((superv1, superv2, PATHRELATION.MUTUAL_EXCLUSION))
#                    
#        # Add precedence and alternation
#        for branchID in branches:
#            superv1 = self.__basicBlockToSuperBlock[branchID]
#            for superEdges in superv1.getBranchPartitions().values():
#                for supere1 in superEdges:
#                    for supere2 in superEdges:
#                        superv1 = self.getVertex(supere1.getVertexID())
#                        superv2 = self.getVertex(supere2.getVertexID())
#                        if superv1 != superv2:
#                            pathRelationEdges.add((superv1, superv2, PATHRELATION.ALTERNATION))
#                            pathRelationEdges.add((superv1, superv2, PATHRELATION.PRECEDENCE))
#                            
#        self.__truePathRelationEdges.update(pathRelationEdges)
        return rootSuperv
    
    def __addEdge (self, sourcev, destinationv, branchID, edgeType=SuperBlockControlFlowEdge):
        global nextEdgeID
        nextEdgeID += 1
        succe = edgeType(destinationv.getVertexID(), branchID)
        prede = edgeType(sourcev.getVertexID(), branchID)
        sourcev.addSuccessorEdge(succe)
        destinationv.addPredecessorEdge(prede)
        succe.setEdgeID(nextEdgeID)
        prede.setEdgeID(nextEdgeID)
        
    def __computeUnknownPathRelations (self, icfg):
        supervToSupervs = {}
        for superv in self:
            supervToSupervs[superv] = set([])
        dfs = DepthFirstSearch(self, self.__rootSuperv.getVertexID())
        for vertexID in dfs.getPostorder():
            superv = self.getVertex(vertexID)
            if vertexID == self.__rootSuperv.getVertexID():
                self.__addPathRelationEdges(icfg, supervToSupervs)
            else:
                alive = True
                for bbID in superv.getBasicBlockIDs():
                    bb = icfg.getVertex(bbID)
                    if self.__lnt.isLoopHeader(bbID) or bb.numberOfSuccessors() > 1:
                        alive = False
                if alive: 
                    supervToSupervs[superv].add(superv)
                else:
                    for succID in superv.getSuccessorIDs():
                        succv = self.getVertex(succID)
                        supervToSupervs[superv].update(supervToSupervs[succv])
                        
    def __addPathRelationEdges (self, icfg, supervToSupervs):
        succPartition = self.__rootSuperv.getBranchPartitions()
        dfs           = DepthFirstSearch(icfg, icfg.getEntryID())
        newVertexID   = 0
        self.__bottomLayer = []
        for vertexID in reversed(dfs.getPostorder()):
            if vertexID in succPartition:
                newVertexID += 1
                newv = PathInformationVertex(newVertexID)
                self.__bottomLayer.append(newv)
                for supere in succPartition[vertexID]:
                    succv = self.getVertex(supere.getVertexID())
                    for superv in supervToSupervs[succv]:
                        # We will monitor this super block during trace parsing as it forms part of the
                        # bottom layer of the path information graph
                        Debug.debugMessage("%d is a monitored super block in %s" % (superv.getVertexID(), self.getName()), 1)
                        self.__monitoredSuperBlocks.add(superv)
                        newset = frozenset([superv.getVertexID()])
                        newv.setsToRuns[newset] = set([])
            if self.__lnt.isLoopHeader(vertexID) and vertexID in self.__rootSuperv.getBasicBlockIDs():
                succv = self.getSuperBlock(vertexID)
                for superv in supervToSupervs[succv]:
                    Debug.debugMessage("%d is a monitored super block in %s" % (superv.getVertexID(), self.getName()), 1)
                    self.__monitoredSuperBlocks.add(superv)
                    newset = frozenset([superv.getVertexID()])
                    newv.setsToRuns[newset] = set([])
        
    def getBottomLayer (self):
        return self.__bottomLayer
        
    def isMonitoredSuperBlock (self, superv):
        return superv in self.__monitoredSuperBlocks
    
    def isMonitoredEdge (self, predID, succID):
        return (predID, succID) in self.__monitoredEdges
    
    def getMonitoredEdgeSuperBlock (self, predID, succID):
        return self.__monitoredEdges[(predID, succID)]
    
    def getSuperBlock (self, basicBlockID):
        assert basicBlockID in self.__basicBlockToSuperBlock, "Unable to find basic block %d in a super block" % basicBlockID
        # Handle the case where the basic block is a loop header because it will always appear in 2 super blocks
        # and we want the one representing the header, not the abstract vertex
        if basicBlockID in self.__headerToRootSuperBlock:
            return self.__headerToRootSuperBlock[basicBlockID]
        return self.__basicBlockToSuperBlock[basicBlockID]
    
    def getSuperBlockRegion (self, headerID):
        assert headerID in self.__headerToSuperBlockSubgraph, "Unable to find super block CFG portion for header %d" % headerID
        return self.__headerToSuperBlockSubgraph[headerID]
    
    def getRootSuperBlock (self):
        assert self.__rootSuperv, "Root super block has not been set"
        return self.__rootSuperv
    
    def getPathInformationGraph (self):
        return self._pathg
    
class PathInformationGraph (DirectedGraph):
    def __init__ (self):
        DirectedGraph.__init__(self)
        self._rootID = None
    
    def setRootID (self, rootID):
        assert rootID >  dummyVertexID, "Invalid root ID %d. It must be a positive integer" % rootID
        assert rootID in self.vertices, "Cannot find vertex %d"
        self._rootID = rootID
        
    def getRootID (self):
        assert self._rootID != dummyVertexID, "Root ID has not yet been set"
        return self._rootID
    
    def __str__(self):
        return "%s\nROOT ID = %d" % (DirectedGraph.__str__(self), self.getRootID())
    
class DominatorGraph (DirectedGraph):
    def __init__ (self, predomTree, postdomTree):
        DirectedGraph.__init__(self)
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

Colors = enum('WHITE', 'BLACK', 'GRAY', 'BLUE', 'RED')

class StrongComponents ():
    def __init__ (self, directedg):
        self.__directedg      = directedg
        self.__vertexToColour = {}
        self.__vertexToSCC    = {}
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
                        
                        