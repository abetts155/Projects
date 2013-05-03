from DirectedGraphs import DirectedGraph
from Vertices import BasicBlock, Vertex, HeaderVertex, SuperBlock, PathInformationVertex
from Edges import SuperBlockControlFlowEdge, SuperBlockLoopEdge
from Trees import Dominators, DominanceFrontiers, DepthFirstSearch
from Utils import enum
import Debug, UDrawGraph

nextVertexID = 0
nextEdgeID   = 0

class SuperBlockGraph (DirectedGraph):
    
    def __init__ (self, icfg, lnt):
        DirectedGraph.__init__(self)
        self.__icfg  = icfg
        self._name   = icfg.getName()
        self.__lnt   = lnt
        self.__pathg = PathInformationGraph()
        self.__headerToSuperBlockSubgraph = {}
        self.__headerToRootSuperBlock = {}
        self.__headerToPathVertices   = {}
        self.__basicBlockToSuperBlock = {}
        self.__rootSuperv             = None
        self.__monitoredBasicBlocks   = set([])
        self.__monitoredEdges         = {}
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, HeaderVertex):
                    headerID = v.getHeaderID()
                    Debug.debugMessage("Analysing header %d" % headerID, 1)
                    forwardICFG        = lnt.induceSubgraph(v)
                    UDrawGraph.makeUdrawFile(forwardICFG, "Header%d" % headerID)
                    predomTree             = Dominators(forwardICFG, forwardICFG.getEntryID())
                    reverseICFG            = forwardICFG.getReverseCFG()
                    postdomTree            = Dominators(reverseICFG, reverseICFG.getEntryID())
                    postDF                 = DominanceFrontiers(reverseICFG, postdomTree)    
                    dominatorg             = DominatorGraph(predomTree, postdomTree)  
                    sccs                   = StrongComponents(dominatorg)
                    branchIpostSuperBlocks = {}
                    self.__headerToSuperBlockSubgraph[headerID] = self.__addSuperBlocks(lnt, forwardICFG, postdomTree, sccs, branchIpostSuperBlocks, headerID)
                    self.__headerToRootSuperBlock[headerID]     = self.__addEdges(lnt, forwardICFG, predomTree, postdomTree, postDF, branchIpostSuperBlocks, headerID)
                    self.__headerToRootSuperBlock[headerID].setLoopHeader(headerID)
                    self.__computePathRelationSubgraph(self.__headerToSuperBlockSubgraph[headerID], self.__headerToRootSuperBlock[headerID], forwardICFG, headerID)
                    if v.getVertexID() == lnt.getRootID():
                        self.__rootSuperv = self.__headerToRootSuperBlock[headerID]
                        
    def __addSuperBlocks (self, lnt, forwardICFG, postdomTree, sccs, branchIpostSuperBlocks, headerID):
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
                if isinstance(v, BasicBlock):
                    superv.addBasicBlock(vertexID)
                else:
                    superv.addEdge(v.edge)
                    self.__monitoredEdges[v.edge] = superv
                self.__basicBlockToSuperBlock[vertexID] = superv
                if v.numberOfSuccessors() > 1:
                    ipostID = postdomTree.getVertex(vertexID).getParentID()
                    if v.hasSuccessor(ipostID):
                        nextVertexID += 1
                        superVertexID = nextVertexID
                        superv        = SuperBlock(superVertexID) 
                        superv.addEdge((vertexID, ipostID))
                        self.__monitoredEdges[(vertexID, ipostID)] = superv
                        self.vertices[superVertexID]     = superv
                        subgraph.vertices[superVertexID] = superv
                        branchIpostSuperBlocks[vertexID] = superv
        return subgraph
                
    def __addEdges (self, lnt, forwardICFG, predomTree, postdomTree, postDF, branchIpostSuperBlocks, headerID):
        rootSuperv = self.__basicBlockToSuperBlock[headerID]
        dfs        = DepthFirstSearch(forwardICFG, forwardICFG.getEntryID())
        branches   = set([])
        # Process vertices in topological order
        for vertexID in dfs.getPostorder():
            v = forwardICFG.getVertex(vertexID)
            # Found a nested loop header
           # if not v.isDummy() and lnt.isLoopHeader(vertexID) and vertexID != headerID:
           #     sourcev      = self.__basicBlockToSuperBlock[ve rtexID]
           #     destinationv = self.__headerToRootSuperBlock[vertexID]
           #     self.__addEdge(sourcev, destinationv, headerID, SuperBlockLoopEdge)
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
                        destinationv = branchIpostSuperBlocks[vertexID]
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
        
    def __computePathRelationSubgraph (self, subgraph, rootv, forwardICFG, headerID):
        supervToSupervs = {}
        for superv in subgraph:
            supervToSupervs[superv] = set([])
        dfs = DepthFirstSearch(subgraph, rootv.getVertexID())
        for vertexID in dfs.getPostorder():
            superv = self.getVertex(vertexID)
            if vertexID == rootv.getVertexID():
                self.__addPathRelationEdges(rootv, forwardICFG, headerID, supervToSupervs)
            else:
                alive = True
                for bbID in superv.getBasicBlockIDs():
                    bb = self.__icfg.getVertex(bbID)
                    if self.__lnt.isLoopHeader(bbID) or bb.numberOfSuccessors() > 1:
                        alive = False
                if alive: 
                    supervToSupervs[superv].add(superv)
                else:
                    for succID in superv.getSuccessorIDs():
                        succv = self.getVertex(succID)
                        supervToSupervs[superv].update(supervToSupervs[succv])
                        
    def __addPathRelationEdges (self, rootv, forwardICFG, headerID, supervToSupervs):
        succPartition             = rootv.getBranchPartitions()
        dfs                       = DepthFirstSearch(forwardICFG, forwardICFG.getEntryID())
        newVertexID               = 0
        previousPartitionVertices = []
        for vertexID in reversed(dfs.getPostorder()):
            if self.__lnt.isLoopHeader(vertexID) and vertexID != headerID:
                previousPartitionVertices.extend(self.__headerToPathVertices[vertexID])
            if vertexID in succPartition:
                thisPartitionVertices = []
                for supere in succPartition[vertexID]:
                    succv = self.getVertex(supere.getVertexID())
                    for superv in supervToSupervs[succv]:
                        Debug.debugMessage("%d is a monitored super block in %s" % (superv.getVertexID(), self.getName()), 1)
                        if superv.getBasicBlockIDs():
                            self.__monitoredBasicBlocks.add(superv.getRepresentativeID())
                        newVertexID = self.__pathg.getNextVertexID()
                        newv        = PathInformationVertex(newVertexID, set([superv.getVertexID()]))
                        self.__pathg.vertices[newVertexID] = newv
                        self.__pathg.superBlockToPathVertex[superv] = newv
                        if thisPartitionVertices and headerID != self.__icfg.getEntryID():
                            for oldv in thisPartitionVertices:
                                parentID = self.__pathg.getNextVertexID()
                                theSet   = set([])
                                theSet.update(oldv.theSet)
                                theSet.update(newv.theSet)
                                parentv  = PathInformationVertex(parentID, theSet)
                                self.__pathg.vertices[parentID] = parentv
                                self.__pathg.addEdge(parentID, oldv.getVertexID())
                                self.__pathg.addEdge(parentID, newv.getVertexID())
                        thisPartitionVertices.append(newv)
                for oldv in previousPartitionVertices:
                    for newv in thisPartitionVertices:
                        parentID = self.__pathg.getNextVertexID()
                        theSet   = set([])
                        theSet.update(oldv.theSet)
                        theSet.update(newv.theSet)
                        parentv  = PathInformationVertex(parentID, theSet)
                        self.__pathg.vertices[parentID] = parentv
                        self.__pathg.addEdge(parentID, oldv.getVertexID())
                        self.__pathg.addEdge(parentID, newv.getVertexID())
                previousPartitionVertices.extend(thisPartitionVertices)
        self.__headerToPathVertices[headerID] = previousPartitionVertices
        
    def computePathInformation (self, superBlockToRuns):
        for superv, runs in superBlockToRuns.iteritems():
            if superv in self.__pathg.superBlockToPathVertex:
                pathv = self.__pathg.superBlockToPathVertex[superv]
                pathv.runs.update(runs)
        UDrawGraph.makeUdrawFile(self.__pathg, "%s.%s" % (self.getName(), "pathg"))
                
    def isMonitoredBasicBlock (self, basicBlockID):
        return basicBlockID in self.__monitoredBasicBlocks
   
    def getMonitoredBasicBlockSuperBlock (self, basicBlockID):
        return self.__basicBlockToSuperBlock[basicBlockID]
    
    def isMonitoredEdge (self, predID, succID):
        return (predID, succID) in self.__monitoredEdges
    
    def getMonitoredEdgeSuperBlock (self, predID, succID):
        assert (predID, succID) in self.__monitoredEdges, "(%d, %d) is not a monitored CFG edge" % (predID, succID)
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
        self.superBlockToPathVertex = {}
    
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
                        
                        