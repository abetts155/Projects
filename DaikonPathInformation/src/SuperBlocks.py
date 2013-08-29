from DirectedGraphs import DirectedGraph
from Vertices import BasicBlock, Vertex, HeaderVertex, SuperBlock, SuperBlockPartition
from Edges import SuperBlockControlFlowEdge
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
        self.__pathg = SuperBlockPathInformationGraph(self._name)
        self.__headerToSuperBlockSubgraph = {}
        self.__headerToRootSuperBlock     = {}
        self.__headerToAliveSuperBlocks   = {}
        self.__basicBlockToSuperBlock     = {}
        self.__rootSuperv                 = None
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
                    self.__headerToSuperBlockSubgraph[headerID], vToSuperv = self.__addSuperBlocks(lnt, forwardICFG, postdomTree, sccs, branchIpostSuperBlocks, headerID)
                    self.__headerToRootSuperBlock[headerID]                = self.__addEdges(lnt, forwardICFG, predomTree, postdomTree, postDF, branchIpostSuperBlocks, headerID, vToSuperv)
                    self.__headerToRootSuperBlock[headerID].setLoopHeader(headerID)
                    if v.getVertexID() == lnt.getRootID():
                        self.__rootSuperv = self.__headerToRootSuperBlock[headerID]
                    self.__computeSuperBlockPartitions(self.__headerToSuperBlockSubgraph[headerID], self.__headerToRootSuperBlock[headerID], forwardICFG, v)
                        
    def __addSuperBlocks (self, lnt, forwardICFG, postdomTree, sccs, branchIpostSuperBlocks, headerID):
        global nextVertexID 
        subgraph      = DirectedGraph()
        sccIDToVertex = {}
        vToSuperv     = {}
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
                vToSuperv[vertexID] = superv
                if isinstance(v, BasicBlock):
                    self.__basicBlockToSuperBlock[vertexID] = superv
                    superv.addBasicBlock(vertexID)
                    if not lnt.isLoopHeader(vertexID):
                        superv.setRepresentativeID(vertexID)
                else:
                    superv.addEdge(v.edge)
                if v.numberOfSuccessors() > 1:
                    ipostID = postdomTree.getVertex(vertexID).getParentID()
                    if v.hasSuccessor(ipostID):
                        nextVertexID += 1
                        superVertexID = nextVertexID
                        superv        = SuperBlock(superVertexID) 
                        superv.addEdge((vertexID, ipostID))
                        self.vertices[superVertexID]     = superv
                        subgraph.vertices[superVertexID] = superv
                        branchIpostSuperBlocks[vertexID] = superv
        return subgraph, vToSuperv
                
    def __addEdges (self, lnt, forwardICFG, predomTree, postdomTree, postDF, branchIpostSuperBlocks, headerID, vToSuperv):
        rootSuperv = self.__basicBlockToSuperBlock[headerID]
        dfs        = DepthFirstSearch(forwardICFG, forwardICFG.getEntryID())
        branches   = set([])
        # Process vertices in topological order
        for vertexID in dfs.getPostorder():
            v = forwardICFG.getVertex(vertexID)
            # Found a branch
            if v.numberOfSuccessors() > 1:
                branches.add(vertexID)
                sourcev =  vToSuperv[vertexID]                
                for succID in v.getSuccessorIDs():
                    if not postdomTree.isAncestor(succID, vertexID):
                        destinationv = vToSuperv[succID]
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
        
    def __addDummyExitToSubgraph (self, subgraph):
        noSuccs = []
        for superv in subgraph:
            if superv.numberOfSuccessors() == 0:
                noSuccs.append(superv)
        if len(noSuccs) > 1:
            nextVertexID = subgraph.getNextVertexID()
            newSuperv    = SuperBlock(nextVertexID)
            newSuperv.setDummy()
            subgraph.vertices[nextVertexID] = newSuperv
            for predSuperv in noSuccs:
                subgraph.addEdge(predSuperv.getVertexID(), nextVertexID)
            return True, nextVertexID
        else:
            assert len(noSuccs) == 1
            return False, noSuccs[0].getVertexID()
        
    def __removeDummyExitFromSubgraph (self, subgraph, exitID):
        exitv = subgraph.getVertex(exitID)
        for predID in exitv.getPredecessorIDs():
            subgraph.removeEdge(predID, exitID)
        subgraph.removeVertex(exitID)
        
    def __computeSuperBlockPartitions (self, subgraph, rootv, forwardICFG, headerv):
        dummy, exitID   = self.__addDummyExitToSubgraph(subgraph)
        predomTree      = Dominators(subgraph, rootv.getVertexID())
        reverseSubgraph = subgraph.getReverseGraph()
        postdomTree     = Dominators(reverseSubgraph, exitID)
        dominatorg      = DominatorGraph(predomTree, postdomTree)
        if dummy:
            self.__removeDummyExitFromSubgraph(subgraph, exitID)
        # Determine which super blocks need to be in the path information graph
        for dominatorv in dominatorg:
            # Only add super blocks which do not pre-dominate or post-dominate other super blocks 
            # and which is not the root super block in this region
            if dominatorv.numberOfSuccessors() == 0 and dominatorv.numberOfPredecessors() > 0:
                supervID  = dominatorv.getVertexID()
                superv    = subgraph.getVertex(supervID)
                isAcyclic = headerv.getVertexID() == self.__lnt.getRootID()
                edges     = set([])
                # Find out which CFG edges we need to monitor during trace parsing to falsify conjectures
                if superv.getBasicBlockIDs() and superv.hasRepresentativeID():
                    repID  = superv.getRepresentativeID()
                    repv   = self.__icfg.getVertex(repID)
                    for succID in repv.getSuccessorIDs():
                        edges.add((repID, succID))
                else:
                    edges = superv.getEdges()
                assert edges, "Unable to find suitable CFG edges for super block %d" % superv.getVertexID()
                pathv = SuperBlockPartition(supervID, isAcyclic, edges) 
                self.__pathg.addVertex(pathv, isAcyclic)
        # Compute reachability of alive super blocks to other super blocks
        aliveSuperBlocks = {}
        for superv in subgraph:
            aliveSuperBlocks[superv] = set([])
        dfs              = DepthFirstSearch(forwardICFG, forwardICFG.getEntryID())
        branchPartitions = rootv.getBranchPartitions()
        branchToAliveSuperBlocks = {}
        prevRootBranches = set([])
        for vertexID in reversed(dfs.getPostorder()):
            # Does this basic block belong to the root super block?
            if vertexID in branchPartitions:
                succIDs = []
                for supere in branchPartitions[vertexID]:
                    succIDs.append(supere.getVertexID())
                # Topologically sort the SB-CFG with the edges emanating from the root super block created for that specific branch
                dfs2 = DepthFirstSearch(subgraph, rootv.getVertexID(), succIDs)
                aliveInThisPartition = set([])
                for supervID in reversed(dfs2.getPostorder()):
                    superv = subgraph.getVertex(supervID)     
                    # Are we monitoring this super block?               
                    if self.__pathg.hasVertex(supervID):
                        # If so, add edges from all alive super blocks which can reach this super block
                        for prede in superv.getPredecessorEdges(): 
                            predv = subgraph.getVertex(prede.getVertexID())
                            for pathv in aliveSuperBlocks[predv]:
                                self.__pathg.addEdge(pathv.getVertexID(), superv.getVertexID())
                        # The super block is alive
                        aliveSuperBlocks[superv].add(superv)
                        aliveInThisPartition.add(superv)
                    # See if there are any collapsed inner loops in the super block
                    prevHeaders = set([])
                    for bbID in superv.getBasicBlockIDs():
                        if self.__lnt.isLoopHeader(bbID) and bbID != headerv.getHeaderID():
                            aliveSuperBlocks[superv].update(self.__headerToAliveSuperBlocks[bbID])
                            if prevHeaders:
                                for headerID in prevHeaders:
                                    for pathv1 in self.__headerToAliveSuperBlocks[headerID]:
                                        for pathv2 in self.__headerToAliveSuperBlocks[bbID]:
                                            self.__pathg.addEdge(pathv1.getVertexID(), pathv2.getVertexID())
                            prevHeaders.add(bbID)
                                
                    for prede in superv.getPredecessorEdges():
                        predv = subgraph.getVertex(prede.getVertexID())
                        aliveSuperBlocks[superv].update(aliveSuperBlocks[predv])
                branchToAliveSuperBlocks[vertexID] = aliveInThisPartition
                # Were there previous partitions?
                if prevRootBranches:
                    for branchID in prevRootBranches:
                        for pathv1 in branchToAliveSuperBlocks[branchID]:
                            for pathv2 in branchToAliveSuperBlocks[vertexID]:
                                self.__pathg.addEdge(pathv1.getVertexID(), pathv2.getVertexID())
                prevRootBranches.add(vertexID)
        
        self.__headerToAliveSuperBlocks[headerv.getHeaderID()] = set([])
        for aliveInPartition in branchToAliveSuperBlocks.values():
            self.__headerToAliveSuperBlocks[headerv.getHeaderID()].update(aliveInPartition)        
            
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
    
    def getSuperBlockPathInformationGraph (self):
        return self.__pathg
    
class SuperBlockPathInformationGraph (DirectedGraph):
    def __init__ (self, name):
        DirectedGraph.__init__(self)
        self._name            = name
        self.__monitoredEdges = {}
        
    def addVertex (self, partitionv, isAcyclic):
        vertexID = partitionv.getVertexID()
        assert vertexID not in self.vertices
        self.vertices[vertexID] = partitionv
        for edge in partitionv.getEdges():
            self.__monitoredEdges[edge] = partitionv
    
    def isMonitoredEdge (self, predID, succID):
        return (predID, succID) in self.__monitoredEdges
    
    def getMonitoredEdgeSuperBlock (self, predID, succID):
        assert (predID, succID) in self.__monitoredEdges, "(%d, %d) is not a monitored CFG edge" % (predID, succID)
        return self.__monitoredEdges[(predID, succID)]
    
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
        
                        
                        