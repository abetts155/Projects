from DirectedGraphs import DirectedGraph
from Vertices import BasicBlock, Vertex, HeaderVertex, SuperBlock, SuperBlockPartition, SuperBlockUnion,\
    CFGEdge
from Edges import SuperBlockControlFlowEdge
from Trees import Dominators, DominanceFrontiers, DepthFirstSearch
from Utils import enum
import Debug, UDrawGraph, itertools
from sys import maxint

nextVertexID     = 0
nextEdgeID       = 0
exclusiveSetSize = maxint

class SuperBlockGraph (DirectedGraph):
    def __init__ (self, icfg, lnt):
        DirectedGraph.__init__(self)
        self.__icfg  = icfg
        self._name   = icfg.getName()
        self.__lnt   = lnt
        self.__pathg = SuperBlockPathInformationGraph(self._name)
        self.__headerToSuperBlockSubgraph = {}
        self.__headerToRootSuperBlock     = {}
        self.__headerToPartitionVertices  = {}
        self.__basicBlockToSuperBlock     = {}
        self.__rootSuperv                 = None
        self.__monitoredBasicBlocks       = set([])
        self.__monitoredEdges             = {}
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
        
    def __computeSuperBlockPartitions (self, subgraph, rootv, forwardICFG, headerv):
        supervToSupervs = {}
        for superv in subgraph:
            supervToSupervs[superv] = set([])
        dfs = DepthFirstSearch(subgraph, rootv.getVertexID())
        for vertexID in dfs.getPostorder():
            superv = self.getVertex(vertexID)
            if vertexID == rootv.getVertexID():
                self.__headerToPartitionVertices[headerv.getHeaderID()] = set([])
                self.__handleRootSuperBlock(supervToSupervs, rootv, forwardICFG, headerv)
            else:
                # Deduce which super blocks we should monitor during trace parsing
                alive = False
                if (superv.numberOfSuccessors() == 0 or superv.numberOfSuccessors() == 1) \
                and superv != rootv:
                    alive = True
                if alive: 
                    supervToSupervs[superv].add(superv)
                else:
                    for succID in superv.getSuccessorIDs():
                        succv = self.getVertex(succID)
                        supervToSupervs[superv].update(supervToSupervs[succv])
                        
    def __handleRootSuperBlock (self, supervToSupervs, rootv, forwardICFG, headerv):
        branchPartitions = rootv.getBranchPartitions()
        dfs = DepthFirstSearch(forwardICFG, forwardICFG.getEntryID())
        for vertexID in reversed(dfs.getPostorder()):
            v = forwardICFG.getVertex(vertexID)
            if self.__lnt.isLoopHeader(vertexID) \
            and vertexID != headerv.getHeaderID() \
            and not isinstance(v, CFGEdge) \
            and not v.isDummy():
                for partitionv in self.__headerToPartitionVertices[vertexID]:
                    self.__pathg.partitionOrder.append(partitionv)
            if vertexID in branchPartitions:
                nextVertexID = self.__pathg.getNextVertexID() 
                theSet       = set([])
                for supere in branchPartitions[vertexID]:
                    superv = self.getVertex(supere.getVertexID())
                    theSet.update(supervToSupervs[superv])
                partitionv   = SuperBlockPartition(nextVertexID, theSet)
                if headerv.getVertexID() == self.__lnt.getRootID():
                    partitionv.acyclicPartition = True
                self.__pathg.vertices[nextVertexID] = partitionv
                self.__headerToPartitionVertices[headerv.getHeaderID()].add(partitionv)
                self.__pathg.partitionOrder.append(partitionv)
        if headerv.getVertexID() == self.__lnt.getRootID():
            for partitionv in self.__pathg:
                for superv in partitionv.runs.keys():
                    if superv.getBasicBlockIDs():
                        self.__monitoredBasicBlocks.add(superv.getRepresentativeID())
                    else:
                        edges = superv.getEdges()
                        assert len(edges) == 1
                        edge = list(edges)[0]
                        self.__monitoredEdges[edge] = superv
          
    def computePathInformation (self, superBlockToRuns, allRuns):
        global exclusiveSetSize
        for superv, runs in superBlockToRuns.iteritems():
            partitionv = self.__pathg.getPathInformationVertex(superv)
            partitionv.runs[superv].update(runs)
            if partitionv.runs[superv] == allRuns:
                self.__pathg.alwaysSuperBlocks.add(superv)
                Debug.debugMessage("%s always executes" % superv.getVertexID(), 1)
                    
        # The size of subsets 
        setSize = min(len(self.__pathg.partitionOrder),exclusiveSetSize)
        for subsetSize in xrange(2, setSize+1):
            Debug.debugMessage("Generating subsets of size %d from set size of %d" % (subsetSize, setSize), 1)
            # Get every partition of size r
            theCombinations = set(itertools.combinations(self.__pathg.partitionOrder, subsetSize))
            Debug.debugMessage("There are %d combinations" % len(theCombinations))
            for partitionTuple in theCombinations:
                theSets = []
                for i in xrange(0, subsetSize):
                    theSets.append(partitionTuple[i].runs.keys())
                cartProducts = set(itertools.product(*theSets))
                Debug.debugMessage("There are %d cross products" % len(cartProducts))
                for cartProduct in cartProducts:
                    runs = allRuns
                    for superv in cartProduct:
                        if superv in superBlockToRuns:
                            runs = runs.intersection(superBlockToRuns[superv])
                    if not runs:
                        self.__pathg.exclusiveTuples.add(cartProduct)
                        print "%s is MUTUALLY EXCLUSIVE" % ', '.join(str(superv.getVertexID()) for superv in cartProduct)
                    nextVertexID = self.__pathg.getNextVertexID() 
                    partitionv   = SuperBlockUnion(nextVertexID, cartProduct, runs)
                    self.__pathg.vertices[nextVertexID] = partitionv
        UDrawGraph.makeUdrawFile(self.__pathg, "%s.%s" % (self.getName(), "partitiong"))
                
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
    
    def getSuperBlockPathInformationGraph (self):
        return self.__pathg
    
    def output (self):
        Debug.verboseMessage("...#basic blocks           = %d" % self.__icfg.numOfVertices())
        Debug.verboseMessage("...#edges                  = %d" % self.__icfg.numOfEdges())
        Debug.verboseMessage("...#super blocks           = %d" % self.numOfVertices())
        Debug.verboseMessage("...#monitored super blocks = %d" % (len(self.__monitoredBasicBlocks)+len(self.__monitoredEdges)))
    
class SuperBlockPathInformationGraph (DirectedGraph):
    def __init__ (self, name):
        DirectedGraph.__init__(self)
        self._name = name
        self.partitionOrder = []
        self.exclusiveTuples = set([])
        self.alwaysSuperBlocks = set([])
    
    def output (self):
        Debug.verboseMessage("%d super blocks always execute" % (len(self.alwaysSuperBlocks)))
        if self.alwaysSuperBlocks:
            count = 0
            for superv in self.alwaysSuperBlocks:
                if superv.getBasicBlockIDs():
                    count += 1
            Debug.verboseMessage("...%d of which are basic blocks" % (count))
            Debug.verboseMessage("...%d of which are edges" % (len(self.alwaysSuperBlocks) - count))
        Debug.verboseMessage("%d mutually exclusive tuples" % (len(self.exclusiveTuples)))
        
    def getPathInformationVertex (self, superv):
        for partitionv in self:
            if superv in partitionv.runs.keys():
                return partitionv
        assert False, "Unable to find partition vertex for super block %d" % superv.getVertexID()
    
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
                        
                        