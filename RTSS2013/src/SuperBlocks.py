from DirectedGraphs import DirectedGraph
from Vertices import Vertex, HeaderVertex, SuperBlock
from Edges import SuperBlockControlFlowEdge, SuperBlockLoopEdge
from Trees import Dominators, DominanceFrontiers, DepthFirstSearch
from Utils import enum
import Debug

class PATHRELATION:
    MUTUAL_EXCLUSION = "MUTUAL_EXCLUSION"
    MUTUAL_INCLUSION = "MUTUAL_INCLUSION"
    PRECEDENCE       = "PRECEDENCE"
    ALTERNATION      = "ALTERNATION"

class LOOPRELATION:
    LESS_THAN    = "<"
    EQUAL        = "=="
    GREATER_THAN = ">"

class SuperBlockGraph (DirectedGraph):
    edgeID = 1
    
    def __init__ (self, icfg, lnt):
        DirectedGraph.__init__(self)
        self._name = icfg.getName()
        self.__headerToSuperBlockSubgraph = {}
        self.__headerToRootSuperBlock = {}
        self.__basicBlockToSuperBlock = {}
        self.__truePathRelationEdges  = set([])
        self.__falsePathRelationEdges = set([])
        self.__rootSuperv             = None
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
        for superv1, superv2, pathrelation in self.__truePathRelationEdges:
            Debug.debugMessage("super(%d), super(%d), %s" % (superv1.getVertexID(), superv2.getVertexID(), pathrelation), 10)
                    
    def __addSuperBlocks (self, lnt, forwardICFG, postdomTree, sccs, emptySuperBlocks, headerID):
        subgraph      = DirectedGraph()
        sccIDToVertex = {}
        for sccID in xrange(1, sccs.numberOfSCCs()+1):
            superVertexID                = self.getNextVertexID()
            superv                       = SuperBlock(superVertexID)
            self.vertices[superVertexID] = superv
            sccIDToVertex[sccID]         = superv
            subgraph.vertices[superVertexID] = superv
        for v in forwardICFG:
            vertexID = v.getVertexID()     
            sccID    = sccs.getSCCID(vertexID)
            superv   = sccIDToVertex[sccID]
            superv.addBasicBlock(vertexID)
            self.__basicBlockToSuperBlock[vertexID] = superv
            if v.numberOfSuccessors() > 1:
                ipostID = postdomTree.getVertex(vertexID).getParentID()
                if v.hasSuccessor(ipostID):
                    superVertexID = self.getNextVertexID()
                    superv        = SuperBlock(superVertexID)
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
  
        reachableSuperBlocks = {}
        superBlocksToAnalyse = set([])
        for vertexID in dfs.getPostorder():
            v = forwardICFG.getVertex(vertexID)
            reachableSuperBlocks[vertexID] = set([])
            superv = self.__basicBlockToSuperBlock[vertexID]
            for succID in v.getSuccessorIDs():
                reachableSuperBlocks[vertexID].update(reachableSuperBlocks[succID])
            if not branches.intersection(superv.getBasicBlockIDs()):
                superBlocksToAnalyse.add(superv)
                reachableSuperBlocks[vertexID].add(superv)
        
        # Add mutual exclusion and mutual inclusion
        pathRelationEdges = set([])  
        for superv1 in superBlocksToAnalyse:
            vertexID = superv1.getRepresentativeID()
            for superv2 in reachableSuperBlocks[vertexID]:
                if superv1 != superv2:
                    pathRelationEdges.add((superv1, superv2, PATHRELATION.MUTUAL_INCLUSION))
                    pathRelationEdges.add((superv1, superv2, PATHRELATION.MUTUAL_EXCLUSION))
                    
        # Add precedence and alternation
        for branchID in branches:
            superv1 = self.__basicBlockToSuperBlock[branchID]
            for superEdges in superv1.getBranchPartitions().values():
                for supere1 in superEdges:
                    for supere2 in superEdges:
                        superv1 = self.getVertex(supere1.getVertexID())
                        superv2 = self.getVertex(supere2.getVertexID())
                        if superv1 != superv2:
                            pathRelationEdges.add((superv1, superv2, PATHRELATION.ALTERNATION))
                            pathRelationEdges.add((superv1, superv2, PATHRELATION.PRECEDENCE))
                            
        self.__truePathRelationEdges.update(pathRelationEdges)
        return rootSuperv
    
    def __addEdge (self, sourcev, destinationv, branchID, edgeType=SuperBlockControlFlowEdge):
        succe = edgeType(destinationv.getVertexID(), branchID)
        prede = edgeType(sourcev.getVertexID(), branchID)
        sourcev.addSuccessorEdge(succe)
        destinationv.addPredecessorEdge(prede)
        succe.setEdgeID(SuperBlockGraph.edgeID)
        prede.setEdgeID(SuperBlockGraph.edgeID)
        SuperBlockGraph.edgeID += 1
    
    def getSuperBlock (self, basicBlockID):
        assert basicBlockID in self.__basicBlockToSuperBlock, "Unable to find basic block %d in a super block" % basicBlockID
        # Handle the case where the basic block is a loop header because it will always appear in 2 super blocks
        # and we want the one representing the header, not the abstract vertex
        if basicBlockID in self.__headerToRootSuperBlock:
            return self.__headerToRootSuperBlock[basicBlockID]
        return self.__basicBlockToSuperBlock[basicBlockID]
    
    def falsify (self, pathTuple):
        assert pathTuple in self.__truePathRelationEdges, "Unable to find (%s, %s, %s) in true path relation edges" \
        % (pathTuple[0].getVertexID(), pathTuple[1].getVertexID(), pathTuple[2])
        Debug.debugMessage("FALSIFYING (%s, %s, %s)" % (pathTuple[0].getVertexID(), pathTuple[1].getVertexID(), pathTuple[2]), 10)
        self.__truePathRelationEdges.remove(pathTuple)
        self.__falsePathRelationEdges.add(pathTuple)

    def getTruePathRelationEdges (self):
        return self.__truePathRelationEdges

    def getFalsePathRelationEdges (self):
        return self.__falsePathRelationEdges
    
    def getSuperBlockRegion (self, headerID):
        assert headerID in self.__headerToSuperBlockSubgraph, "Unable to find super block CFG portion for header %d" % headerID
        return self.__headerToSuperBlockSubgraph[headerID]
    
    def getRootSuperBlock (self):
        assert self.__rootSuperv, "Root super block has not been set"
        return self.__rootSuperv
    
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
                        
                        