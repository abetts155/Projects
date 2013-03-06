from DirectedGraphs import DirectedGraph
from Vertices import Vertex, HeaderVertex, SuperBlock, dummyVertexID
from Edges import SuperBlockControlFlowEdge, SuperBlockPathRelationEdge
from Trees import Dominators, DominanceFrontiers, DepthFirstSearch
from Utils import enum
import Debug

class SuperBlockGraph (DirectedGraph):
    def __init__ (self, icfg, lnt):
        DirectedGraph.__init__(self)
        self.__headerToRootSuperBlock = {}
        self.__basicBlockToSuperBlock = {}
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
                    self.__addSuperBlocks(lnt, forwardICFG, postdomTree, sccs, emptySuperBlocks, headerID)
                    self.__addEdges(forwardICFG, predomTree, postdomTree, postDF, emptySuperBlocks, headerID)
                    assert not emptySuperBlocks, "Added empty super blocks in loop region with header %d but some remain disconnected" % headerID
                    self.__headerToRootSuperBlock[headerID] = self.getSuperBlock(headerID)
                    
    def __addSuperBlocks (self, lnt, forwardICFG, postdomTree, sccs, emptySuperBlocks, headerID):
        sccIDToVertex = {}
        for sccID in xrange(1, sccs.numberOfSCCs()+1):
            superVertexID                = self.getNextVertexID()
            superv                       = SuperBlock(superVertexID)
            self.vertices[superVertexID] = superv
            sccIDToVertex[sccID]         = superv
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
                    emptySuperBlocks.append(superv)
        # Merge super blocks from nested loops
        for v in forwardICFG:
            vertexID = v.getVertexID()     
            if lnt.isLoopHeader(vertexID) and vertexID != headerID:
                superv     = self.__headerToRootSuperBlock[vertexID]
                redundantv = self.__basicBlockToSuperBlock[vertexID]
                superv.addBasicBlocks(redundantv.getBasicBlockIDs())
                for bbID in redundantv.getBasicBlockIDs():
                    self.__basicBlockToSuperBlock[bbID] = superv
                self.removeVertex(redundantv.getVertexID())
                
    def __addEdges (self, forwardICFG, predomTree, postdomTree, postDF, emptySuperBlocks, headerID):
        rootSuperv              = self.__basicBlockToSuperBlock[headerID]
        branchToLeafSuperBlocks = {}
        dfs                     = DepthFirstSearch(forwardICFG, forwardICFG.getEntryID())
        currentBranchID         = dummyVertexID
        # Process vertices in topological order
        for vertexID in reversed(dfs.getPostorder()):
            v = forwardICFG.getVertex(vertexID)
            if v.numberOfSuccessors() > 1:
                if vertexID in rootSuperv.getBasicBlockIDs():
                    currentBranchID = vertexID
                    branchToLeafSuperBlocks[currentBranchID] = set([])
                sourcev = self.__basicBlockToSuperBlock[vertexID]                
                for succID in v.getSuccessorIDs():
                    if not postdomTree.isAncestor(succID, vertexID):
                        destinationv = self.__basicBlockToSuperBlock[succID]
                        if not sourcev.hasSuccessor(destinationv.getVertexID()):
                            self.__addEdge(sourcev, destinationv, vertexID)
                    else:
                        destinationv = emptySuperBlocks.pop()
                        self.__addEdge(sourcev, destinationv, vertexID)
            elif v.numberOfPredecessors() == 1:
                destinationv = self.__basicBlockToSuperBlock[vertexID]
                assert currentBranchID != dummyVertexID, "Current branch in root super block not found"
                branchToLeafSuperBlocks[currentBranchID].add(destinationv)
          
        for i in range(len(branchToLeafSuperBlocks.keys())):
            branchID1 = branchToLeafSuperBlocks.keys()[i]
            for branchID2 in branchToLeafSuperBlocks.keys()[i+1:]:
                Debug.debugMessage("Analysing region (%d, %d)" % (branchID1, branchID2), 10)
                for sourcev in branchToLeafSuperBlocks[branchID1]:
                    for destinationv in branchToLeafSuperBlocks[branchID2]:
                        self.__addPathRelationEdgeEdge(sourcev, destinationv, SuperBlockPathRelationEdge.PathRelations.MUTUAL_EXCLUSION)
                        self.__addPathRelationEdgeEdge(sourcev, destinationv, SuperBlockPathRelationEdge.PathRelations.MUTUAL_INCLUSION)
    
    def __addEdge (self, sourcev, destinationv, branchID):
        succe = SuperBlockControlFlowEdge(destinationv.getVertexID(), branchID)
        prede = SuperBlockControlFlowEdge(sourcev.getVertexID(), branchID)
        sourcev.addSuccessorEdge(succe)
        destinationv.addPredecessorEdge(prede)
        
    def __addPathRelationEdgeEdge (self, sourcev, destinationv, relationType):
        e = SuperBlockPathRelationEdge(sourcev.getVertexID(), relationType)
        destinationv.addPathRelationEdge(e)
    
    def getSuperBlock (self, basicBlockID):
        assert basicBlockID in self.__basicBlockToSuperBlock, "Unable to find basic block %d in a super block" % basicBlockID
        return self.__basicBlockToSuperBlock[basicBlockID]
    
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
                        
                        