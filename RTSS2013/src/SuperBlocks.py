from DirectedGraphs import DirectedGraph
from Vertices import Vertex, HeaderVertex, SuperBlock
from Edges import SuperBlockControlFlowEdge
from Trees import Dominators, DominanceFrontiers
from Utils import enum
import Debug

class SuperBlockGraph (DirectedGraph):
    def __init__ (self, icfg, lnt):
        DirectedGraph.__init__(self)
        self.__headerToRootSuperBlock = {}
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
                    disconnected       = []
                    vertexToSuperBlock = {}
                    self.__addSuperBlocks(lnt, forwardICFG, postdomTree, sccs, disconnected, vertexToSuperBlock, headerID)
                    self.__addEdges(forwardICFG, predomTree, postdomTree, postDF, disconnected, vertexToSuperBlock)
                    assert not disconnected, "Added empty super blocks in loop region with header %d but some remain disconnected" % headerID
                    self.__headerToRootSuperBlock[headerID] = self.__findRootSuperBlock(headerID)
    
    def __findRootSuperBlock (self, headerID):
        for v in self:
            if v.containsBasicBlock(headerID):
                return v 
        assert False, "Unable to find super block with header %d" % headerID
                    
    def __addSuperBlocks (self, lnt, forwardICFG, postdomTree, sccs, disconnected, vertexToSuperBlock, headerID):
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
            vertexToSuperBlock[vertexID] = superv
            if v.numberOfSuccessors() > 1:
                ipostID = postdomTree.getVertex(vertexID).getParentID()
                if v.hasSuccessor(ipostID):
                    superVertexID = self.getNextVertexID()
                    superv        = SuperBlock(superVertexID)
                    self.vertices[superVertexID] = superv
                    disconnected.append(superv)
        # Merge super blocks from nested loops
        for v in forwardICFG:
            vertexID = v.getVertexID()     
            if lnt.isLoopHeader(vertexID) and vertexID != headerID:
                superv     = self.__headerToRootSuperBlock[vertexID]
                redundantv = vertexToSuperBlock[vertexID]
                superv.addBasicBlocks(redundantv.getBasicBlockIDs())
                for bbID in redundantv.getBasicBlockIDs():
                    vertexToSuperBlock[bbID] = superv
                self.removeVertex(redundantv.getVertexID())
                
    def __addEdges (self, forwardICFG, predomTree, postdomTree, postDF, disconnected, vertexToSuperBlock):
        for v in forwardICFG:
            vertexID = v.getVertexID()
            if v.numberOfSuccessors() > 1:
                sourcev = vertexToSuperBlock[vertexID]
                for succID in v.getSuccessorIDs():
                    if not postdomTree.isAncestor(succID, vertexID):
                        destinationv = vertexToSuperBlock[succID]
                        if not sourcev.hasSuccessor(destinationv.getVertexID()):
                            self.__addEdge(sourcev, destinationv, vertexID)
                    else:
                        destinationv = disconnected.pop()
                        self.__addEdge(sourcev, destinationv, vertexID)
            if v.numberOfPredecessors() > 1:
                ipreID = predomTree.getVertex(vertexID).getParentID()
                if postdomTree.getVertex(ipreID).getParentID() != vertexID:
                    destinationv = vertexToSuperBlock[vertexID]
                    for predID in v.getPredecessorIDs():
                        sourcev = vertexToSuperBlock[predID]
                        if not sourcev.hasSuccessor(destinationv.getVertexID()):
                            self.__addEdge(sourcev, destinationv, predID)    
                    if postDF.size(vertexID) > 1:
                        Debug.debugMessage("Acyclic IRREDUCIBLE merge %d found" % vertexID, 1)
                        destinationv.setUnstructuredMerge()
                    else:
                        Debug.debugMessage("Acyclic REDUCIBLE merge %d found" % vertexID, 1)
    
    def __addEdge (self, sourcev, destinationv, branchID):
        succe = SuperBlockControlFlowEdge(destinationv.getVertexID(), branchID)
        sourcev.addSuccessorEdge(succe)
        prede = SuperBlockControlFlowEdge(sourcev.getVertexID(), branchID)
        destinationv.addPredecessorEdge(prede)
    
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
                        
                        