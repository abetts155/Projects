from DirectedGraphs import DirectedGraph
from Vertices import CFGVertex, Vertex, HeaderVertex, SuperBlock
from Edges import SuperBlockControlFlowEdge
from Trees import Dominators, DominanceFrontiers, DepthFirstSearch
from CFGs import EnhancedCFG
from Utils import enum
import Debug

nextVertexID = 0
nextEdgeID   = 0

class SuperBlockGraph (DirectedGraph):
    def __init__ (self, cfg=None, lnt=None):
        DirectedGraph.__init__(self)
        if cfg and lnt:
            self.__cfg = cfg
            self._name = cfg.getName()
            self.__headerToSuperBlockSubgraph = {}
            self.__headerToSuperBlockRoot     = {}
            for level, vertices in lnt.levelIterator(True):
                for treev in vertices:
                    if isinstance(treev, HeaderVertex):
                        headerID = treev.getHeaderID()                    
                        Debug.debugMessage("Analysing header %d" % headerID, 1)
                        forwardCFG         = lnt.induceSubgraph(treev)
                        enhancedCFG        = EnhancedCFG(forwardCFG)
                        predomTree         = Dominators(enhancedCFG, enhancedCFG.getEntryID())
                        reverseEnhancedCFG = enhancedCFG.getReverseGraph()
                        postdomTree        = Dominators(reverseEnhancedCFG, reverseEnhancedCFG.getEntryID())
                        postDF             = DominanceFrontiers(reverseEnhancedCFG, postdomTree)   
                        dominatorg         = DominatorGraph(predomTree, postdomTree)
                        sccs               = StrongComponents(dominatorg)      
                        self.__headerToSuperBlockSubgraph[headerID], vToSuperv = self.__addSuperBlocks(lnt, enhancedCFG, postdomTree, sccs, headerID)     
                        self.__headerToSuperBlockRoot[headerID] = self.__addEdges(lnt, enhancedCFG, predomTree, postdomTree, postDF, headerID, vToSuperv)   
    
            for superv in self:
                if superv.getBasicBlockIDs():
                    for vertexID in superv.getBasicBlockIDs():
                        if not lnt.isLoopHeader(vertexID):
                            superv.setRepresentativeID(vertexID)
                        elif len(superv.getBasicBlockIDs()) == 1:
                            superv.setRepresentativeID(vertexID)
                        
    def __addSuperBlocks (self, lnt, forwardCFG, postdomTree, sccs, headerID):
        global nextVertexID 
        subgraph      = SuperBlockGraph()
        sccIDToVertex = {}
        vToSuperv     = {}
        for sccID in xrange(1, sccs.numberOfSCCs()+1):
            nextVertexID += 1
            superVertexID                = nextVertexID
            superv                       = SuperBlock(superVertexID)
            self.vertices[superVertexID] = superv
            sccIDToVertex[sccID]         = superv
            subgraph.vertices[superVertexID] = superv
        for v in forwardCFG:
            if not v.isDummy():
                vertexID = v.getVertexID()  
                sccID    = sccs.getSCCID(vertexID)
                superv   = sccIDToVertex[sccID]
                vToSuperv[vertexID] = superv
                if isinstance(v, CFGVertex):
                    if not lnt.isLoopHeader(vertexID) or vertexID == headerID:
                        superv.addBasicBlock(vertexID)
                else:
                    predv = forwardCFG.getVertex(v.edge[0])
                    succv = forwardCFG.getVertex(v.edge[1])
                    if not predv.isDummy() and not succv.isDummy():
                        superv.addEdge(v.edge)
        return subgraph, vToSuperv
                
    def __addEdges (self, lnt, forwardCFG, predomTree, postdomTree, postDF, headerID, vToSuperv):
        rootSuperv = vToSuperv[headerID]
        dfs        = DepthFirstSearch(forwardCFG, forwardCFG.getEntryID())
        branches   = set([])
        # Process vertices in topological order
        for vertexID in dfs.getPostorder():
            v = forwardCFG.getVertex(vertexID)
            # Found a branch
            if v.numberOfSuccessors() > 1:
                branches.add(vertexID)
                sourcev = vToSuperv[vertexID]                
                for succID in v.getSuccessorIDs():
                    if not postdomTree.isAncestor(succID, vertexID):
                        destinationv = vToSuperv[succID]
                        if not sourcev.hasSuccessor(destinationv.getVertexID()):
                            self.__addEdge(sourcev, destinationv, vertexID)
            # Found a merge
            if v.numberOfPredecessors() > 1:
                ipreID = predomTree.getImmediateDominator(vertexID)
                if postdomTree.getImmediateDominator(ipreID) != vertexID:
                    destinationv = vToSuperv[vertexID]
                    if postDF.size(vertexID) > 1:
                        destinationv.setUnstructuredMerge()
                    for predID in v.getPredecessorIDs():
                        sourcev = vToSuperv[predID]
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
        
    def getSubregion (self, headerID):
        assert headerID in self.__headerToSuperBlockSubgraph, "Unable to find super block region for header %d" % headerID
        return self.__headerToSuperBlockSubgraph[headerID]
    
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
        
                        
                        