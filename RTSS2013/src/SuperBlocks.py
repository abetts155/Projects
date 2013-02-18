from DirectedGraphs import DirectedGraph
from Vertices import Vertex, HeaderVertex, SuperBlock
from Trees import Dominators, DominanceFrontiers
import Debug

class SuperBlockGraph (DirectedGraph):
    def __init__ (self, icfg, lnt):
        DirectedGraph.__init__(self)
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, HeaderVertex):
                    headerID = v.getHeaderID()
                    Debug.debugMessage("Analysing header %d" % headerID, 1)
                    forwardICFG = lnt.induceSubgraph(v)
                    predomTree  = Dominators(forwardICFG, forwardICFG.getEntryID())
                    reverseICFG = forwardICFG.getReverseCFG()
                    postdomTree = Dominators(reverseICFG, reverseICFG.getEntryID())
                    preDF       = DominanceFrontiers(forwardICFG, predomTree)
                    postDF      = DominanceFrontiers(reverseICFG, postdomTree)    
                    dominatorg  = DominatorGraph(predomTree, postdomTree)  
                    sccs        = StrongComponents(dominatorg)
                    self.__addSuperBlocks(forwardICFG, sccs)
    
    def __addSuperBlocks (self, forwardICFG, sccs):
        sccIDToVertex = {}
        for sccID in xrange(1, sccs.numberOfSCCs()+1):
            vertexID                = self.getNextVertexID()
            superv                  = SuperBlock(vertexID)
            self.vertices[vertexID] = superv
            sccIDToVertex[sccID]    = superv
        for v in forwardICFG:
            vertexID = v.getVertexID()
            sccID    = sccs.getSCCID(vertexID)
            superv   = sccIDToVertex[sccID]
            superv.addBasicBlock(vertexID)
        
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
                if not v.hasPredecessor(parentID):
                    self.addEdge(v.getParentID(), vertexID)
                
def enum(*sequential, **named):
    enums = dict(zip(sequential, range(len(sequential))), **named)
    reverse = dict((value, key) for key, value in enums.iteritems())
    enums['reverse_mapping'] = reverse
    return type('Enum', (), enums)

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
        for vertexID in vertexList:
            if self.__vertexToColour[vertexID] == Colors.BLACK:
                self.__sccCounter += 1
                # The vertex v is from the forward directed graph.
                # Need to get the vertex from the reverse graph instead
                self.__visit2(self.__reverseg .getVertex(vertexID))
    
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
            Debug.debugMessage("Vertex %d is in SCC %d" % (vertexID, self.__sccCounter), 1)
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
                        
                        