import CFGs, Vertices, Debug, Trees
import copy

class ICFG (CFGs.CFG):
    def __init__ (self, cfg):
        Debug.debugMessage(cfg, 10)
        CFGs.CFG.__init__(self)
        self.__branchDivergentEdges = []
        self.setName(cfg.getName())
        for bb in cfg:
            bbID = bb.getVertexID()
            self.vertices[bbID] = copy.deepcopy(bb)
        self.__addIpoints()
    
    def __addIpoints (self):
        for bb in self:
            address, instr = bb.getFirstInstruction()
            vertexID = self.getNextVertexID ()
            ipoint = Vertices.Ipoint(vertexID, int(address, 16))
            self.vertices[vertexID] = ipoint
            Debug.debugMessage("Adding Ipoint %d with ID %s" % (vertexID, address), 4)
            self.__linkIpoint(bb, ipoint)
            
    def __linkIpoint (self, bb, ipoint):
        for predID in bb.getPredecessorIDs():
            predv = self.getVertex(predID)
            self.addEdge(predID, ipoint.getVertexID())
            predv.removeSuccessor(bb.getVertexID())
            bb.removePredecessor(predID)
        self.addEdge(ipoint.getVertexID(), bb.getVertexID())
        
    def isIpoint (self, vertexID):
        v = self.getVertex(vertexID)
        return isinstance(v, Vertices.Ipoint)
    
    def getIpoint (self, vertexID):
        v = self.getVertex(vertexID)
        assert isinstance(v, Vertices.Ipoint), "Vertex %d is not an Ipoint" % vertexID
        return v
    
    def isBranchDivergentEdge (self, predID, succID):
        return (predID, succID) in self.__branchDivergentEdges
    
    def addBranchDivergenceEdges (self, lnt):
        # Look for forward branches
        forwardBranches = []
        for v in self:
            vertexID = v.getVertexID()
            if v.numberOfSuccessors () > 1:
                if not lnt.isLoopHeader(vertexID):
                    backEdge = False
                    for succID in v.getSuccessorIDs():
                        if lnt.isLoopBackEdge(vertexID, succID):
                            backEdge = True
                    if not backEdge:
                        forwardBranches.append(vertexID)
                else:
                    forLoop  = False
                    loopBody = lnt.getLoopBody(vertexID)
                    for succID in v.getSuccessorIDs():
                        if succID not in loopBody:
                            forLoop = True
                    if not forLoop:
                        forwardBranches.append(vertexID)                    
        if forwardBranches:
            self.__solveDFF(forwardBranches)        
    
    def __solveDFF (self, forwardBranches):
        # Initialise data structures needed
        vertexToReachable = {}
        for v in self:
            vertexToReachable[v] = set([])
        
        # Do a reverse post-order. Avoid the entry vertex
        dfs    = Trees.DepthFirstSearch(self, self._entryID)
        postID = self.numOfVertices() - 1
        while postID >= 1:
            vertexID = dfs.getPostorderVertexID(postID)
            v        = self.getVertex(vertexID)
            vertexToReachable[v].add(vertexID)
            for predID in v.getPredecessorIDs():
                predv = self.getVertex(predID)
                vertexToReachable[v].add(predID)
                vertexToReachable[v].update(vertexToReachable[predv])
            postID -= 1 
        
        # Now analyse the immediate post-dominator 
        reverseg    = self.getReverseCFG()
        postdomTree = Trees.Dominators(reverseg, reverseg.getEntryID())
        for vertexID in forwardBranches:
            Debug.debugMessage("Vertex %d is a forward branch" % vertexID, 5)
            branchv  = self.getVertex(vertexID)
            succSet  = set(branchv.getSuccessorIDs())
            treev    = postdomTree.getVertex(vertexID)
            parentID = treev.getParentID()
            mergev   = self.getVertex(parentID)
            Debug.debugMessage("Analysing region (%d, %d)" % (vertexID, parentID), 5)
#            for succID in branchv.getSuccessorIDs():
#                if succID != parentID:
#                    self.addEdge(parentID, succID)
#                    self.__branchDivergentEdges.append((parentID, succID))
            for predID in mergev.getPredecessorIDs():
                predv    = self.getVertex(predID)
                newsuccs = set.difference(succSet, vertexToReachable[predv]) 
                for newsuccID in newsuccs:
                    if newsuccID not in predv.getSuccessorIDs() and newsuccID != predID:
                        self.addEdge(predID, newsuccID)
                        self.__branchDivergentEdges.append((predID, newsuccID))
    