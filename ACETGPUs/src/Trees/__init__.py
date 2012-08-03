import DirectedGraph, Debug, Vertices

class Tree (DirectedGraph.DirectedGraph):
    def __init__ (self):
        DirectedGraph.DirectedGraph.__init__(self)
        self._rootID = DirectedGraph.dummyVertexID 
        
    def setRootID (self, rootID):
        assert rootID > DirectedGraph.dummyVertexID, "Invalid root ID %s. It must be a positive integer" % rootID
        assert rootID in self.vertices, "Cannot find vertex %s"
        self._rootID = rootID
        
    def getRootID (self):
        assert self._rootID != DirectedGraph.dummyVertexID, "Root ID has not yet been set"
        return self._rootID
        
    def addVertex (self, vertexID):
        assert vertexID not in self.vertices, "Adding vertex %s which is already in tree" % vertexID
        treev = Vertices.TreeVertex(vertexID)
        self.vertices[vertexID] = treev
        
    def addEdge (self, predID, succID):
        DirectedGraph.DirectedGraph.addEdge(self, predID, succID)
        succv = self.getVertex(succID)
        succv.setParentID(predID)
        
    def isInternalVertex (self, vertexID):
        return self.getVertex(_vertexID).numberOfSuccessors() > 0
        
    def levelIterator (self, up=True):
        rootv = self.getVertex(self.getRootID())
        rootv.setLevel(0)
        queue = [rootv]
        levelToVertices = {}
        while queue:
            v = queue.pop()
            for succID in v.getSuccessorIDs():
                queue.insert(0, self.getVertex(succID))
            
            if v.getVertexID() == self.getRootID():
                levelToVertices[0] = [rootv]
            else:
                newLevel = self.getVertex(v.getParentID()).getLevel() + 1
                v.setLevel(newLevel)
                if newLevel not in levelToVertices.keys():
                    levelToVertices[newLevel] = []
                levelToVertices[newLevel].append(v)
        
        if up:
            for level in reversed(sorted(levelToVertices.keys())):
                yield level, levelToVertices[level]
        else:
            for level in sorted(levelToVertices.keys()):
                yield level, levelToVertices[level]
        
class DepthFirstSearch (Tree):
    def __init__(self, directedg, rootID):
        assert rootID in directedg.vertices.keys(), "Unable to find vertex %s from which to initiate depth-first search" % rootID
        Tree.__init__(self)
        self.__preorder = []
        self.__postorder = []
        self.__preorderID = 1
        self.__postorderID = 1
        self.__vertexID2PreID = {}
        self.__vertexID2PostID = {}
        self.__backedges = []
        self.initialise (directedg)
        self.setRootID(rootID)
        self.doSearch (directedg, rootID)
        
    def initialise (self, directedg):
        for v in directedg:
            vertexID = v.getVertexID()
            self.vertices[vertexID] = Vertices.TreeVertex(vertexID)
            self.__vertexID2PreID[vertexID] = 0
            self.__vertexID2PostID[vertexID] = 0
        
    def doSearch (self, directedg, vertexID):
        self.__vertexID2PreID[vertexID] = self.__preorderID
        self.__preorder.append(vertexID)
        self.__preorderID += 1
        
        v = directedg.getVertex(vertexID)
        for succID in v.getSuccessorIDs ():
            if self.__vertexID2PreID[succID] == 0:
                self.addEdge(vertexID, succID)
                self.doSearch(directedg, succID)
            elif self.__vertexID2PreID[vertexID] < self.__vertexID2PreID[succID]:
                pass
            elif self.__vertexID2PostID[succID] == 0:
                self.__backedges.append([vertexID, succID])
        
        self.__vertexID2PostID[vertexID] = self.__postorderID
        self.__postorder.append(vertexID)
        self.__postorderID += 1
        
    def getPreorder (self):
        return self.__preorder
    
    def getPostorder (self):
        return self.__postorder
    
    def isDFSBackedge (self, sourceID, destinationID):
        return [sourceID, destinationID] in self.__backedges
    
class LoopNests (Tree):
    def __init__(self, directedg, rootID):
        assert rootID in directedg.vertices.keys(), "Unable to find vertex %s from which to initiate depth-first search" % rootID
        Tree.__init__(self)
        self.__directedg = directedg
        self.__parent = {}
        self.__loopBodies = {}
        self.__loopTails = {}
        self.__headerVertices = {}
        self.__loopExits = {}
        self.__selfLoopHeaders = []
        self._initialise ()
        self._findLoops (rootID)
        self._findExits ()
        # Set the tree root ID to the header vertex representing the root of the 
        # directed graph
        self.setRootID(self.__headerVertices[rootID])
        
    def _initialise (self):
        for v in self.__directedg:
            vertexID = v.getVertexID()
            self.__parent[vertexID] = vertexID
            self.vertices[vertexID] = Vertices.TreeVertex(vertexID)
            
    def _findLoops (self, rootID):
        dfs = DepthFirstSearch (self.__directedg, rootID)
        for vertexID in reversed(dfs.getPreorder()):
            v = self.__directedg.getVertex(vertexID)
            worklist = []
            for predID in v.getPredecessorIDs():
                if dfs.isDFSBackedge(predID, vertexID):
                    if predID == vertexID:
                        Debug.debugMessage("%s => %s is a loop-back edge of trivial loop" % (predID, vertexID), 3)
                        self._addSelfLoop (vertexID)
                    else:
                        Debug.debugMessage("%s => %s is a loop-back edge of non-trivial loop" % (predID, vertexID), 3)
                        worklist.append(self.__parent[predID])
            
            if worklist:
                self._findLoop (dfs, worklist, vertexID)
                
    def _addSelfLoop (self, headerID):
        newVertexID = self.getNextVertexID()
        headerv = Vertices.HeaderVertex(newVertexID, headerID)
        self.vertices[newVertexID] = headerv
        self.__headerVertices[headerID] = newVertexID
        self.__loopTails[headerID] = [headerID]
        self.__loopBodies[headerID] = [headerID]
        self.__selfLoopHeaders.append(headerID)
        self.addEdge(newVertexID, headerID)
            
    def _findLoop (self, dfs, worklist, headerID):
        if headerID not in self.__headerVertices.keys():
            newVertexID = self.getNextVertexID()
            headerv     = Vertices.HeaderVertex(newVertexID, headerID)
            self.vertices[newVertexID] = headerv
            self.__headerVertices[headerID] = newVertexID
            self.__loopTails[headerID] = [t for t in worklist]
        else:
            self.__loopTails[headerID].extend([t for t in worklist])
            
        loopBody = []
        while worklist:
            listID = worklist[-1]
            del worklist[-1]
            loopBody.append(listID)
            
            v = self.__directedg.getVertex(listID)
            for predID in v.getPredecessorIDs():
                if not dfs.isDFSBackedge(predID, listID):
                    repID = self.__parent[predID]
                    if repID not in worklist and repID not in loopBody and repID != headerID:
                        worklist.append(repID)
        
        if loopBody:
            parentID = self.__headerVertices[headerID]
            for vertexID in loopBody:
                self.__parent[vertexID] = headerID
                if vertexID in self.__headerVertices.keys():
                    childID = self.__headerVertices[vertexID]
                    self.addEdge(parentID, childID)
                else:
                    self.addEdge(parentID, vertexID)
            self.addEdge(parentID, headerID)
            self.__loopBodies[headerID] = [headerID]
            self.__loopBodies[headerID].extend([v for v in loopBody])
            
    def _findExits (self):
        for headerID in self.__headerVertices.keys():
            self.__loopExits[headerID] = []
            for vertexID in self.__loopBodies[headerID]:
                v = self.__directedg.getVertex(vertexID)
                for succID in v.getSuccessorIDs():
                    if succID not in self.__loopBodies[headerID]:
                        if headerID != vertexID and self.isLoopHeader(vertexID):
                            if succID not in self.__loopBodies[vertexID]:
                                self.__loopExits[headerID].append(vertexID)
                        else:
                            self.__loopExits[headerID].append(vertexID)
            Debug.debugMessage("Exits of %s = %s" % (headerID, self.__loopExits[headerID]), 4)
            
    def __str__ (self):
        string = "*" * 20 + " LNT Output " + "*" * 20 + "\n"
        for v in self.vertices.values():
            string += v.__str__()
        return string
    
    def isLoopHeader (self, vertexID):
        return vertexID in self.__headerVertices.keys()
    
    def getInternalHeaderVertex (self, vertexID):
        v = self.getVertex(vertexID)
        assert isinstance(v, Vertices.HeaderVertex), "Vertex %s of LNT is not an internal header vertex" % vertexID
        return v
    
    def isSelfLoopHeader (self, vertexID):
        return vertexID in self.__selfLoopHeaders
        
    def getHeaderIDs (self):
        return self.__headerVertices.keys()
    
    def getNumberOfHeaders (self):
        return len(self.__headerVertices.keys())
    
    def getNumberOfSelfLoopHeaders (self):
        return len(self.__selfLoopHeaders)
    
    def getSelfLoopHeaderIDs (self):
        return self.__selfLoopHeaders
    
    def getLoopTails (self, headerID):
        assert headerID in self.__headerVertices.keys(), "Vertex %s is not a loop header" % headerID
        return self.__loopTails[headerID]
    
    def numberOfLoopTails (self, headerID):
        assert headerID in self.__headerVertices.keys(), "Vertex %s is not a loop header" % headerID
        return len(self.__loopTails[headerID])
    
    def getLoopExits (self, headerID):
        assert headerID in self.__headerVertices.keys(), "Vertex %s is not a loop header" % headerID
        return self.__loopExits[headerID]
    
    def getLoopBody (self, headerID):
        assert headerID in self.__headerVertices.keys(), "Vertex %s is not a loop header" % headerID
        return self.__loopBodies[headerID]
    
    def getIpointsInLoopBody (self, headerID):
        assert headerID in self.__headerVertices.keys(), "Vertex %s is not a loop header" % headerID
        return [vertexID for vertexID in self.__loopBodies[headerID] 
                if isinstance(self.__directedg.getVertex(vertexID), Vertices.Ipoint)]
    
    def isLoopBackEdge (self, sourceID, destinationID):
        if destinationID not in self.__headerVertices.keys():
            return False
        else:
            return sourceID in self.__loopTails[destinationID]
        
    def isLoopExitOfLoop (self, vertexID, headerID):
        assert headerID in self.__headerVertices.keys(), "Vertex %s is not a loop header" % headerID
        return vertexID in self.__loopExits[headerID]
    
    def isLoopExit (self, vertexID):
        for headerID in self.__headerVertices:
            if self.isLoopExitOfLoop(vertexID, headerID):
                return True
        return False
    