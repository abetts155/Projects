import DirectedGraph, Debug
from Vertices import TreeVertex, HeaderVertex
from multiprocessing.pool import worker

class Tree (DirectedGraph.DirectedGraph):
    def __init__ (self):
        DirectedGraph.DirectedGraph.__init__(self)
        self.rootID = DirectedGraph.dummyVertexID 
        
    def setRootID (self, rootID):
        assert rootID > DirectedGraph.dummyVertexID, "Invalid root ID %s. It must be a positive integer" % rootID
        assert rootID in self.vertices, "Cannot find vertex %s"
        self.rootID = rootID
        
    def getRootID (self):
        assert self.rootID != DirectedGraph.dummyVertexID, "Root ID has not yet been set"
        return self.rootID
        
    def addVertex (self, vertexID):
        assert vertexID not in self.vertices, "Adding vertex %s which is already in tree" % vertexID
        treev = TreeVertex(vertexID)
        self.vertices[vertexID] = treev
        
    def addEdge (self, predID, succID):
        DirectedGraph.DirectedGraph.addEdge(self, predID, succID)
        succv = self.getVertex(succID)
        succv.setParentID(predID)
        
class DepthFirstSearch (Tree):
    def __init__(self, directedg, rootID):
        assert rootID in directedg.vertices.keys(), "Unable to find vertex %s from which to initiate depth-first search" % rootID
        Tree.__init__(self)
        self.preorder = []
        self.postorder = []
        self.preorderID = 1
        self.postorderID = 1
        self.vertexID2PreID = {}
        self.vertexID2PostID = {}
        self.backedges = []
        self.initialise (directedg)
        self.setRootID(rootID)
        self.doSearch (directedg, rootID)
        
    def initialise (self, directedg):
        for v in directedg:
            vertexID = v.getVertexID()
            self.vertices[vertexID] = TreeVertex(vertexID)
            self.vertexID2PreID[vertexID] = 0
            self.vertexID2PostID[vertexID] = 0
        
    def doSearch (self, directedg, vertexID):
        self.vertexID2PreID[vertexID] = self.preorderID
        self.preorder.append(vertexID)
        self.preorderID += 1
        
        v = directedg.getVertex(vertexID)
        for succID in v.getSuccessorIDs ():
            if self.vertexID2PreID[succID] == 0:
                self.addEdge(vertexID, succID)
                self.doSearch(directedg, succID)
            elif self.vertexID2PreID[vertexID] < self.vertexID2PreID[succID]:
                pass
            elif self.vertexID2PostID[succID] == 0:
                self.backedges.append([vertexID, succID])
        
        self.vertexID2PostID[vertexID] = self.postorderID
        self.postorder.append(vertexID)
        self.postorderID += 1
        
    def getPreorder (self):
        return self.preorder
    
    def getPostorder (self):
        return self.postorder
    
class LoopNests (Tree):
    def __init__(self, directedg, rootID):
        assert rootID in directedg.vertices.keys(), "Unable to find vertex %s from which to initiate depth-first search" % rootID
        Tree.__init__(self)
        self.directedg = directedg
        self.parent = {}
        self.loopBodies = {}
        self.loopTails = {}
        self.headerVertices = {}
        self.initialise ()
        self.findLoops (rootID)
        
        # Set the tree root ID to the header vertex representing the root of the 
        # directed graph
        self.rootID = self.headerVertices[rootID]
        
    def initialise (self):
        for v in self.directedg:
            vertexID = v.getVertexID()
            self.parent[vertexID] = vertexID
            self.vertices[vertexID] = TreeVertex(vertexID)
            
    def findLoops (self, rootID):
        dfs = DepthFirstSearch (self.directedg, rootID)
        for vertexID in reversed(dfs.getPreorder()):
            v = self.directedg.getVertex(vertexID)
            worklist = []
            for predID in v.getPredecessorIDs():
                if [predID, vertexID] in dfs.backedges:
                    if predID == vertexID:
                        Debug.debugMessage("%s => %s is a loop-back edge of trivial loop" % (predID, vertexID), 3)
                        self.addSelfLoop (vertexID)
                    else:
                        Debug.debugMessage("%s => %s is a loop-back edge of non-trivial loop" % (predID, vertexID), 3)
                        worklist.append(self.parent[predID])
            
            if worklist:
                self.findLoop (dfs, worklist, vertexID)
                
    def addSelfLoop (self, headerID):
        newVertexID = self.getNextVertexID()
        headerv = HeaderVertex(newVertexID, headerID)
        self.vertices[newVertexID] = headerv
        self.headerVertices[headerID] = newVertexID
        self.loopTails[headerID] = [headerID]
        self.loopBodies[headerID] = [headerID]
        self.addEdge(newVertexID, headerID)
            
    def findLoop (self, dfs, worklist, headerID):
        if headerID not in self.headerVertices.keys():
            newVertexID = self.getNextVertexID()
            headerv = HeaderVertex(newVertexID, headerID)
            self.vertices[newVertexID] = headerv
            self.headerVertices[headerID] = newVertexID
            self.loopTails[headerID] = [t for t in worklist]
        else:
            self.loopTails[headerID].extend([t for t in worklist])
            
        loopBody = []
        while worklist:
            listID = worklist[-1]
            del worklist[-1]
            loopBody.append(listID)
            
            v = self.directedg.getVertex(listID)
            for predID in v.getPredecessorIDs():
                if [predID, listID] not in dfs.backedges:
                    repID = self.parent[predID]
                    if repID not in worklist and repID not in loopBody and repID != headerID:
                        worklist.append(repID)
        
        if loopBody:
            parentID = self.headerVertices[headerID]
            for vertexID in loopBody:
                self.parent[vertexID] = headerID
                if vertexID in self.headerVertices.keys():
                    childID = self.headerVertices[vertexID]
                    self.addEdge(parentID, childID)
                else:
                    self.addEdge(parentID, vertexID)
            self.addEdge(parentID, headerID)
            self.loopBodies[headerID] = [headerID]
            self.loopBodies[headerID].extend([v for v in loopBody])
            
    def __str__ (self):
        string = "*" * 20 + " LNT Output " + "*" * 20 + "\n"
        for v in self.vertices.values():
            string += v.__str__()
        return string
            
            