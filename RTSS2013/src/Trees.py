from DirectedGraphs import DirectedGraph
from Vertices import TreeVertex, HeaderVertex, Ipoint, dummyVertexID
import Debug, CFGs

class Tree (DirectedGraph):
    def __init__ (self):
        DirectedGraph.__init__(self)
        self._rootID = dummyVertexID
        
    def setRootID (self, rootID):
        assert rootID > dummyVertexID, "Invalid root ID %d. It must be a positive integer" % rootID
        assert rootID in self.vertices, "Cannot find vertex %d"
        self._rootID = rootID
        
    def getRootID (self):
        assert self._rootID != dummyVertexID, "Root ID has not yet been set"
        return self._rootID
        
    def addVertex (self, vertexID):
        assert vertexID not in self.vertices, "Adding vertex %d which is already in tree" % vertexID
        treev = TreeVertex(vertexID)
        self.vertices[vertexID] = treev
        
    def addEdge (self, predID, succID):
        DirectedGraph.addEdge(self, predID, succID)
        succv = self.getVertex(succID)
        succv.setParentID(predID)
        
    def isInternalVertex (self, vertexID):
        return self.getVertex(vertexID).numberOfSuccessors() > 0
    
    def getAllProperAncestors (self, vertexID):
        ancestors = []
        while vertexID != self._rootID:
            parentID = self.getVertex(vertexID).getParentID()
            ancestors.append(self.getVertex(parentID))
            vertexID = parentID
        return ancestors
    
    def isAncestor (self, left, right):
        if left == right:
            return True
        elif right == self._rootID:
            return False
        else:
            vertexID = right
            parentID = self.getVertex(vertexID).getParentID()
            while parentID != self._rootID and parentID != left:
                vertexID = parentID
                parentID = self.getVertex(vertexID).getParentID()
            if parentID == left:
                return True
            else:
                return False
    
    def isProperAncestor (self, left, right):
        if left == right:
            return False
        else:
            return self.isAncestor(left, right)
    
    def getHeight (self):
        maxHeight = 0
        height = {}
        stack  = []
        rootv = self.getVertex(self.getRootID())
        stack.append(rootv)
        height[rootv] = 0
        while stack:
            v = stack.pop()
            for succID in v.getSuccessorIDs():
                succv = self.getVertex(succID)
                stack.append(succv)
                height[succv] = height[v] + 1
                if height[succv] > maxHeight:
                    maxHeight = height[succv]
        return maxHeight
        
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
                
    def __str__ (self):
        string = ""
        for level, vertices in self.levelIterator(False):
            string += "%s LEVEL = %d %s\n" % ('*' * 5, level, '*' * 5)
            for v in vertices:
                if level == 0:
                    string += "%d (root)\n" % (v.getVertexID())
                else:
                    string += "%d (parent = %d)\n" % (v.getVertexID(), v.getParentID())
        return string
    

class Betts (Tree):
    def __init__(self, directedg, rootID):
        assert rootID in directedg.vertices.keys(), "Unable to find vertex %d from which to initiate depth-first search" % rootID
        Tree.__init__(self)
        self.__directedg = directedg
        self.__rootID    = rootID
        self.__singles   = []
        self.__merges    = []
        self.__branches  = []
        self.__ipre      = {}
        self.__ipost     = {}
        dfs = DepthFirstSearch(self.__directedg, self.__rootID)
        self._initialise(dfs)
        self._compute(dfs)
    
    def _initialise (self, dfs):
        for vertexID in reversed(dfs.getPostorder()):
            treev = dfs.getVertex(vertexID)
            v     = self.__directedg.getVertex(vertexID)
            self.__ipre[v]  = v
            self.__ipost[v] = v
            if v.numberOfPredecessors() == 1 and v.numberOfSuccessors() == 1 and vertexID != dfs.getRootID():
                if not self.__directedg.getVertex(treev.getParentID()) in self.__singles:
                    self.__singles.append(v)
            if v.numberOfSuccessors() > 1:
                self.__branches.append(v)
            if  v.numberOfPredecessors() > 1:
                self.__merges.append(v)
    
    def _compute (self,dfs):
        for v in self.__singles:
            self._reduce(v)
        print self.__directedg
        for v in reversed(self.__branches):
            self._reduce(v)
                
        for v in self.__directedg:
            print "ipre(%d) = %d" % (v.getVertexID(), self.__ipre[v].getVertexID()) 
            print "ipost(%d) = %d" % (v.getVertexID(), self.__ipost[v].getVertexID())  
            
    def _reduce (self, v):
        pred        = self.__directedg.getVertex(v.getPredecessorIDs()[0])
        source      = pred
        succ        = self.__directedg.getVertex(v.getSuccessorIDs()[0])
        destination = succ
        nextv       = v
        print pred
        print v
        print succ
        while True:
            self.__ipre[nextv] = pred
            self.__ipost[nextv] = succ
            if succ in self.__merges or succ.getVertexID() == self.__directedg.getExitID():
                break
            pred  = nextv
            nextv = succ
            succ  = self.__directedg.getVertex(nextv.getSuccessorIDs()[0])
            destination = succ
        print destination
        self.__directedg.removeEdge(source.getVertexID(), v.getVertexID())
        self.__directedg.removeEdge(nextv.getVertexID(), destination.getVertexID())
        self.__directedg.addEdge(source.getVertexID(),destination.getVertexID())

class LengauerTarjan (Tree):
    def __init__(self, directedg, rootID):
        assert rootID in directedg.vertices.keys(), "Unable to find vertex %d from which to initiate depth-first search" % rootID
        Tree.__init__(self)
        self.__directedg = directedg
        dfs = DepthFirstSearch(self.__directedg, rootID)
        self.__parent = {}
        self.__semi = {}
        self.__idom = {}
        self.__ancestor = {}
        self.__best = {}
        self.__bucket = {}
        self._initialise (dfs)
        self._compute (dfs)
        
    def _initialise (self, dfs):
        for v in self.__directedg:
            vertexID = v.getVertexID()
            if vertexID != dfs.getRootID():
                self.__parent[vertexID] = dfs.getVertex(vertexID).getParentID()
            self.__semi[vertexID] = dfs.getPreID(vertexID)
            self.__ancestor[vertexID] = None
            self.__idom[vertexID] = None
            self.__bucket[vertexID] = set([])
    
    def _compute (self, dfs):
        for preID in xrange(self.__directedg.numOfVertices(), 1, -1):
            vertexID = dfs.getPreorderVertexID(preID)
            v        = self.__directedg.getVertex(vertexID)
            parentID = self.__parent[vertexID]
            for predID in v.getPredecessorIDs():
                uID = self._eval(predID)
                if self.__semi[uID] < self.__semi[vertexID]:
                    self.__semi[vertexID] = self.__semi[uID]
            print "semi(%d) = %d" % (vertexID,  self.__semi[vertexID]) 
            # At this point semi only has DFS numbers
            self.__bucket[dfs.getPreorderVertexID(self.__semi[vertexID])].add(vertexID)
            self._link(parentID, vertexID)
            for bID in self.__bucket[parentID]:
                uID = self._eval(bID)
                if self.__semi[uID] < dfs.getPreID(parentID): 
                    self.__idom[bID] = uID
                else:
                    self.__idom[bID] = parentID
            self.__bucket[parentID] = set([])
    
        for preID in xrange(2, self.__directedg.numOfVertices() + 1, 1):
            vertexID = dfs.getPreorderVertexID(preID)
            if self.__idom[vertexID] != dfs.getPreorderVertexID(self.__semi[vertexID]):
                self.__idom[vertexID] = self.__idom[self.__idom[vertexID]]
                    
        for vertexID in dfs.getPreorder():
            if vertexID != dfs.getRootID():
                print "idom(%d) = %d" % (vertexID, self.__idom[vertexID])
                
    def _eval (self, vID):
        aID = self.__ancestor[vID]
        while aID and self.__ancestor[aID]:
            if self.__semi[vID] > self.__semi[aID]:
                vID = aID
            aID = self.__ancestor[aID]
        return vID
    
    def _link (self, vID, wID):
        self.__ancestor[wID] = vID
        
class DepthFirstSearch (Tree):
    def __init__(self, directedg, rootID):
        assert rootID in directedg.vertices.keys(), "Unable to find vertex %d from which to initiate depth-first search" % rootID
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
            self.vertices[vertexID] = TreeVertex(vertexID)
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
    
    def getPreorderVertexID (self, preID):
        assert preID - 1 < len(self.__preorder), "Pre-order number %d too high" % preID
        return self.__preorder[preID-1]
    
    def getPostorderVertexID (self, postID):
        assert postID - 1 < len(self.__postorder), "Post-order number %d too high" % postID
        return self.__postorder[postID-1]
    
    def getPreID (self, vertexID):
        assert vertexID in self.__vertexID2PreID, "Unable to find pre-order numbering for vertex %d" % vertexID
        return self.__vertexID2PreID[vertexID]
    
    def getPostID (self, vertexID):
        assert vertexID in self.__vertexID2PostID, "Unable to find post-order numbering for vertex %d" % vertexID
        return self.__vertexID2PostID[vertexID]
    
    def isDFSBackedge (self, sourceID, destinationID):
        return [sourceID, destinationID] in self.__backedges
    
class CompressedDominatorTree (Tree):
    def __init__(self, domTree, lca, vertexID, neighbourIDs):
        Debug.debugMessage("Building compressed dominator tree for %d using %s" \
                           % (vertexID, neighbourIDs), 20)
        Tree.__init__(self)
        self.__build(lca, neighbourIDs)
        self._rootID = domTree.getImmediateDominator(vertexID)
        
    def __build(self, lca, edges):
        querySet = set(edges)      
        while len(querySet) > 1:
            vToLCA  = {}
            for e1 in querySet:
                for e2 in querySet:
                    if e1 != e2:
                        lcaID = lca.getLCA(e1, e2)
                        # Update for e1
                        if e1 in vToLCA:
                            oldlcaID = vToLCA[e1]
                            if lca.getLevel(lcaID) > lca.getLevel(oldlcaID) and oldlcaID != e1:
                                vToLCA[e1] = lcaID
                        else:
                            vToLCA[e1] = lcaID
                        # Update for e2
                        if e2 in vToLCA:
                            oldlcaID = vToLCA[e2]
                            if lca.getLevel(lcaID) > lca.getLevel(oldlcaID) and oldlcaID != e2:
                                vToLCA[e2] = lcaID
                        else:
                            vToLCA[e2] = lcaID
            # Add edge links                
            for vertexID, parentID in vToLCA.items():
                if not self.hasVertex(vertexID):
                    self.addVertex(vertexID)                
                if not self.hasVertex(parentID):
                    self.addVertex(parentID)
                if parentID != vertexID:
                    Debug.debugMessage("Adding edge (%d, %d)" % (parentID, vertexID), 20)
                    self.addEdge(parentID, vertexID)
            # Any vertex without a predecessor goes into the query set
            newQuerySet = []
            for v in self:
                if v.numberOfPredecessors() == 0:
                    newQuerySet.append(v.getVertexID())
            querySet = set(newQuerySet)
    
class LeastCommonAncestor ():
    def __init__(self, tree):
        self.__tree = tree
        self.__euler = {}
        self.__level = {}
        self.__vertexToLevel = {}
        self.__representative = {}
        self.__eulerIndex = 0
        self.__dummyLevel = 0
        self.__vertexToLevel[self.__tree.getRootID()] = 0
        self.__doDFS(self.__tree.getRootID())
        self.__dummyLevel += 1
        self.__computeRepresentativeIndices()
        
    def __doDFS (self, vertexID):
        v = self.__tree.getVertex(vertexID)
        self.__euler[self.__eulerIndex] = vertexID
        self.__eulerIndex += 1
        for succID in v.getSuccessorIDs():
            self.__vertexToLevel[succID] = self.__vertexToLevel[vertexID] + 1
            if self.__vertexToLevel[succID] > self.__dummyLevel:
                self.__dummyLevel = self.__vertexToLevel[succID]
            self.__doDFS(succID)
            self.__euler[self.__eulerIndex] = vertexID
            self.__eulerIndex += 1
    
    def __computeRepresentativeIndices (self):
        for index, vertexID in self.__euler.items():
            self.__representative[vertexID] = index
            self.__level[index] = self.__vertexToLevel[vertexID]
            
    def getLevel (self, vertexID):
        assert vertexID in self.__vertexToLevel, "Cannot find vertex %d" % vertexID
        return self.__vertexToLevel[vertexID]
            
    def getLCA (self, left, right):    
        Debug.debugMessage("Computing lca(%d, %d)" % (left, right), 20) 
        repID1      = self.__representative[left]
        repID2      = self.__representative[right]
        lowestLevel = self.__dummyLevel
        startIndex  = None
        endIndex    = None
        levelIndex  = 2 * self.__tree.numOfVertices()
        
        if repID1 < repID2:
            startIndex = repID1
            endIndex   = repID2
        else:
            startIndex = repID2
            endIndex   = repID1
        for i in range (startIndex, endIndex+1):
            if self.__level[i] < lowestLevel:
                lowestLevel = self.__level[i]
                levelIndex  = i
        Debug.debugMessage("lca(%d, %d) = %d" % (left, right, self.__euler[levelIndex]), 15)
        return self.__euler[levelIndex]
    
class Dominators (Tree):
    def __init__(self, directedg, rootID):
        assert rootID in directedg.vertices.keys(), "Unable to find vertex %d from which to initiate depth-first search" % rootID
        Tree.__init__(self)
        self.__directedg = directedg
        self.__immediateDom = {}
        self._initialise (rootID)
        self._solveDFF ()
        self._addEdges ()
        
    def getImmediateDominator (self, vertexID):
        assert vertexID != self._rootID, "Vertex %d does not have an immediate dominator as it is the root" % vertexID
        return self.getVertex(vertexID).getParentID()
    
    def _initialise (self, rootID):
        for v in self.__directedg:
            vertexID = v.getVertexID()            
            self.vertices[vertexID] = TreeVertex(vertexID)
            if vertexID == rootID:
                self.__immediateDom[vertexID] = vertexID
            else:
                self.__immediateDom[vertexID] = dummyVertexID
        self.setRootID(rootID)
    
    def _solveDFF (self):
        dfs = DepthFirstSearch(self.__directedg, self._rootID)
        changed = True
        while changed:
            changed = False
            postID = self.__directedg.numOfVertices()
            while postID >= 1:
                vertexID = dfs.getPostorderVertexID(postID)
                if vertexID != self._rootID:
                    v               = self.__directedg.getVertex(vertexID)
                    processedPredID = dummyVertexID
                    newIdomID       = dummyVertexID
                    
                    for predID in v.getPredecessorIDs():
                        if self.__immediateDom[predID] != dummyVertexID:
                            processedPredID = predID
                            newIdomID       = processedPredID
                    for predID in v.getPredecessorIDs():
                        if predID != processedPredID:
                            if self.__immediateDom[predID] != dummyVertexID:
                                newIdomID = self._intersect(dfs, predID, newIdomID)
                    
                    if newIdomID != dummyVertexID:
                        if self.__immediateDom[vertexID] != newIdomID:
                            changed = True
                            self.__immediateDom[vertexID] = newIdomID
                postID -= 1
    
    def _intersect (self, dfs, left, right):
        uID = left
        vID = right
        while (dfs.getPostID(uID) != dfs.getPostID(vID)):
            while (dfs.getPostID(uID) < dfs.getPostID(vID)):
                uID = self.__immediateDom[uID]
            while (dfs.getPostID(vID) < dfs.getPostID(uID)):
                vID = self.__immediateDom[vID]
        return uID
    
    def _addEdges (self):
        for v in self.__directedg:
            vertexID = v.getVertexID()
            if vertexID != self._rootID:
                assert self.__immediateDom[vertexID] != dummyVertexID, "Immediate dominator of %d not set" % vertexID
                self.addEdge(self.__immediateDom[vertexID], vertexID)
                
class DominanceFrontiers:
    def __init__(self, cfg, domTree):
        self.__cfg     = cfg
        self.__domTree = domTree
        self.__initialise()
        self.__compute()
        
    def output(self):
        for v in self.__cfg:
            vertexID = v.getVertexID()
            print "DF(%d) = %s" % (vertexID, self.__vToDF[vertexID])
        
    def size (self, vertexID):
        assert vertexID in self.__vToDF, "Unable to find %d in dominance frontiers" % vertexID
        return len(self.__vToDF[vertexID])
    
    def isEmpty (self, vertexID):
        assert vertexID in self.__vToDF, "Unable to find %d in dominance frontiers" % vertexID
        return len(self.__vToDF[vertexID]) == 0
    
    def set (self, vertexID):
        assert vertexID in self.__vToDF, "Unable to find %d in dominance frontiers" % vertexID
        return self.__vToDF[vertexID]
    
    def contains (self, vertexID, elementID):
        assert vertexID in self.__vToDF, "Unable to find %d in dominance frontiers" % vertexID
        return elementID in self.__vToDF[vertexID]
    
    def __initialise(self):
        self.__vToDF = {}
        for v in self.__cfg:
            self.__vToDF[v.getVertexID()] = set([])
    
    def __compute(self):
        for v in self.__cfg:
            vertexID = v.getVertexID()    
            if v.numberOfPredecessors() > 1: 
                idomID = self.__domTree.getImmediateDominator(vertexID)
                for predID in v.getPredecessorIDs():
                    runnerID = predID
                    while runnerID != idomID:
                        self.__vToDF[runnerID].add(vertexID)
                        runnerID = self.__domTree.getImmediateDominator(runnerID)
    
class LoopNests (Tree):
    def __init__(self, directedg, rootID):
        Tree.__init__(self)
        assert rootID in directedg.vertices.keys(), "Unable to find vertex %d from which to initiate depth-first search" % rootID
        self.__predomTree = Dominators(directedg, rootID)
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
            self.vertices[vertexID] = TreeVertex(vertexID)
            
    def _findLoops (self, rootID):
        self.__dfs = DepthFirstSearch (self.__directedg, rootID)
        for vertexID in reversed(self.__dfs.getPreorder()):
            v = self.__directedg.getVertex(vertexID)
            worklist = []
            for predID in v.getPredecessorIDs():
                if self.__dfs.isDFSBackedge(predID, vertexID):
                    assert self.__predomTree.isAncestor(vertexID, predID), "Non-reducible loop found with DFS backedge %d => %d" % (predID, vertexID)
                    if predID == vertexID:
                        Debug.debugMessage("%s => %s is a loop-back edge of trivial loop" % (predID, vertexID), 15)
                        self._addSelfLoop (vertexID)
                    else:
                        Debug.debugMessage("%s => %s is a loop-back edge of non-trivial loop" % (predID, vertexID), 15)
                        worklist.append(self.__parent[predID])
            
            if worklist:
                self._findLoop (worklist, vertexID)
                
    def _addSelfLoop (self, headerID):
        newVertexID = self.getNextVertexID()
        headerv = HeaderVertex(newVertexID, headerID)
        self.vertices[newVertexID] = headerv
        self.__headerVertices[headerID] = newVertexID
        self.__loopTails[headerID] = [headerID]
        self.__loopBodies[headerID] = [headerID]
        self.__selfLoopHeaders.append(headerID)
        self.addEdge(newVertexID, headerID)
            
    def _findLoop (self, worklist, headerID):
        if headerID not in self.__headerVertices.keys():
            newVertexID = self.getNextVertexID()
            headerv     = HeaderVertex(newVertexID, headerID)
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
                if not self.__dfs.isDFSBackedge(predID, listID):
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
            Debug.debugMessage("Exits of %s = %s" % (headerID, self.__loopExits[headerID]), 15)
            
    def __str__ (self):
        string = "*" * 20 + " LNT Output " + "*" * 20 + "\n"
        for v in self.vertices.values():
            string += v.__str__()
        return string
    
    def isLoopHeader (self, vertexID):
        return vertexID in self.__headerVertices.keys()
    
    def getInternalHeaderVertex (self, vertexID):
        v = self.getVertex(vertexID)
        assert isinstance(v, HeaderVertex), "Vertex %s of LNT is not an internal header vertex" % vertexID
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
    
    def isLoopTail (self, vertexID):
        for headerID, tails in self.__loopTails.iteritems():
            if vertexID in tails:
                return True
        return False
    
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
                if isinstance(self.__directedg.getVertex(vertexID), Ipoint)]
    
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
    
    def isDoWhileLoop (self, headerID):
        assert headerID in self.__headerVertices.keys(), "Vertex %s is not a loop header" % headerID
        return set(self.__loopTails[headerID]) == set(self.__loopExits[headerID])
    
    def isNested (self, left, right):
        return self.isProperAncestor(right, left)
    
    def induceSubgraph (self, headerv):
        assert isinstance(headerv, HeaderVertex), "To induce the acyclic portion of a loop body, you must pass an internal vertex of the LNT."
        headerID = headerv.getHeaderID()
        flowg    = CFGs.ICFG()
        edges    = {}
        worklist = []
        worklist.extend(self.getLoopTails(headerID))
        while worklist:
            vertexID = worklist.pop()
            if not flowg.hasVertex(vertexID):
                # Add the correct vertex type to the induced graph
                if isinstance(self.__directedg.getVertex(vertexID), Ipoint):
                    ipoint = CFGs.Ipoint(vertexID, vertexID)
                    flowg.addIpoint(ipoint)
                else:
                    bb = CFGs.BasicBlock(vertexID)
                    flowg.addVertex(bb)
                # Now discover which edges are incident to this vertex
                edges[vertexID] = set([])                        
                originalv = self.__directedg.getVertex(vertexID)
                for predID in originalv.getPredecessorIDs():
                    treePredv    = self.getVertex(predID)
                    headerPredv  = self.getVertex(treePredv.getParentID())
                    predHeaderID = headerPredv.getHeaderID()
                    if not self.__dfs.isDFSBackedge(predID, vertexID):
                        if predHeaderID == headerID:
                            worklist.append(predID)
                            edges[vertexID].add(predID)
                        elif self.isNested(headerPredv.getVertexID(), headerv.getVertexID()):
                            worklist.append(predHeaderID)
                            edges[vertexID].add(predHeaderID)
                            
        # Add edges in induced subgraph
        for vertexID, predIDs in edges.items():
            for predID in predIDs:
                flowg.addEdge(predID, vertexID) 
        # Add a continuation vertex to induced subgraph
        continuationID = flowg.getNextVertexID()
        bb = CFGs.BasicBlock(continuationID)
        bb.setDummy()
        flowg.addVertex(bb)
        flowg.setExitID(continuationID)
        flowg.getVertex(continuationID).setDummy()
        # Add edge from each exit and each tail to continuation vertex
        for exitID in self.__loopExits[headerID]:
            flowg.addEdge(exitID, continuationID)
        for tailID in self.__loopTails[headerID]:
            flowg.addEdge(tailID, continuationID)
        # Add entry vertex to induced subgraph
        flowg.setEntryID(headerv.getHeaderID())
        return flowg

