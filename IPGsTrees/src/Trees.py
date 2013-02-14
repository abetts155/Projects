from DirectedGraphs import DirectedGraph, dummyVertexID
from Vertices import TreeVertex, HeaderVertex, Ipoint
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
        ancestors = set([])
        while vertexID != self._rootID:
            parentID = self.getVertex(vertexID).getParentID()
            ancestors.add(self.getVertex(parentID))
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
        str = ""
        for level, vertices in self.levelIterator(False):
            str += "%s LEVEL = %d %s\n" % ('*' * 5, level, '*' * 5)
            for v in vertices:
                if level == 0:
                    str += "%d (root)\n" % (v.getVertexID())
                else:
                    str += "%d (parent = %d)\n" % (v.getVertexID(), v.getParentID())
        return str
        
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
        assert preID - 1 < len(self.__postorder), "Pre-order number %d too high" % preID
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
    
class LoopNests (Tree):
    def __init__(self, directedg, rootID):
        assert rootID in directedg.vertices.keys(), "Unable to find vertex %d from which to initiate depth-first search" % rootID
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
            self.vertices[vertexID] = TreeVertex(vertexID)
            
    def _findLoops (self, rootID):
        self.__dfs = DepthFirstSearch (self.__directedg, rootID)
        for vertexID in reversed(self.__dfs.getPreorder()):
            v = self.__directedg.getVertex(vertexID)
            worklist = []
            for predID in v.getPredecessorIDs():
                if self.__dfs.isDFSBackedge(predID, vertexID):
                    if predID == vertexID:
                        Debug.debugMessage("%s => %s is a loop-back edge of trivial loop" % (predID, vertexID), 3)
                        self._addSelfLoop (vertexID)
                    else:
                        Debug.debugMessage("%s => %s is a loop-back edge of non-trivial loop" % (predID, vertexID), 3)
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
        flowg    = CFGs.ICFG()
        edges    = {}
        worklist = []
        worklist.extend(self.getLoopTails(headerv.getHeaderID()))
        while worklist:
            vertexID = worklist.pop()
            if not flowg.hasVertex(vertexID):
                # Add the correct vertex type to the induced graph
                if isinstance(self.__directedg.getVertex(vertexID), CFGs.BasicBlock):
                    bb = CFGs.BasicBlock(vertexID)
                    flowg.addVertex(bb)
                else:
                    ipoint = CFGs.Ipoint(vertexID, vertexID)
                    flowg.addIpoint(ipoint)
                # Now discover which edges are incident to this vertex
                edges[vertexID] = set([])
                if self.isLoopHeader(vertexID) and vertexID != headerv.getHeaderID():
                    if self.isDoWhileLoop(vertexID):
                        flowg.getVertex(vertexID).setDummy()
                        
                originalv = self.__directedg.getVertex(vertexID)
                for predID in originalv.getPredecessorIDs():
                    treePredv    = self.getVertex(predID)
                    headerPredv  = self.getVertex(treePredv.getParentID())
                    predHeaderID = headerPredv.getHeaderID()
                    if not self.__dfs.isDFSBackedge(predID, vertexID):
                        if predHeaderID == headerv.getHeaderID():
                            worklist.append(predID)
                            edges[vertexID].add(predID)
                        elif self.isNested(headerPredv.getVertexID(), headerv.getVertexID()):
                            if self.isDoWhileLoop(predHeaderID):
                                worklist.append(predHeaderID)
                                edges[vertexID].add(predHeaderID)
                            else:
                                worklist.append(predID)
                                edges[vertexID].add(predID)
        
        # Add edges in induced subgraph
        for vertexID, predIDs in edges.items():
            for predID in predIDs:
                flowg.addEdge(predID, vertexID) 
        # Add exit vertex to induced subgraph
        noSuccs = []
        for v in flowg:
            if v.numberOfSuccessors() == 0:
                noSuccs.append(v.getVertexID())
        if len(noSuccs) != 1:
            exitID = flowg.getNextVertexID()
            bb = CFGs.BasicBlock(exitID)
            flowg.addVertex(bb)
            flowg.setExitID(exitID)
            flowg.getVertex(exitID).setDummy()
            for predID in noSuccs:
                flowg.addEdge(predID, exitID)
        else:
            exitID = noSuccs[0]
            flowg.setExitID(exitID)
        # Add entry vertex to induced subgraph
        flowg.setEntryID(headerv.getHeaderID())
        return flowg
    
    