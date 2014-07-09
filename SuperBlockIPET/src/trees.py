import directed_graphs
import vertices
import debug
import utils

class Tree(directed_graphs.DirectedGraph):
    def __init__ (self):
        directed_graphs.DirectedGraph.__init__(self)
        self._rootID = vertices.dummyID
        
    def setRootID (self, rootID):
        assert rootID > vertices.dummyID, "Invalid root ID %d. It must be a positive integer" % rootID
        assert rootID in self.the_vertices, "Cannot find vertex %d"
        self._rootID = rootID
        
    def getRootID (self):
        assert self._rootID != vertices.dummyID, "Root ID has not yet been set"
        return self._rootID
        
    def addVertex (self, vertexID):
        assert vertexID not in self.the_vertices, "Adding vertex %d which is already in tree" % vertexID
        treev = vertices.TreeVertex(vertexID)
        self.the_vertices[vertexID] = treev
        
    def addEdge (self, predID, succID):
        directed_graphs.DirectedGraph.addEdge(self, predID, succID)
        succv = self.getVertex(succID)
        succv.parentID = predID
    
    def getAllProperAncestors (self, vertexID):
        ancestors = []
        while vertexID != self._rootID:
            parentID = self.getVertex(vertexID).parentID
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
            parentID = self.getVertex(vertexID).parentID
            while parentID != self._rootID and parentID != left:
                vertexID = parentID
                parentID = self.getVertex(vertexID).parentID
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
            for succID in v.successors.keys():
                succv = self.getVertex(succID)
                stack.append(succv)
                height[succv] = height[v] + 1
                if height[succv] > maxHeight:
                    maxHeight = height[succv]
        return maxHeight
        
    def levelIterator(self, up=True):
        rootv = self.getVertex(self.getRootID())
        rootv.level = 0
        queue = [rootv]
        levelToVertices = {}
        while queue:
            v = queue.pop()
            for succID in v.successors.keys():
                queue.insert(0, self.getVertex(succID))
            if v.vertexID == self.getRootID():
                levelToVertices[0] = [rootv]
            else:
                v.level = self.getVertex(v.parentID).level + 1
                if v.level not in levelToVertices.keys():
                    levelToVertices[v.level] = []
                levelToVertices[v.level].append(v)
        if up:
            for level in reversed(sorted(levelToVertices.keys())):
                yield level, levelToVertices[level]
        else:
            for level in sorted(levelToVertices.keys()):
                yield level, levelToVertices[level]
        
class DepthFirstSearch (Tree):
    Colors = utils.enum('WHITE', 'BLACK', 'GRAY')

    def __init__(self, directedg, rootID):
        assert rootID in directedg.the_vertices.keys(), "Unable to find vertex %d from which to initiate depth-first search" % rootID
        Tree.__init__(self)
        self.__initialise (directedg, rootID)
        self.__doSearch (directedg, rootID)
        if self.numOfEdges() != self.numOfVertices() - 1:
            debug.warning_message("Edges = %d, vertices = %d" % (self.numOfEdges(), self.numOfVertices()))
        
    def __initialise (self, directedg, rootID):
        self.__preorder  = []
        self.__postorder = []
        self.__vertexID2PostID = {}
        self.__vertexID2PreID = {}
        self.__vertexToColor = {}
        self.__backedges = []
        for v in directedg:
            self.the_vertices[v.vertexID] = vertices.TreeVertex(v.vertexID)
            self.__vertexID2PreID[v.vertexID] = 0
            self.__vertexID2PostID[v.vertexID] = 0
            self.__vertexToColor[v.vertexID] = DepthFirstSearch.Colors.WHITE
        self.__preorderID  = 1
        self.__postorderID = 1
        self.setRootID(rootID) 
      
    def __doSearch (self, directedg, vertexID):
        self.__vertexID2PreID[vertexID] = self.__preorderID
        self.__preorder.append(vertexID)
        self.__preorderID += 1
           
        v = directedg.getVertex(vertexID)
        for succID in v.successors.keys ():
            if self.__vertexID2PreID[succID] == 0:
                self.addEdge(vertexID, succID)
                self.__doSearch(directedg, succID)
            elif self.__vertexID2PreID[vertexID] < self.__vertexID2PreID[succID]:
                pass
            elif self.__vertexID2PostID[succID] == 0:
                self.__backedges.append([vertexID, succID])
        self.__vertexID2PostID[vertexID] = self.__postorderID
        self.__postorder.append(vertexID)
        self.__postorderID += 1
        
    def getPreorder(self):
        return self.__preorder
    
    def getPostorder(self):
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
        debug.debug_message("Building compressed dominator tree for %d using %s" \
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
                    debug.debug_message("Adding edge (%d, %d)" % (parentID, vertexID), 20)
                    self.addEdge(parentID, vertexID)
            # Any vertex without a predecessor goes into the query set
            newQuerySet = []
            for v in self:
                if v.number_of_predecessors() == 0:
                    newQuerySet.append(v.vertexID)
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
        for succID in v.successors.keys():
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
        debug.debug_message("Computing lca(%d, %d)" % (left, right), 20) 
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
        debug.debug_message("lca(%d, %d) = %d" % (left, right, self.__euler[levelIndex]), 15)
        return self.__euler[levelIndex]
    
class Dominators (Tree):
    def __init__(self, directedg, rootID):
        assert rootID in directedg.the_vertices.keys(), "Unable to find vertex %d from which to initiate depth-first search" % rootID
        Tree.__init__(self)
        self.__directedg = directedg
        self.__immediateDom = {}
        self.initialise(rootID)
        self.solve()
        self.add_edges()
        
    def getImmediateDominator (self, vertexID):
        assert vertexID != self._rootID, "Vertex %d does not have an immediate dominator as it is the root" % vertexID
        return self.getVertex(vertexID).parentID
    
    def initialise(self, rootID):
        for v in self.__directedg:           
            self.the_vertices[v.vertexID] = vertices.TreeVertex(v.vertexID)
            if v.vertexID == rootID:
                self.__immediateDom[v.vertexID] = v.vertexID
            else:
                self.__immediateDom[v.vertexID] = vertices.dummyID
        self.setRootID(rootID)
    
    def solve(self):
        dfs = DepthFirstSearch(self.__directedg, self._rootID)
        changed = True
        while changed:
            changed = False
            postID = self.__directedg.numOfVertices()
            while postID >= 1:
                vertexID = dfs.getPostorderVertexID(postID)
                if vertexID != self._rootID:
                    v               = self.__directedg.getVertex(vertexID)
                    processedPredID = vertices.dummyID
                    newIdomID       = vertices.dummyID
                    for predID in v.predecessors.keys():
                        if self.__immediateDom[predID] != vertices.dummyID:
                            processedPredID = predID
                            newIdomID       = processedPredID
                    for predID in v.predecessors.keys():
                        if predID != processedPredID:
                            if self.__immediateDom[predID] != vertices.dummyID:
                                newIdomID = self.intersect(dfs, predID, newIdomID)
                    if newIdomID != vertices.dummyID:
                        if self.__immediateDom[vertexID] != newIdomID:
                            changed = True
                            self.__immediateDom[vertexID] = newIdomID
                postID -= 1
    
    def intersect (self, dfs, left, right):
        uID = left
        vID = right
        while (dfs.getPostID(uID) != dfs.getPostID(vID)):
            while (dfs.getPostID(uID) < dfs.getPostID(vID)):
                uID = self.__immediateDom[uID]
            while (dfs.getPostID(vID) < dfs.getPostID(uID)):
                vID = self.__immediateDom[vID]
        return uID
    
    def add_edges(self):
        for v in self.__directedg:
            if v.vertexID != self._rootID:
                assert self.__immediateDom[v.vertexID] != vertices.dummyID, "Immediate dominator of %d not set" % v.vertexID
                self.addEdge(self.__immediateDom[v.vertexID], v.vertexID)
                
class DominanceFrontiers:
    def __init__(self, cfg, domTree):
        self.__cfg     = cfg
        self.__domTree = domTree
        self.initialise()
        self.compute()
        
    def output(self):
        for v in self.__cfg:
            print "DF(%d) = %s" % (v.vertexID, self.__vToDF[v.vertexID])
        
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
    
    def initialise(self):
        self.__vToDF = {}
        for v in self.__cfg:
            self.__vToDF[v.vertexID] = set([])
    
    def compute(self):
        for v in self.__cfg:   
            if v.number_of_predecessors() > 1: 
                idomID = self.__domTree.getImmediateDominator(v.vertexID)
                for predID in v.predecessors.keys():
                    runnerID = predID
                    while runnerID != idomID:
                        self.__vToDF[runnerID].add(v.vertexID)
                        runnerID = self.__domTree.getImmediateDominator(runnerID)
    
class LoopNests (Tree):
    def __init__(self, directedg, rootID):
        Tree.__init__(self)
        assert rootID in directedg.the_vertices.keys(), "Unable to find vertex %d from which to initiate depth-first search" % rootID
        self.name = directedg.name
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
        assert self.numOfEdges() == self.numOfVertices() - 1, "Edges = %d, vertices = %d" % (self.numOfEdges(), self.numOfVertices()) 
        
    def _initialise (self):
        for v in self.__directedg:
            self.__parent[v.vertexID] = v.vertexID
            self.the_vertices[v.vertexID] = vertices.TreeVertex(v.vertexID)
            
    def _findLoops (self, rootID):
        self.__dfs = DepthFirstSearch (self.__directedg, rootID)
        for vertexID in reversed(self.__dfs.getPreorder()):
            v = self.__directedg.getVertex(vertexID)
            worklist = []
            for predID in v.predecessors.keys():
                if self.__dfs.isDFSBackedge(predID, vertexID):
                    assert self.__predomTree.isAncestor(vertexID, predID), "Non-reducible loop found with DFS backedge %d => %d" % (predID, vertexID)
                    if predID == vertexID:
                        debug.debug_message("%s => %s is a loop-back edge of trivial loop" % (predID, vertexID), 15)
                        self._addSelfLoop (vertexID)
                    else:
                        debug.debug_message("%s => %s is a loop-back edge of non-trivial loop" % (predID, vertexID), 15)
                        worklist.append(self.__parent[predID])
            
            if worklist:
                self._findLoop (worklist, vertexID)
                
    def _addSelfLoop (self, headerID):
        newVertexID = self.getNextVertexID()
        headerv = vertices.HeaderVertex(newVertexID, headerID)
        self.the_vertices[newVertexID] = headerv
        self.__headerVertices[headerID] = newVertexID
        self.__loopTails[headerID] = [headerID]
        self.__loopBodies[headerID] = [headerID]
        self.__selfLoopHeaders.append(headerID)
        self.addEdge(newVertexID, headerID)
            
    def _findLoop (self, worklist, headerID):
        if headerID not in self.__headerVertices.keys():
            newVertexID = self.getNextVertexID()
            headerv     = vertices.HeaderVertex(newVertexID, headerID)
            self.the_vertices[newVertexID] = headerv
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
            for predID in v.predecessors.keys():
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
            self.__loopExits[headerID] = set([])
            for vertexID in self.__loopBodies[headerID]:
                v = self.__directedg.getVertex(vertexID)
                for succID in v.successors.keys():
                    if succID not in self.__loopBodies[headerID]:
                        if headerID != vertexID and self.isLoopHeader(vertexID):
                            if succID not in self.__loopBodies[vertexID]:
                                self.__loopExits[headerID].add((vertexID, succID))
                        else:
                            self.__loopExits[headerID].add((vertexID, succID))
            debug.debug_message("Exits of %s = %s" % (headerID, self.__loopExits[headerID]), 15)
            
    def __str__ (self):
        string = "*" * 20 + " LNT Output " + "*" * 20 + "\n"
        for v in self.the_vertices.values():
            string += v.__str__()
        return string
    
    def isLoopHeader (self, vertexID):
        return vertexID in self.__headerVertices.keys()
    
    def getInternalHeaderVertex (self, vertexID):
        v = self.getVertex(vertexID)
        assert isinstance(v, vertices.HeaderVertex), "Vertex %s of LNT is not an internal header vertex" % vertexID
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
        for tails in self.__loopTails.values():
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
    
    def getLoopExitSources (self, headerID):
        assert headerID in self.__headerVertices.keys(), "Vertex %s is not a loop header" % headerID
        sources = set([])
        for predID, succID in self.__loopExits[headerID]:
            sources.add(predID)
        return sources
    
    def getLoopExitDestinations (self, headerID, exitID):
        assert headerID in self.__headerVertices.keys(), "Vertex %s is not a loop header" % headerID
        destinations = set([])
        for predID, succID in self.__loopExits[headerID]:
            destinations.add(succID)
        return destinations
    
    def getLoopBody (self, headerID):
        assert headerID in self.__headerVertices.keys(), "Vertex %s is not a loop header" % headerID
        return self.__loopBodies[headerID]
    
    def isLoopBackEdge (self, sourceID, destinationID):
        if destinationID not in self.__headerVertices.keys():
            return False
        else:
            return sourceID in self.__loopTails[destinationID]
    
    def isLoopExitSource (self, vertexID):
        for edges in self.__loopExits.values():
            for edge in edges:
                if edge[0] == vertexID:
                    return True
        return False
    
    def isLoopExitDestination (self, vertexID):
        for edges in self.__loopExits.values():
            for edge in edges:
                if edge[1] == vertexID:
                    return True
        return False
    
    def isLoopExitEdge (self, predID, succID):
        for headerID, edges in self.__loopExits.iteritems():
            for edge in edges:
                if edge[0] == predID and edge[1] == succID:
                    return headerID
        return None
    
    def isDoWhileLoop (self, headerID):
        assert headerID in self.__headerVertices.keys(), "Vertex %s is not a loop header" % headerID
        return set(self.__loopTails[headerID]) == set(edge[0] for edge in self.__loopExits[headerID])
    
    def isNested (self, left, right):
        return self.isProperAncestor(right, left)
    
    def getLoopExitPathsVertices (self, headerID):
        vertices = set([])
        worklist = []
        worklist.extend(self.getLoopExitSources(headerID))
        while worklist:
            vertexID = worklist.pop()
            vertices.add(vertexID)
            v        = self.__directedg.getVertex(vertexID)
            for predID in v.predecessors.keys():
                treePredv    = self.getVertex(predID)
                headerPredv  = self.getVertex(treePredv.getParentID())
                if not self.__dfs.isDFSBackedge(predID, vertexID):
                    if headerPredv.headerID == headerID:
                        worklist.append(predID)
                    elif self.isNested(headerPredv.vertexID, headerID):
                        worklist.append(headerPredv.headerID)
        return vertices
    
    def induceSubgraph (self, headerv):
        assert isinstance(headerv, vertices.HeaderVertex), "To induce the acyclic portion of a loop body, you must pass an internal vertex of the LNT."
        edges    = set([])
        analysed = set([])
        worklist = []
        worklist.extend(self.getLoopTails(headerv.headerID))
        while worklist:                        
            vertexID = worklist.pop()
            analysed.add(vertexID)
            v = self.__directedg.getVertex(vertexID)
            for predID in v.predecessors.keys():
                treePredv    = self.getVertex(predID)
                headerPredv  = self.getVertex(treePredv.parentID)
                predHeaderID = headerPredv.headerID
                if not self.__dfs.isDFSBackedge(predID, vertexID):
                    if predHeaderID == headerv.headerID:
                        if predID not in analysed:
                            worklist.append(predID)
                        edges.add((predID, vertexID))
                    elif self.isNested(headerPredv.vertexID, headerv.vertexID):
                        if predHeaderID not in analysed:
                            worklist.append(predHeaderID)
                        for sourceID, destinationID in self.getLoopExits(predHeaderID):
                            edges.add((sourceID, destinationID))
                            if predHeaderID != sourceID:
                                edges.add((predHeaderID, sourceID))
            # Add loop-exit edges to outer loops originating at this vertex
            for succID in v.successors.keys():
                treeSuccv    = self.getVertex(succID)
                headerSuccv  = self.getVertex(treeSuccv.parentID)
                succHeaderID = headerSuccv.headerID
                if succHeaderID != headerv.headerID and self.isNested(headerv.vertexID, headerSuccv.vertexID):
                    edges.add((vertexID, succID))
                
        flowg = directed_graphs.CFG()
        for predID, succID in edges:
            if not flowg.hasVertex(predID):
                v = vertices.CFGVertex(predID)
                flowg.addVertex(v)
            if not flowg.hasVertex(succID):
                v = vertices.CFGVertex(succID)
                flowg.addVertex(v)
            flowg.addEdge(predID, succID)
        # Add a dummy exit vertex to induced subgraph
        exitID = flowg.getNextVertexID()
        exitv  = vertices.CFGVertex(exitID)
        flowg.addVertex(exitv)
        exitv.dummy = True
        # Add edges from tails and exits to dummy exit vertex
        for v in flowg:
            if v.number_of_successors() == 0 and v.vertexID != exitID:
                flowg.addEdge(v.vertexID, exitID)
        # Set exit vertex of induced subgraph
        flowg.set_exitID(exitID)
        # Set entry vertex of induced subgraph
        flowg.set_entryID(headerv.headerID)
        return flowg

