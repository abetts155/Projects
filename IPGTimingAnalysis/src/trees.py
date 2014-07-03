import directed_graphs
import cfgs
import vertices
import debug

class Tree(directed_graphs.DirectedGraph):
    def __init__ (self):
        directed_graphs.DirectedGraph.__init__(self)
        self.rootID = vertices.dummyID
        
    def set_rootID (self, rootID):
        assert rootID > vertices.dummyID, "Invalid root ID %d. It must be a positive integer" % rootID
        assert rootID in self.the_vertices, "Cannot find vertex %d"
        self.rootID = rootID
        
    def get_rootID (self):
        assert self.rootID != vertices.dummyID, "Root ID has not yet been set"
        return self.rootID
        
    def addVertex (self, vertexID):
        assert vertexID not in self.the_vertices, "Adding vertex %d which is already in tree" % vertexID
        treev = vertices.TreeVertex(vertexID)
        self.the_vertices[vertexID] = treev
        
    def addEdge (self, predID, succID):
        directed_graphs.DirectedGraph.addEdge(self, predID, succID)
        succv = self.getVertex(succID)
        succv.parentID = predID
        
    def isInternalVertex (self, vertexID):
        return self.getVertex(vertexID).number_of_successors() > 0
    
    def getAllProperAncestors (self, vertexID):
        ancestors = []
        while vertexID != self.rootID:
            parentID = self.getVertex(vertexID).get_parentID()
            ancestors.append(self.getVertex(parentID))
            vertexID = parentID
        return ancestors
    
    def isAncestor (self, left, right):
        if left == right:
            return True
        elif right == self.rootID:
            return False
        else:
            vertexID = right
            parentID = self.getVertex(vertexID).get_parentID()
            while parentID != self.rootID and parentID != left:
                vertexID = parentID
                parentID = self.getVertex(vertexID).get_parentID()
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
        rootv = self.getVertex(self.get_rootID())
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
        
    def levelIterator (self, up=True):
        rootv = self.getVertex(self.get_rootID())
        rootv.level = 0
        queue = [rootv]
        levelToVertices = {}
        while queue:
            v = queue.pop()
            for succID in v.successors.keys():
                queue.insert(0, self.getVertex(succID))
            
            if v.vertexID == self.get_rootID():
                levelToVertices[0] = [rootv]
            else:
                newLevel = self.getVertex(v.get_parentID()).level + 1
                v.level = newLevel
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
        the_string = ""
        for level, the_vertices in self.levelIterator(False):
            the_string += "%s LEVEL = %d %s\n" % ('*' * 5, level, '*' * 5)
            for v in the_vertices:
                if level == 0:
                    the_string += "%d (root)\n" % (v.vertexID)
                else:
                    the_string += "%d (parent = %d)\n" % (v.vertexID, v.get_parentID())
        return the_string
        
class DepthFirstSearch (Tree):
    def __init__(self, directedg, rootID):
        assert rootID in directedg.the_vertices.keys(), "Unable to find vertex %d from which to initiate depth-first search" % rootID
        Tree.__init__(self)
        self.__preorder = []
        self.__postorder = []
        self.__preorderID = 1
        self.__postorderID = 1
        self.__vertexID2PreID = {}
        self.__vertexID2PostID = {}
        self.__backedges = []
        self.initialise (directedg)
        self.set_rootID(rootID)
        self.doSearch (directedg, rootID)
        
    def initialise (self, directedg):
        for v in directedg:
            vertexID = v.vertexID
            self.the_vertices[vertexID] = vertices.TreeVertex(vertexID)
            self.__vertexID2PreID[vertexID] = 0
            self.__vertexID2PostID[vertexID] = 0
        
    def doSearch (self, directedg, vertexID):
        self.__vertexID2PreID[vertexID] = self.__preorderID
        self.__preorder.append(vertexID)
        self.__preorderID += 1
        
        v = directedg.getVertex(vertexID)
        for succID in v.successors.keys ():
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
    
class LoopNests (Tree):
    def __init__(self, directedg, rootID):
        assert rootID in directedg.the_vertices.keys(), "Unable to find vertex %d from which to initiate depth-first search" % rootID
        Tree.__init__(self)
        self.name = directedg.name
        self.__directedg = directedg
        self.__parent = {}
        self.__loopBodies = {}
        self.__loopTails = {}
        self.__headerVertices = {}
        self.__loopExits = {}
        self.__selfLoopHeaders = []
        self.initialise()
        self.find_loops(rootID)
        self.find_loop_exits()
        # Set the tree root ID to the header vertex representing the root of the 
        # directed graph
        self.set_rootID(self.__headerVertices[rootID])
        
    def initialise(self):
        for v in self.__directedg:
            self.__parent[v.vertexID] = v.vertexID
            treev = vertices.TreeVertex(v.vertexID)
            self.the_vertices[v.vertexID] = treev
            if isinstance(v, vertices.CFGEdge):
                treev.edge = v.edge
            if isinstance(v, vertices.CFGVertex):
                if v.is_ipoint:
                    treev.is_ipoint = v.is_ipoint
            
    def find_loops(self, rootID):
        self.__dfs = DepthFirstSearch (self.__directedg, rootID)
        for vertexID in reversed(self.__dfs.getPreorder()):
            v = self.__directedg.getVertex(vertexID)
            worklist = []
            for predID in v.predecessors.keys():
                if self.__dfs.isDFSBackedge(predID, vertexID):
                    if predID == vertexID:
                        debug.debug_message("%s => %s is a loop-back edge of trivial loop" % (predID, vertexID), __name__, 3)
                        self.add_self_loop(vertexID)
                    else:
                        debug.debug_message("%s => %s is a loop-back edge of non-trivial loop" % (predID, vertexID), __name__, 3)
                        worklist.append(self.__parent[predID])
            
            if worklist:
                self.build_loop_body(worklist, vertexID)
                
    def add_self_loop(self, headerID):
        newVertexID = self.getNextVertexID()
        headerv = vertices.HeaderVertex(newVertexID, headerID)
        self.the_vertices[newVertexID] = headerv
        self.__headerVertices[headerID] = newVertexID
        self.__loopTails[headerID] = [headerID]
        self.__loopBodies[headerID] = [headerID]
        self.__selfLoopHeaders.append(headerID)
        self.addEdge(newVertexID, headerID)
            
    def build_loop_body(self, worklist, headerID):
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
            
    def find_loop_exits(self):
        for headerID in self.__headerVertices.keys():
            self.__loopExits[headerID] = []
            for vertexID in self.__loopBodies[headerID]:
                v = self.__directedg.getVertex(vertexID)
                for succID in v.successors.keys():
                    if succID not in self.__loopBodies[headerID]:
                        if headerID != vertexID and self.isLoopHeader(vertexID):
                            if succID not in self.__loopBodies[vertexID]:
                                self.__loopExits[headerID].append(vertexID)
                        else:
                            self.__loopExits[headerID].append(vertexID)
            debug.debug_message("Exits of %s = %s" % (headerID, self.__loopExits[headerID]), __name__, 4)
            
    def __str__ (self):
        string = "*" * 20 + " LNT Output " + "*" * 20 + "\n"
        for v in self.the_vertices.values():
            string += v.__str__()
        return string
    
    def isLoopHeader (self, vertexID):
        return vertexID in self.__headerVertices.keys()
    
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
    
    def induce_subgraph (self, headerv):
        assert isinstance(headerv, vertices.HeaderVertex), "To induce the acyclic portion of a loop body, you must pass an internal vertex of the LNT"
        flowg    = cfgs.ICFG()
        edges    = {}
        worklist = []
        worklist.extend(self.getLoopTails(headerv.headerID))
        while worklist:
            vertexID = worklist.pop()
            if not flowg.hasVertex(vertexID): 
                originalv = self.__directedg.getVertex(vertexID)
                # Add the correct vertex type to the induced graph
                if originalv.is_ipoint:
                    newv = vertices.CFGVertex(vertexID, True)
                else:
                    newv = vertices.CFGVertex(vertexID, False)
                flowg.addVertex(newv)
                # Now discover which edges are incident to this vertex
                edges[vertexID] = set([])                       
                for predID in originalv.predecessors.keys():
                    treePredv    = self.getVertex(predID)
                    headerPredv  = self.getVertex(treePredv.get_parentID())
                    predHeaderID = headerPredv.headerID
                    if not self.__dfs.isDFSBackedge(predID, vertexID):
                        if predHeaderID == headerv.headerID:
                            worklist.append(predID)
                            edges[vertexID].add(predID)
                        elif self.isNested(headerPredv.vertexID, headerv.vertexID):
                            worklist.append(predHeaderID)
                            edges[vertexID].add(predHeaderID)
        # Add edges in induced subgraph
        for vertexID, predIDs in edges.items():
            for predID in predIDs:
                flowg.addEdge(predID, vertexID) 
        # Add exit vertex to induced subgraph
        noSuccs = []
        for v in flowg:
            if v.number_of_successors() == 0:
                noSuccs.append(v.vertexID)
        if len(noSuccs) != 1:
            exitID = flowg.getNextVertexID()
            bb = vertices.CFGVertex(exitID, False)
            flowg.addVertex(bb)
            flowg.setExitID(exitID)
            flowg.getVertex(exitID).setDummy()
            for predID in noSuccs:
                flowg.addEdge(predID, exitID)
        else:
            exitID = noSuccs[0]
            flowg.setExitID(exitID)
        # Add entry vertex to induced subgraph
        flowg.setEntryID(headerv.headerID)
        return flowg
    
    