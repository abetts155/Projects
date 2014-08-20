import vertices
import debug
import copy

class DirectedGraph ():        
    def __init__ (self):
        self.vertices = {}
        self._name = None
    
    def setName (self, name):
        self._name = name
        
    def getName (self):
        return self._name
    
    def vertexIDs (self):
        return set(self.vertices.keys())
    
    def getVertex (self, vertexID):
        assert vertexID in self.vertices, "Vertex " + str(vertexID) + " is not in the graph"
        return self.vertices[vertexID]
    
    def removeVertex (self, vertexID):
        assert vertexID in self.vertices, "Vertex " + str(vertexID) + " is not in the graph"
        del self.vertices[vertexID]
    
    def hasVertex (self, vertexID):
        return vertexID in self.vertices
    
    def addEdge (self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        predv.addSuccessor(succID)
        succv.addPredecessor(predID)
        
    def hasEdge (self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        return predv.hasSuccessor(succID) or succv.hasPredecessor(predID)
    
    def removeEdge (self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        predv.removeSuccessor(succID)
        succv.removePredecessor(predID)
        
    def addPredecessoredges (self):
        for v in self:
            vertexID = v.vertexID
            for succID in v.getSuccessorIDs():
                succv = self.getVertex(succID)
                if not succv.hasPredecessor(vertexID):
                    succv.addPredecessor(vertexID)
    
    def getNextVertexID (self):
        nextID = 1
        while nextID in self.vertices.keys():
            nextID = nextID + 1 
        return nextID
    
    def numOfvertices (self):
        return len(self.vertices)
    
    def numOfedges(self):
        total = 0
        for v in self.vertices.values():
            total += v.numberOfSuccessors()
        return total
    
    def getReverseGraph (self):
        reverseg = DirectedGraph() 
        # Add vertices
        for v in self:
            vertexID = v.vertexID
            copyv    = vertices.Vertex(vertexID)
            reverseg.vertices[vertexID] = copyv
        # Add edges
        for v in self:
            predID = v.vertexID
            for succID in v.getSuccessorIDs():
                reverseg.addEdge(succID, predID)
        return reverseg
    
    def __iter__ (self):
        return self.vertices.values().__iter__()
    
    def __str__ (self):
        string = "*" * 40 + "\n"
        for v in self.vertices.values():
            string += v.__str__()
        return string

class FlowGraph (DirectedGraph):
    def __init__ (self):
        DirectedGraph.__init__(self)
        self._entryID = vertices.dummyVertexID
        self._exitID = vertices.dummyVertexID
        
    def getEntryID (self):
        assert self._entryID != vertices.dummyVertexID, "Entry to flow graph not found"
        return self._entryID
    
    def getExitID (self):
        return self._exitID
    
    def __str__ (self):
        string = "*" * 40 + "\n" + \
        "Entry ID = %s\n" % str(self._entryID) + \
        "Exit ID  = %s\n" % str(self._exitID) + "\n"
        for v in self.vertices.values():
            string += v.__str__()
        string += "*" * 40 + "\n"
        return string
    
class EnhancedCFG (FlowGraph):
    def __init__ (self, cfg=None):
        FlowGraph.__init__(self)
        self.__edgeToVertex = {}
        if cfg:
            self._name = cfg.getName()
            for v in cfg:
                newVertexID = v.vertexID
                newv        = vertices.Vertex(newVertexID)
                self.vertices[newVertexID] = newv
                if newVertexID == cfg.getEntryID():
                    self._entryID = newVertexID
                if newVertexID == cfg.getExitID():
                    self._exitID = newVertexID
            assert self._entryID != vertices.dummyVertexID
            assert self._exitID != vertices.dummyVertexID
            for v in cfg:
                vertexID = v.vertexID
                for succID in v.getSuccessorIDs():
                    newVertexID = self.getNextVertexID()
                    newv        = vertices.CFGEdge(newVertexID, vertexID, succID)
                    self.__edgeToVertex[(vertexID, succID)] = newv
                    self.vertices[newVertexID] = newv
                    self.addEdge(vertexID, newVertexID)
                    self.addEdge(newVertexID, succID)
                    
    def getVertexForCFGEdge (self, programPoint):
        assert programPoint in self.__edgeToVertex
        return self.__edgeToVertex[programPoint]
        
    def getReverseGraph (self):
        reverseg = EnhancedCFG() 
        # Add vertices
        for v in self:
            copyv = copy.copy(v)
            copyv.removeAllSuccessors()
            copyv.removeAllPredecessors()
            reverseg.vertices[copyv.vertexID] = copyv
        # Add edges
        for v in self:
            predID = v.vertexID
            predv  = reverseg.getVertex(predID)
            for succID in v.getSuccessorIDs():
                succv = reverseg.getVertex(succID)
                predv.addPredecessor(succID)
                succv.addSuccessor(predID)
        # Set the entry and exit IDs
        reverseg._entryID = self.getExitID()
        reverseg._exitID  = self.getEntryID()
        return reverseg 
                
    def __str__ (self):
        string = "*" * 20 + " Enhanced CFG Output " + "*" * 20 + "\n" + \
        "Entry ID = %s\n" % str(self._entryID) + \
        "Exit ID  = %s\n" % str(self._exitID) + "\n"
        for v in self.vertices.values():
            string += v.__str__() + "\n"
        return string
        
class CFG (FlowGraph):    
    def __init__ (self):
        FlowGraph.__init__(self)
        self.__addressToVertex = {}
        self.__callSites = {}
        
    def addCallSite (self, vertexID, calleeName):
        assert vertexID in self.vertices, "Vertex %d does not belong to the CFG of '%s'" % (vertexID, self._name)
        self.__callSites[vertexID] = calleeName
    
    def isCallSite (self, vertexID):
        return vertexID in self.__callSites
    
    def dumpCallSites (self):
        print self.__callSites
    
    def removeCallSite (self, vertexID):
        assert vertexID in self.__callSites, "Vertex %d is not a call site of '%s'" % (vertexID, self._name)
        del self.__callSites[vertexID]
        
    def getCalleeName (self, vertexID):
        assert vertexID in self.__callSites, "Vertex %d is not a call site of '%s'" % (vertexID, self._name)
        return self.__callSites[vertexID]
    
    def getReverseCFG (self):
        reverseg = CFG()
        # Add vertices
        for v in self:
            copyv = copy.copy(v)
            copyv.removeAllSuccessors()
            copyv.removeAllPredecessors()
            reverseg.addVertex(copyv)
        # Add edges
        for v in self:
            predID = v.vertexID
            predv  = reverseg.getVertex(predID)
            for succID in v.getSuccessorIDs():
                succv = reverseg.getVertex(succID)
                predv.addPredecessor(succID)
                succv.addSuccessor(predID)
        # Set the entry and exit IDs
        reverseg.setEntryID(self.getExitID())
        reverseg.setExitID(self.getEntryID())
        return reverseg
        
    def addVertex (self, bb):
        bbID = bb.vertexID
        assert bbID not in self.vertices, \
        "Adding basic block %d which is already in graph" % bbID
        self.vertices[bbID] = bb
        
    def getVertex (self, bbID):
        return DirectedGraph.getVertex(self, bbID)
    
    def getVertexWithAddress (self, address):
        if address in self.__addressToVertex:
            return self.__addressToVertex[address]
        for v in self:
            if v.hasAddress(address):
                self.__addressToVertex[address] = v
                return v
        assert False, "Unable to find basic block with address %s" % hex(address) 
        
    def setEntryID (self, entryID=None):
        if entryID is None:
            candidates = []
            toRemove   = []
            for bb in self.vertices.values():
                if bb.numberOfPredecessors() == 0:
                    candidates.append(bb)
            for bb in candidates:
                bbID  = bb.vertexID
                if self._entryID != vertices.dummyVertexID:
                    debug.warning_message("The entry ID has already been set to %d. Found another entry candidate %d" % (self._entryID, bbID))
                    currentEntryv = self.getVertex(self._entryID)
                    entryAddress  = currentEntryv.getFirstInstruction().getAddress()
                    firstAddress  = bb.getFirstInstruction().getAddress()
                    if firstAddress < entryAddress:
                        self._entryID = bbID
                        debug.warning_message("Resetting entry vertex to %d" % bbID)
                        toRemove.append(currentEntryv)
                    else:
                        toRemove.append(bb)
                else:
                    self._entryID = bbID
            for bb in toRemove:
                bbID = bb.vertexID
                for predID in bb.getPredecessorIDs():
                    predv = self.getVertex(predID)
                    predv.removeSuccessor(bbID)
                for succID in bb.getSuccessorIDs():
                    succv = self.getVertex(succID)
                    succv.removePredecessor(bbID)
                self.removeVertex(bbID)
            assert self._entryID != vertices.dummyVertexID, "Unable to find a vertex without predecessors to set as the exit in '%s'" % self._name
        else:
            assert entryID in self.vertices, "Cannot find vertex " + str(entryID) + " in vertices"
            assert entryID > vertices.dummyVertexID, "Entry ID " + str(entryID) + " is not positive"
            self._entryID = entryID
        
    def setExitID (self, exitID=None):
        if exitID is None:
            for bb in self.vertices.values():
                if bb.numberOfSuccessors() == 0:
                    bbID = bb.vertexID
                    assert self._exitID == vertices.dummyVertexID, "The exit ID has already been set to %d. Found another entry candidate %d" % (self._entryID, bbID)
                    self._exitID = bbID
            if self._exitID == vertices.dummyVertexID:
                debug.warning_message("Unable to find a vertex without successors to set as the exit in '%s'" % self._name)
        else:
            assert exitID in self.vertices, "Cannot find vertex " + str(exitID) + " in vertices"
            assert exitID > vertices.dummyVertexID, "Exit ID " + str(exitID) + " is not positive"
            self._exitID = exitID
            
    def addExitEntryEdge (self):
        if self._exitID != vertices.dummyVertexID:
            entryv = self.getVertex(self._entryID)
            exitv = self.getVertex(self._exitID)
            entryv.addPredecessor(self._exitID)
            exitv.addSuccessor(self._entryID)
            
    def getFirstInstruction (self):
        v = self.getVertex(self._entryID)
        return v.getFirstInstruction()
    
    def getLastInstruction (self):
        v = self.getVertex(self._exitID)
        return v.getLastInstruction()
        
    def setEdgeIDs (self):
        edgeID = 1
        for v in self:
            for succID in v.getSuccessorIDs():
                succe = v.getSuccessorEdge(succID)
                succe.setEdgeID(edgeID)
                succv = self.getVertex(succID)
                prede = succv.getPredecessorEdge(v.vertexID)
                prede.setEdgeID(edgeID)
                edgeID += 1
        
    def __str__ (self):
        string = "*" * 20 + " CFG Output " + "*" * 20 + "\n" + \
        "Entry ID = %s\n" % str(self._entryID) + \
        "Exit ID  = %s\n" % str(self._exitID) + "\n"
        for bb in self.vertices.values():
            string += bb.__str__() + "\n"
        return string


class Tree (DirectedGraph):
    def __init__ (self):
        DirectedGraph.__init__(self)
        self._rootID = vertices.dummyVertexID
        
    def setRootID (self, rootID):
        assert rootID > vertices.dummyVertexID, "Invalid root ID %d. It must be a positive integer" % rootID
        assert rootID in self.vertices, "Cannot find vertex %d"
        self._rootID = rootID
        
    def getRootID (self):
        assert self._rootID != vertices.dummyVertexID, "Root ID has not yet been set"
        return self._rootID
        
    def addVertex (self, vertexID):
        assert vertexID not in self.vertices, "Adding vertex %d which is already in tree" % vertexID
        treev = vertices.TreeVertex(vertexID)
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
        levelTovertices = {}
        while queue:
            v = queue.pop()
            for succID in v.getSuccessorIDs():
                queue.insert(0, self.getVertex(succID))
            
            if v.vertexID == self.getRootID():
                levelTovertices[0] = [rootv]
            else:
                newLevel = self.getVertex(v.getParentID()).getLevel() + 1
                v.setLevel(newLevel)
                if newLevel not in levelTovertices.keys():
                    levelTovertices[newLevel] = []
                levelTovertices[newLevel].append(v)
        
        if up:
            for level in reversed(sorted(levelTovertices.keys())):
                yield level, levelTovertices[level]
        else:
            for level in sorted(levelTovertices.keys()):
                yield level, levelTovertices[level]
                
    def __str__ (self):
        string = ""
        for level, vertices in self.levelIterator(False):
            string += "%s LEVEL = %d %s\n" % ('*' * 5, level, '*' * 5)
            for v in vertices:
                if level == 0:
                    string += "%d (root)\n" % (v.vertexID)
                else:
                    string += "%d (parent = %d)\n" % (v.vertexID, v.getParentID())
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
            print "ipre(%d) = %d" % (v.vertexID, self.__ipre[v].vertexID) 
            print "ipost(%d) = %d" % (v.vertexID, self.__ipost[v].vertexID)  
            
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
            if succ in self.__merges or succ.vertexID == self.__directedg.getExitID():
                break
            pred  = nextv
            nextv = succ
            succ  = self.__directedg.getVertex(nextv.getSuccessorIDs()[0])
            destination = succ
        print destination
        self.__directedg.removeEdge(source.vertexID, v.vertexID)
        self.__directedg.removeEdge(nextv.vertexID, destination.vertexID)
        self.__directedg.addEdge(source.vertexID,destination.vertexID)

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
            vertexID = v.vertexID
            if vertexID != dfs.getRootID():
                self.__parent[vertexID] = dfs.getVertex(vertexID).getParentID()
            self.__semi[vertexID] = dfs.getPreID(vertexID)
            self.__ancestor[vertexID] = None
            self.__idom[vertexID] = None
            self.__bucket[vertexID] = set([])
    
    def _compute (self, dfs):
        for preID in xrange(self.__directedg.numOfvertices(), 1, -1):
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
    
        for preID in xrange(2, self.__directedg.numOfvertices() + 1, 1):
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
    def __init__(self, directedg, rootID, succIDs=None):
        assert rootID in directedg.vertices.keys(), "Unable to find vertex %d from which to initiate depth-first search" % rootID
        Tree.__init__(self)
        self.__preorder = []
        self.__postorder = []
        self.__preorderID = 1
        self.__postorderID = 1
        self.__vertexID2PreID = {}
        self.__vertexID2PostID = {}
        self.__backedges = []
        self.initialise(directedg)
        self.setRootID(rootID)
        if succIDs:
            self.doSearch(directedg, rootID, succIDs)
        else:
            self.doSearch(directedg, rootID, directedg.getVertex(rootID).getSuccessorIDs())
        
    def initialise (self, directedg):
        for v in directedg:
            vertexID = v.vertexID
            self.vertices[vertexID] = vertices.TreeVertex(vertexID)
            self.__vertexID2PreID[vertexID] = 0
            self.__vertexID2PostID[vertexID] = 0
        
    def doSearch (self, directedg, vertexID, succIDs):
        self.__vertexID2PreID[vertexID] = self.__preorderID
        self.__preorder.append(vertexID)
        self.__preorderID += 1
        
        for succID in succIDs:
            if self.__vertexID2PreID[succID] == 0:
                self.addEdge(vertexID, succID)
                self.doSearch(directedg, succID, directedg.getVertex(succID).getSuccessorIDs())
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
                if v.numberOfPredecessors() == 0:
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
        debug.debug_message("Computing lca(%d, %d)" % (left, right), 20) 
        repID1      = self.__representative[left]
        repID2      = self.__representative[right]
        lowestLevel = self.__dummyLevel
        startIndex  = None
        endIndex    = None
        levelIndex  = 2 * self.__tree.numOfvertices()
        
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
        assert rootID in directedg.vertices.keys(), "Unable to find vertex %d from which to initiate depth-first search" % rootID
        Tree.__init__(self)
        self.__directedg = directedg
        self.__immediateDom = {}
        self._initialise (rootID)
        self._solveDFF ()
        self._addedges ()
        
    def getImmediateDominator (self, vertexID):
        assert vertexID != self._rootID, "Vertex %d does not have an immediate dominator as it is the root" % vertexID
        return self.getVertex(vertexID).getParentID()
    
    def _initialise (self, rootID):
        for v in self.__directedg:
            vertexID = v.vertexID            
            self.vertices[vertexID] = vertices.TreeVertex(vertexID)
            if vertexID == rootID:
                self.__immediateDom[vertexID] = vertexID
            else:
                self.__immediateDom[vertexID] = vertices.dummyVertexID
        self.setRootID(rootID)
    
    def _solveDFF (self):
        dfs = DepthFirstSearch(self.__directedg, self._rootID)
        changed = True
        while changed:
            changed = False
            postID = self.__directedg.numOfvertices()
            while postID >= 1:
                vertexID = dfs.getPostorderVertexID(postID)
                if vertexID != self._rootID:
                    v               = self.__directedg.getVertex(vertexID)
                    processedPredID = vertices.dummyVertexID
                    newIdomID       = vertices.dummyVertexID
                    
                    for predID in v.getPredecessorIDs():
                        if self.__immediateDom[predID] != vertices.dummyVertexID:
                            processedPredID = predID
                            newIdomID       = processedPredID
                    for predID in v.getPredecessorIDs():
                        if predID != processedPredID:
                            if self.__immediateDom[predID] != vertices.dummyVertexID:
                                newIdomID = self._intersect(dfs, predID, newIdomID)
                    
                    if newIdomID != vertices.dummyVertexID:
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
    
    def _addedges (self):
        for v in self.__directedg:
            vertexID = v.vertexID
            if vertexID != self._rootID:
                assert self.__immediateDom[vertexID] != vertices.dummyVertexID, "Immediate dominator of %d not set" % vertexID
                self.addEdge(self.__immediateDom[vertexID], vertexID)
                
class DominanceFrontiers:
    def __init__(self, cfg, domTree):
        self.__cfg     = cfg
        self.__domTree = domTree
        self.__initialise()
        self.__compute()
        
    def output(self):
        for v in self.__cfg:
            vertexID = v.vertexID
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
            self.__vToDF[v.vertexID] = set([])
    
    def __compute(self):
        for v in self.__cfg:
            vertexID = v.vertexID    
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
        self.__headervertices = {}
        self.__loopExits = {}
        self.__selfLoopHeaders = []
        self._initialise ()
        self._findLoops (rootID)
        self._findExits ()
        # Set the tree root ID to the header vertex representing the root of the 
        # directed graph
        self.setRootID(self.__headervertices[rootID])
        
    def _initialise (self):
        for v in self.__directedg:
            vertexID = v.vertexID
            self.__parent[vertexID] = vertexID
            self.vertices[vertexID] = vertices.TreeVertex(vertexID)
            
    def _findLoops (self, rootID):
        self.__dfs = DepthFirstSearch (self.__directedg, rootID)
        for vertexID in reversed(self.__dfs.getPreorder()):
            v = self.__directedg.getVertex(vertexID)
            worklist = []
            for predID in v.getPredecessorIDs():
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
        self.vertices[newVertexID] = headerv
        self.__headervertices[headerID] = newVertexID
        self.__loopTails[headerID] = [headerID]
        self.__loopBodies[headerID] = [headerID]
        self.__selfLoopHeaders.append(headerID)
        self.addEdge(newVertexID, headerID)
            
    def _findLoop (self, worklist, headerID):
        if headerID not in self.__headervertices.keys():
            newVertexID = self.getNextVertexID()
            headerv     = vertices.HeaderVertex(newVertexID, headerID)
            self.vertices[newVertexID] = headerv
            self.__headervertices[headerID] = newVertexID
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
            parentID = self.__headervertices[headerID]
            for vertexID in loopBody:
                self.__parent[vertexID] = headerID
                if vertexID in self.__headervertices.keys():
                    childID = self.__headervertices[vertexID]
                    self.addEdge(parentID, childID)
                else:
                    self.addEdge(parentID, vertexID)
            self.addEdge(parentID, headerID)
            self.__loopBodies[headerID] = [headerID]
            self.__loopBodies[headerID].extend([v for v in loopBody])
            
    def _findExits (self):
        for headerID in self.__headervertices.keys():
            self.__loopExits[headerID] = {}
            for vertexID in self.__loopBodies[headerID]:
                v = self.__directedg.getVertex(vertexID)
                for succID in v.getSuccessorIDs():
                    if succID not in self.__loopBodies[headerID]:
                        if headerID != vertexID and self.isLoopHeader(vertexID):
                            if succID not in self.__loopBodies[vertexID]:
                                self.__loopExits[headerID][vertexID] = []
                                self.__loopExits[headerID][vertexID].append(succID)
                        else:
                            self.__loopExits[headerID][vertexID] = []
                            self.__loopExits[headerID][vertexID].append(succID)
            debug.debug_message("Exits of %s = %s" % (headerID, self.__loopExits[headerID].keys()), 15)
            
    def __str__ (self):
        string = "*" * 20 + " LNT Output " + "*" * 20 + "\n"
        for v in self.vertices.values():
            string += v.__str__()
        return string
    
    def isLoopHeader (self, vertexID):
        return vertexID in self.__headervertices.keys()
    
    def getInternalHeaderVertex (self, vertexID):
        v = self.getVertex(vertexID)
        assert isinstance(v, vertices.HeaderVertex), "Vertex %s of LNT is not an internal header vertex" % vertexID
        return v
    
    def isSelfLoopHeader (self, vertexID):
        return vertexID in self.__selfLoopHeaders
        
    def getHeaderIDs (self):
        return self.__headervertices.keys()
    
    def getNumberOfHeaders (self):
        return len(self.__headervertices.keys())
    
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
        assert headerID in self.__headervertices.keys(), "Vertex %s is not a loop header" % headerID
        return self.__loopTails[headerID]
    
    def numberOfLoopTails (self, headerID):
        assert headerID in self.__headervertices.keys(), "Vertex %s is not a loop header" % headerID
        return len(self.__loopTails[headerID])
    
    def getLoopExitSources (self, headerID):
        assert headerID in self.__headervertices.keys(), "Vertex %s is not a loop header" % headerID
        return self.__loopExits[headerID].keys()
    
    def getLoopExitDestinations (self, headerID, exitID):
        assert headerID in self.__headervertices.keys(), "Vertex %s is not a loop header" % headerID
        assert exitID in self.__loopExits[headerID].keys(), "Vertex %s is not an exit of loop with header %d" % (exitID, headerID)
        return self.__loopExits[headerID][exitID]
    
    def isLoopExitEdge (self, predID, succID):
        for headerID in self.__headervertices:
            if self.isLoopExitOfLoop(predID, headerID):
                if succID in self.__loopExits[headerID][predID]:
                    return headerID
        return None
    
    def getLoopBody (self, headerID):
        assert headerID in self.__headervertices.keys(), "Vertex %s is not a loop header" % headerID
        return self.__loopBodies[headerID]
    
    def isLoopBackEdge (self, sourceID, destinationID):
        if destinationID not in self.__headervertices.keys():
            return False
        else:
            return sourceID in self.__loopTails[destinationID]
        
    def isLoopExitOfLoop (self, vertexID, headerID):
        assert headerID in self.__headervertices.keys(), "Vertex %s is not a loop header" % headerID
        return vertexID in self.__loopExits[headerID]
    
    def isLoopExit (self, vertexID):
        for headerID in self.__headervertices:
            if self.isLoopExitOfLoop(vertexID, headerID):
                return True
        return False
    
    def isDoWhileLoop (self, headerID):
        assert headerID in self.__headervertices.keys(), "Vertex %s is not a loop header" % headerID
        return set(self.__loopTails[headerID]) == set(self.__loopExits[headerID])
    
    def isNested (self, left, right):
        return self.isProperAncestor(right, left)
    
    def induceSubgraph (self, headerv):
        assert isinstance(headerv, vertices.HeaderVertex), "To induce the acyclic portion of a loop body, you must pass an internal vertex of the LNT."
        headerID     = headerv.getHeaderID()
        inducedCFG   = CFG()
        vertices     = set([])
        edges        = set([])
        bodyvertices = set([])
        bodyedges    = set([])
        worklist     = []
        bodyvertices.add(headerID)
        worklist.extend(self.getLoopTails(headerID))
        while worklist:
            vertexID = worklist.pop()
            vertices.add(vertexID)
            if not inducedCFG.hasVertex(vertexID):
                originalv = self.__directedg.getVertex(vertexID)
                for predID in originalv.getPredecessorIDs():
                    treePredv    = self.getVertex(predID)
                    headerPredv  = self.getVertex(treePredv.getParentID())
                    predHeaderID = headerPredv.getHeaderID()
                    if not self.__dfs.isDFSBackedge(predID, vertexID):
                        if predHeaderID == headerID:
                            worklist.append(predID)
                            bodyvertices.add(predID)
                            edges.add((predID, vertexID))
                            bodyedges.add((predID, vertexID))
                        elif self.isNested(headerPredv.vertexID, headerv.vertexID):
                            worklist.append(predHeaderID)     
                            vertices.add(predID)      
                            if predHeaderID != predID:         
                                edges.add((predHeaderID, predID))
                            edges.add((predID, vertexID))
                            bodyedges.add((predID, vertexID))
        
        # Add vertices to induced subgraph
        for vertexID in vertices:
            bb = vertices.BasicBlock(vertexID)
            inducedCFG.addVertex(bb)
        # Add edges to induced subgraph
        for (predID, succID) in edges:
            inducedCFG.addEdge(predID, succID)
        # Ensure the induced subgraph has a single exit point
        exits = []
        for v in inducedCFG:
            if v.numberOfSuccessors() == 0:
                exits.append(v)
        assert exits, "No exits found in header region %d" % headerID
        if len(exits) > 1:
            # Add a dummy exit vertex to induced subgraph
            exitID = inducedCFG.getNextVertexID()
            bb = vertices.BasicBlock(exitID)
            inducedCFG.addVertex(bb)
            bb.setDummy()
            for v in exits:
                inducedCFG.addEdge(v.vertexID, exitID)
            # Set exit vertex of induced subgraph
            inducedCFG.setExitID(exitID)
        else:
            inducedCFG.setExitID(exits[0].vertexID)
        # Set entry vertex of induced subgraph
        inducedCFG.setEntryID(headerv.getHeaderID())
        return inducedCFG, bodyvertices, bodyedges
    
