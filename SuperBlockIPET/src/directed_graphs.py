import trees
import vertices
import edges
import debug
import utils
import copy

class DirectedGraph:        
    def __init__ (self):
        self.the_vertices = {}
        self.name = None
        
    def addVertex (self, v):
        assert v.vertexID not in self.the_vertices, "Adding vertex %d which is already in graph" % v.vertexID
        self.the_vertices[v.vertexID] = v
    
    def getVertex(self, vertexID):
        assert vertexID in self.the_vertices, "Vertex " + str(vertexID) + " is not in the graph"
        return self.the_vertices[vertexID]
    
    def removeVertex(self, vertexID):
        assert vertexID in self.the_vertices, "Vertex " + str(vertexID) + " is not in the graph"
        del self.the_vertices[vertexID]
    
    def hasVertex(self, vertexID):
        return vertexID in self.the_vertices
    
    def addEdge(self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        predv.add_successor(succID)
        succv.add_predecessor(predID)
        
    def hasEdge(self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        return predv.has_successor(succID) or succv.hasPredecessor(predID)
    
    def removeEdge(self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        predv.removeSuccessor(succID)
        succv.removePredecessor(predID)
        
    def add_predecessor_edges(self):
        for v in self:
            for succID in v.successors.keys():
                succv = self.getVertex(succID)
                if not succv.has_predecessor(v.vertexID):
                    succv.add_predecessor(v.vertexID)
    
    def getNextVertexID(self):
        nextID = 1
        while nextID in self.the_vertices.keys():
            nextID = nextID + 1 
        return nextID
    
    def numOfVertices(self):
        return len(self.the_vertices)
    
    def numOfEdges(self):
        total = 0
        for v in self.the_vertices.values():
            total += v.numberOfSuccessors()
        return total
    
    def __iter__ (self):
        return self.the_vertices.values().__iter__()
    
    def __str__ (self):
        string = "*" * 40 + "\n"
        for v in self.the_vertices.values():
            string += v.__str__()
        return string

class FlowGraph (DirectedGraph):
    def __init__ (self):
        DirectedGraph.__init__(self)
        self.entryID = vertices.dummyID
        self.exitID  = vertices.dummyID
        
    def get_entryID (self):
        assert self.entryID != vertices.dummyID, "Entry to flow graph not found"
        return self.entryID
    
    def get_exitID (self):
        assert self.exitID != vertices.dummyID, "Exit to flow graph not found"
        return self.exitID
        
    def get_reverse_graph(self):
        reverseg = FlowGraph() 
        # Add vertices
        for v in self:
            copyv = copy.copy(v)
            copyv.successors   = {}
            copyv.predecessors = {}
            reverseg.the_vertices[copyv.vertexID] = copyv
        # Add edges
        for v in self:
            predID = v.vertexID
            predv  = reverseg.getVertex(predID)
            for succID in v.successors.keys():
                succv = reverseg.getVertex(succID)
                predv.add_predecessor(succID)
                succv.add_successor(predID)
        # Set the entry and exit IDs
        reverseg.entryID = self.get_exitID()
        reverseg.exitID  = self.get_entryID()
        return reverseg
            
    def set_edgeIDs (self):
        edgeID = 1
        for v in self:
            for succID in v.successors.keys():
                succe = v.get_successor_edge(succID)
                succe.set_edgeID(edgeID)
                succv = self.getVertex(succID)
                prede = succv.get_predecessor_edge(v.vertexID)
                prede.set_edgeID(edgeID)
                edgeID += 1
        
class EnhancedCFG(FlowGraph):
    def __init__ (self, cfg):
        FlowGraph.__init__(self)
        self.name = cfg.name
        for v in cfg:
            newv = vertices.CFGVertex(v.vertexID, v.is_ipoint)
            self.the_vertices[v.vertexID] = newv
            if v.vertexID == cfg.get_entryID():
                self.entryID = v.vertexID
            if v.vertexID == cfg.get_exitID():
                self.exitID = v.vertexID
        assert self.entryID != vertices.dummyID
        assert self.exitID != vertices.dummyID
        newVertexID = 0
        for v in cfg:
            for succID in v.successors.keys():
                newVertexID -= 1
                newv        = vertices.CFGEdge(newVertexID, v.vertexID, succID)
                self.the_vertices[newVertexID] = newv
                self.addEdge(v.vertexID, newVertexID)
                self.addEdge(newVertexID, succID)
        
class CFG(FlowGraph):    
    def __init__ (self):
        FlowGraph.__init__(self)
        
    def set_entry_and_exit(self):
        without_predecessors = []
        without_successors   = []
        for v in self:
            if v.number_of_successors() == 0:
                without_successors.append(v.vertexID)
            if v.number_of_predecessors() == 0:
                without_predecessors.append(v.vertexID)
                
        entryID = None
        if len(without_predecessors) == 0:
            debug.exit_message("CFG '%s' does not have an entry point" % self.name)
        elif len(without_predecessors) > 1:
            debug_info = ""
            for bbID in without_predecessors:
                bb = self.getVertex(bbID)
                debug_info += bb.__str__()
            debug.exit_message("CFG '%s' has too many entry points: %s" % (self.name, debug_info))
        else:
            entryID = self.getNextVertexID()
            entryv  = vertices.CFGVertex(entryID, True)
            self.addVertex(entryv)
            self.set_entryID(entryID)
            self.addEdge(entryID, without_predecessors[0])
        
        exitID = None
        if len(without_successors) == 0:
            debug.exit_message("CFG '%s' does not have an exit point" % self.name)
        elif len(without_successors) > 1:
            debug_info = ""
            for bbID in without_successors:
                bb = self.getVertex(bbID)
                debug_info += bb.__str__()
            debug.exit_message("CFG '%s' has too many exit points: %s" % (self.name, debug_info))
        else:
            exitID = self.getNextVertexID()
            exitv  = vertices.CFGVertex(exitID, True)
            self.addVertex(exitv)
            self.set_exitID(exitID)
            self.addEdge(without_successors[0], exitID)
        assert entryID, "Unable to set entry ID"
        assert exitID, "Unable to set exit ID"
        self.addEdge(exitID, entryID)
        
    def set_entryID(self, entryID):
        assert entryID in self.the_vertices, "Cannot find vertex " + str(entryID) + " in vertices"
        assert entryID != vertices.dummyID, "Entry ID " + str(entryID) + " is not positive"
        self.entryID = entryID
        
    def set_exitID(self, exitID):
        assert exitID in self.the_vertices, "Cannot find vertex " + str(exitID) + " in vertices"
        assert exitID != vertices.dummyID, "Exit ID " + str(exitID) + " is not positive"
        self.exitID = exitID

class SuperBlockGraph (DirectedGraph):
    next_vertexID = 0
    next_edgeID   = 0
    
    def __init__ (self, cfg=None, lnt=None):
        DirectedGraph.__init__(self)
        if cfg and lnt:
            self.__cfg = cfg
            self.name = cfg.getName()
            self.__headerToSuperBlockSubgraph = {}
            self.__headerToSuperBlockRoot     = {}
            for level, vertices in lnt.levelIterator(True):
                for treev in vertices:
                    if isinstance(treev, vertices.HeaderVertex):
                        headerID = treev.getHeaderID()                    
                        debug.debug_message("Analysing header %d" % headerID, __name__, 1)
                        forwardCFG         = lnt.induceSubgraph(treev)
                        enhancedCFG        = EnhancedCFG(forwardCFG)
                        predomTree         = trees.Dominators(enhancedCFG, enhancedCFG.getEntryID())
                        reverseEnhancedCFG = enhancedCFG.getReverseGraph()
                        postdomTree        = trees.Dominators(reverseEnhancedCFG, reverseEnhancedCFG.getEntryID())
                        postDF             = trees.DominanceFrontiers(reverseEnhancedCFG, postdomTree)   
                        dominatorg         = DominatorGraph(predomTree, postdomTree)
                        sccs               = StrongComponents(dominatorg)      
                        self.__headerToSuperBlockSubgraph[headerID], vToSuperv = self.__addSuperBlocks(lnt, enhancedCFG, postdomTree, sccs, headerID)     
                        self.__headerToSuperBlockRoot[headerID] = self.__addEdges(lnt, enhancedCFG, predomTree, postdomTree, postDF, headerID, vToSuperv)
                        
    def __add_super_blocks (self, lnt, forwardCFG, postdomTree, sccs, headerID):
        loopBody      = lnt.getLoopBody(headerID)
        subgraph      = SuperBlockGraph()
        sccIDToVertex = {}
        vToSuperv     = {}
        for sccID in xrange(1, sccs.numberOfSCCs()+1):
            SuperBlockGraph.next_vertexID += 1
            superVertexID                = SuperBlockGraph.next_vertexID
            superv                       = vertices.SuperBlock(superVertexID)
            self.the_vertices[superVertexID] = superv
            sccIDToVertex[sccID]         = superv
            subgraph.vertices[superVertexID] = superv
        for v in forwardCFG:
            if not v.isDummy():
                vertexID = v.getVertexID()  
                sccID    = sccs.getSCCID(vertexID)
                superv   = sccIDToVertex[sccID]
                vToSuperv[vertexID] = superv
                if isinstance(v, vertices.CFGVertex):
                    if vertexID not in loopBody or (lnt.isLoopHeader(vertexID) and headerID != vertexID):
                        superv.outOfScopeBlocks.add(vertexID)
                    else:
                        superv.basicBlocks.add(vertexID)
                        superv.repBasicBlock = vertexID
                else:
                    predID = v.edge[0]
                    succID = v.edge[1]
                    predv  = forwardCFG.getVertex(predID)
                    succv  = forwardCFG.getVertex(succID)
                    if not predv.isDummy() and not succv.isDummy():
                        if lnt.isLoopExitEdge(v.edge[0], v.edge[1]):                            
                            if v.edge in lnt.getLoopExits(headerID):
                                superv.edges.add(v.edge)
                                superv.repEdge = v.edge
                            else:
                                superv.loopExitEdges.add(v.edge)
                        elif predID in loopBody or succID in loopBody:
                            superv.edges.add(v.edge)
                            superv.repEdge = v.edge                            
        return subgraph, vToSuperv
                
    def __addEdges (self, lnt, forwardCFG, predomTree, postdomTree, postDF, headerID, vToSuperv):
        rootSuperv = vToSuperv[headerID]
        dfs        = trees.DepthFirstSearch(forwardCFG, forwardCFG.getEntryID())
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
                        if not sourcev.has_successor(destinationv.getVertexID()):
                            SuperBlockGraph.next_edgeID += 1
                            succe = edges.BranchControlFlowEdge(destinationv.getVertexID(), vertexID)
                            prede = edges.BranchControlFlowEdge(sourcev.getVertexID(), vertexID)
                            sourcev.addSuccessorEdge(succe)
                            destinationv.addPredecessorEdge(prede)
                            succe.setEdgeID(SuperBlockGraph.next_edgeID)
                            prede.setEdgeID(SuperBlockGraph.next_edgeID)
            # Found a merge
            if v.numberOfPredecessors() > 1:
                ipreID = predomTree.getImmediateDominator(vertexID)
                if postdomTree.getImmediateDominator(ipreID) != vertexID:
                    destinationv = vToSuperv[vertexID]
                    if postDF.size(vertexID) > 1:
                        destinationv.unstructuredMerge = True
                    for predID in v.getPredecessorIDs():
                        sourcev = vToSuperv[predID]
                        SuperBlockGraph.next_edgeID += 1
                        succe = edges.MergeControlFlowEdge(destinationv.getVertexID())
                        prede = edges.MergeControlFlowEdge(sourcev.getVertexID())
                        sourcev.addSuccessorEdge(succe)
                        destinationv.addPredecessorEdge(prede)
                        succe.setEdgeID(SuperBlockGraph.next_edgeID)
                        prede.setEdgeID(SuperBlockGraph.next_edgeID)
        return rootSuperv
        
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
            self.the_vertices[vertexID] = vertices.Vertex(vertexID)        

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

class StrongComponents:
    Colors = utils.enum('WHITE', 'BLACK', 'GRAY', 'BLUE', 'RED')
    
    def __init__ (self, directedg):
        self.__directedg      = directedg
        self.__vertexToColour = {}
        self.__vertexToSCC    = {}
        self.__SCCToVertices  = {}
        # Initialise
        for v in directedg:
            vertexID = v.getVertexID()
            self.__vertexToColour[vertexID] = StrongComponents.Colors.WHITE
            self.__vertexToSCC[vertexID]    = 0
        # Depth-first search on forward graph
        self.__preCounter = 0
        vertexList        = []
        for v in directedg:
            vertexID = v.getVertexID()
            if self.__vertexToColour[vertexID] == StrongComponents.Colors.WHITE:
                self.__visit1(v, vertexList)
        # Depth-first search on reverse graph
        self.__sccCounter = 0
        self.__reverseg   = directedg.getReverseGraph()
        for vertexID in reversed(vertexList):
            if self.__vertexToColour[vertexID] == StrongComponents.Colors.BLACK:
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
            if self.__vertexToColour[vertexID] == StrongComponents.Colors.WHITE:
                self.__vertexToColour[vertexID] = StrongComponents.Colors.GRAY
                stack.append(poppedv)
                for succID in poppedv.getSuccessorIDs():
                    if self.__vertexToColour[succID] == StrongComponents.Colors.WHITE:
                        stack.append(self.__directedg.getVertex(succID))
            elif self.__vertexToColour[vertexID] == StrongComponents.Colors.GRAY:  
                self.__vertexToColour[vertexID] = StrongComponents.Colors.BLACK
                vertexList.append(vertexID)
                
    def __visit2 (self, v):
        stack = []
        stack.append(v)
        while stack:
            poppedv = stack.pop()
            vertexID = poppedv.getVertexID()
            self.__vertexToSCC[vertexID] = self.__sccCounter
            self.__SCCToVertices[self.__sccCounter].add(vertexID)
            if self.__vertexToColour[vertexID] == StrongComponents.Colors.BLACK:
                self.__vertexToColour[vertexID] = StrongComponents.Colors.BLUE
                stack.append(poppedv)
                for succID in poppedv.getSuccessorIDs():
                    if self.__vertexToColour[succID] == StrongComponents.Colors.BLACK:
                        stack.append(self.__reverseg.getVertex(succID))
            elif self.__vertexToColour[vertexID] == StrongComponents.Colors.BLUE:
                self.__vertexToColour[vertexID] = StrongComponents.Colors.RED
                
    def numberOfSCCs (self):
        return self.__sccCounter
    
    def getSCCID (self, vertexID):
        assert vertexID in self.__vertexToSCC, "Unable to find SCC of vertex %d" % vertexID
        return self.__vertexToSCC[vertexID]
    
    def getVertexIDs (self, sccID):
        assert sccID in self.__SCCToVertices, "Unable to find set of vertices associated with SCC ID %d" % sccID
        return self.__SCCToVertices[sccID]  
    