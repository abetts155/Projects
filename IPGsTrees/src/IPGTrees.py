import Trees, Vertices, UDrawGraph, Debug, DirectedGraphs
import copy

class MaxVertex (Vertices.TreeVertex):
    pass

class IterationEdgeVertex (Vertices.TreeVertex):
    def __init__ (self, vertexID, sourceID, destinationID):
        Vertices.TreeVertex.__init__(self, vertexID)
        self.__sourceID = sourceID
        self.__destinationID = destinationID
    
    def getSourceID (self):
        return self.__sourceID
    
    def getDestinationID (self):
        return self.__destinationID

class LoopBoundVertex (Vertices.TreeVertex):
    def __init__ (self, vertexID, expr):
        Vertices.TreeVertex.__init__(self, vertexID)
        self.__expr = expr
        
    def getExpr (self):
        return self.__expr

class IPGTree (DirectedGraphs.DirectedGraph):    
    def __init__ (self, basename, icfg, lnt, ipg):
        DirectedGraphs.DirectedGraph.__init__(self)
        self.__ipg = ipg
        self.__headerToForwardIPG        = {}
        self.__headerToIterationEdgeTree = {}
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, Vertices.HeaderVertex) and level > 0:
                    Debug.debugMessage("Analysing header %d" % v.getHeaderID(), 1)
                    forwardICFG = lnt.induceSubgraph(v)
                    UDrawGraph.makeUdrawFile (forwardICFG, "%s.%s.Header%d.%s" % (basename, icfg.getName(), v.getHeaderID(), "icfg"))
                    forwardIPG = ForwardIPG(forwardICFG)
                    UDrawGraph.makeUdrawFile (forwardIPG, "%s.%s.Header%d.%s" % (basename, icfg.getName(), v.getHeaderID(), "ipg"))
                    self.__headerToForwardIPG[v.getHeaderID()] = forwardIPG
                    self.__addForwardIPGToTree(forwardIPG)
                    iterationEdgeTree = IterationEdgeTree(forwardIPG)
                    self.__headerToIterationEdgeTree[v.getHeaderID()] = iterationEdgeTree 
                    UDrawGraph.makeUdrawFile (iterationEdgeTree, "%s.%s.%s" % (basename, icfg.getName(), "itree"))
        
    def __addForwardIPGToTree (self, forwardIPG):
        toRemove = []
        for v in forwardIPG:
            if not v.isGhost():
                vertexID = v.getVertexID()
                copyv    = copy.deepcopy(v)
                self.vertices[vertexID] = copyv
                for predID in v.getPredecessorIDs():
                    predv = forwardIPG.getVertex(predID)
                    if predv.isGhost():
                        copyv.removePredecessor(predID)
                for succID in v.getSuccessorIDs():
                    succv = forwardIPG.getVertex(succID)
                    if succv.isGhost():
                        copyv.removeSuccessor(succID)
                if copyv.numberOfSuccessors() == 0 and copyv.numberOfPredecessors() == 0:
                    toRemove.append(vertexID)
        for vertexID in toRemove:
            self.removeVertex(vertexID)
            
    def getForwardIPG (self, headerID):
        assert headerID in self.__headerToForwardIPG, "Unable to find forward IPG region of loop %d" % (headerID)
        return self.__headerToForwardIPG[headerID]
    
    def getIterationEdgeTree (self, headerID):
        assert headerID in self.__headerToIterationEdgeTree, "Unable to find iteration tree for loop %d" % (headerID)
        return self.__headerToIterationEdgeTree[headerID]
            
class IterationEdgeTree (Trees.Tree):
    constantBound         = "1"
    selfLoopBound         = "n - 1"
    selfLoopMinusOneBound = "n - 2"
    cycleBound            = "ceil((n - 1)/2)"
    cycleBackBound        = "n - 1 - ceil((n - 1)/2)"
    
    def __init__ (self, forwardIPG): 
        Trees.Tree.__init__(self)
        maxv = self.__createMaxVertex()
        for sourceID in forwardIPG.getIterationEdgeSources():
            for destinationID in forwardIPG.getIterationEdgeDestinations():
                Debug.debugMessage("(%d, %d) is an iteration edge" % (sourceID, destinationID), 1)
                if sourceID == destinationID:
                    iterationEdgev = self.__createIterationEdgeVertex(sourceID, destinationID)
                    loopBoundv     = self.__createLoopBoundExprVertex(IterationEdgeTree.selfLoopBound)
                    self.__addEdge(maxv, iterationEdgev)
                    self.__addEdge(iterationEdgev, loopBoundv)
                else:
                    iterationEdgev  = self.__createIterationEdgeVertex(sourceID, destinationID)
                    self.__addEdge(maxv, iterationEdgev)
                    # Create first branch out of iteration edge
                    loopBoundv1     = self.__createLoopBoundExprVertex(IterationEdgeTree.cycleBound)
                    iterationEdgev2 = self.__createIterationEdgeVertex(destinationID, sourceID)
                    loopBoundv2     = self.__createLoopBoundExprVertex(IterationEdgeTree.cycleBackBound)
                    self.__addEdge(iterationEdgev, loopBoundv1)
                    self.__addEdge(loopBoundv1, iterationEdgev2)
                    self.__addEdge(iterationEdgev2, loopBoundv2)
                    # Create second branch out of iteration edge
                    loopBoundv3     = self.__createLoopBoundExprVertex(IterationEdgeTree.constantBound)
                    iterationEdgev3 = self.__createIterationEdgeVertex(destinationID, destinationID)
                    loopBoundv4     = self.__createLoopBoundExprVertex(IterationEdgeTree.selfLoopMinusOneBound)
                    self.__addEdge(iterationEdgev, loopBoundv3)
                    self.__addEdge(loopBoundv3, iterationEdgev3)
                    self.__addEdge(iterationEdgev3, loopBoundv4)
        self.setRootID(maxv.getVertexID())
                
    def __createMaxVertex (self):
        vertexID = self.getNextVertexID()
        v        = MaxVertex(vertexID)
        self.vertices[vertexID] = v
        return v            
        
    def __createIterationEdgeVertex (self, sourceID, destinationID):
        vertexID = self.getNextVertexID()
        v        = IterationEdgeVertex(vertexID, sourceID, destinationID)
        self.vertices[vertexID] = v
        return v
    
    def __createLoopBoundExprVertex (self, expr):
        vertexID = self.getNextVertexID()
        v        = LoopBoundVertex(vertexID, expr)
        self.vertices[vertexID] = v
        return v
    
    def __addEdge (self, sourcev, destinationv):
        from Edges import Edge
        prede = Edge(sourcev.getVertexID())
        succe = Edge(destinationv.getVertexID())
        sourcev.addSuccessorEdge(succe)
        destinationv.addPredecessorEdge(prede)  
        destinationv.setParentID(sourcev.getVertexID())  
    
class ForwardIPG (DirectedGraphs.FlowGraph):
    edgeID = 1
    
    def __init__ (self, icfg=None):
        DirectedGraphs.FlowGraph.__init__(self)
        if icfg:
            self.__vertexToReachable = {}
            self.__iterationEdgeDestinations = set([])
            self.__iterationEdgeSources = set([])
            self.__addIpoints(icfg)
            self.__addAcyclicEdges(icfg)    
            self.__guaranteeSingleEntryAndSingleExit() 
            
    def __addIpoints (self, icfg):    
        for v in icfg:
            vertexID = v.getVertexID()
            self.__vertexToReachable[vertexID] = set([])  
            if vertexID == icfg.getEntryID() and not icfg.isIpoint(vertexID):
                self.__vertexToReachable[vertexID].add(vertexID)          
            # Ipoint actions
            if icfg.isIpoint(vertexID):
                ipointv = Vertices.Ipoint(vertexID, v.getIpointID())
                self.vertices[vertexID] = ipointv
            
    def __guaranteeSingleEntryAndSingleExit (self):
        entries = []
        exits   = []
        for v in self:
            if v.numberOfPredecessors() == 0:
                entries.append(v.getVertexID())
            if v.numberOfSuccessors() == 0:
                exits.append(v.getVertexID())
        if len(entries) > 1:
            entryID       = self.getNextVertexID()
            self._entryID = entryID
            ipointv       = Vertices.Ipoint(entryID, entryID)
            ipointv.setGhost()
            self.vertices[entryID] = ipointv
            for succID in entries:
                self.__addEdge(entryID, succID, True)
        else:
            self._entryID = entries[0]
        if len(exits) > 1:
            exitID       = self.getNextVertexID()
            self._exitID = exitID
            ipointv      = Vertices.Ipoint(exitID, exitID)
            ipointv.setGhost()
            self.vertices[exitID] = ipointv
            for predID in exits:
                self.__addEdge(predID, exitID, True)
        else:
            self._exitID = exits[0]
                
    def __addAcyclicEdges (self, icfg):
        # Compute a topological sort on the ICFG
        dfs = Trees.DepthFirstSearch(icfg, icfg.getEntryID())
        # Perform data-flow analysis
        changed = True
        while changed:
            changed = False
            for vertexID in reversed(dfs.getPostorder()):
                v = icfg.getVertex(vertexID)
                for predID in v.getPredecessorIDs():
                    if icfg.isIpoint(predID):
                        if icfg.isIpoint(vertexID):
                            self.__addEdge(predID, vertexID)
                        else:
                            self.__vertexToReachable[vertexID].add(predID)
                    else:
                        for keyID in self.__vertexToReachable[predID]:
                            if icfg.isIpoint(keyID) and icfg.isIpoint(vertexID):
                                self.__addEdge(keyID, vertexID)
                            elif not icfg.isIpoint(keyID) and icfg.isIpoint(vertexID):
                                Debug.debugMessage("%d is an iteration edge DESTINATION" % vertexID, 1)
                                self.__iterationEdgeDestinations.add(vertexID)
                            else:
                                self.__vertexToReachable[vertexID].add(keyID)
                if v.numberOfSuccessors() == 0:
                    for keyID in self.__vertexToReachable[vertexID]:
                        if icfg.isIpoint(keyID):
                            Debug.debugMessage("%d is an iteration edge SOURCE" % keyID, 1)
                            self.__iterationEdgeSources.add(keyID)
                            
    def __addEdge (self, predID, succID,isDummy=False):
        from Edges import IPGEdge
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        if not predv.hasSuccessor(succID):
            assert not succv.hasPredecessor(predID), "Vertex %d has predecessor %d although vertex %d does not have successor %d" % (succID, predID, predID, succID)
            predv.addIpointSuccessor (succv.getIpointID(), succID)
            succe = IPGEdge(succID, ForwardIPG.edgeID, isDummy)
            prede = IPGEdge(predID, ForwardIPG.edgeID, isDummy)
            predv.addSuccessorEdge(succe)
            succv.addPredecessorEdge(prede) 
            ForwardIPG.edgeID += 1
            
    def getReverseIPG (self):
        reverseg = ForwardIPG() 
        # Add vertices
        for v in self:
            vertexID = v.getVertexID()
            copyv    = Vertices.Ipoint(vertexID, vertexID)
            reverseg.vertices[vertexID] = copyv
    
        # Add edges
        for v in self:
            predID = v.getVertexID()
            predv  = reverseg.getVertex(predID)
            for succID in v.getSuccessorIDs():
                succv = reverseg.getVertex(succID)
                predv.addPredecessor(succID)
                succv.addSuccessor(predID)
                
        # Set the entry and exit IDs
        reverseg._entryID = self.getExitID()
        reverseg._exitID  = self.getEntryID()
        return reverseg
    
    def getIterationEdgeSources (self):
        return self.__iterationEdgeSources
    
    def getIterationEdgeDestinations (self):
        return self.__iterationEdgeDestinations
            
class DoCalculation:
    def __init__ (self, itree, lnt, data):
        self.__vertexToWCETValues = {}
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, Vertices.HeaderVertex) and level > 0:
                    headerID = v.getHeaderID()
                    Debug.debugMessage("Doing calculation in region with header %d" % headerID, 1)
                    forwardIPG = itree.getForwardIPG(headerID)
                    self.__doDataFlowAnalysis(forwardIPG, data)
                    iterationEdgeTree = itree.getIterationEdgeTree(headerID)
                    self.__doTreeTraversal(iterationEdgeTree, data, headerID)
    
    def __doDataFlowAnalysis (self, forwardIPG, data):
        dfs = Trees.DepthFirstSearch(forwardIPG, forwardIPG.getEntryID())
        for vertexID in reversed(dfs.getPostorder()):
            self.__vertexToWCETValues[vertexID] = []
            v = forwardIPG.getVertex(vertexID)
            if v.numberOfPredecessors() == 0:
                self.__vertexToWCETValues[vertexID].append(0)
            elif v.numberOfPredecessors() == 1:
                predID = v.getPredecessorIDs()[0]
                transitionWCET = data.getTransitionWCET(predID, vertexID)
                values         = [transitionWCET + x for x in self.__vertexToWCETValues[predID]]
                self.__vertexToWCETValues[vertexID].extend(values)
            else:
                allValues = []
                for predID in v.getPredecessorIDs():
                    transitionWCET = data.getTransitionWCET(predID, vertexID)
                    values         = [transitionWCET + x for x in self.__vertexToWCETValues[predID]]
                    allValues.extend(values)
                maxValue = max(allValues)
                self.__vertexToWCETValues[vertexID].append(maxValue)
            Debug.debugMessage("WCET values @ %d = %s" % (vertexID, self.__vertexToWCETValues[vertexID]), 1) 
            
    def __doTreeTraversal (self, iterationEdgeTree, data, headerID):
        vertexToValue = {}
        bound = data.getLoopBound(headerID)
        for level, vertices in iterationEdgeTree.levelIterator(True):
            for v in vertices:
                vertexID = v.getVertexID()
                if isinstance(v, LoopBoundVertex):
                    val = self.__evaulateLoopExpr(v.getExpr(), bound)
                    Debug.debugMessage("Evaluating %s yields %d" % (v.getExpr(), val), 1)
                    vertexToValue[vertexID] = val
                elif isinstance(v, IterationEdgeVertex):
                    sourceID       = v.getSourceID()
                    destinationID  = v.getDestinationID()
                    transitionWCET = data.getTransitionWCET(sourceID, destinationID)
                    if v.numberOfSuccessors() == 1:
                        succID = v.getSuccessorIDs()[0]
                        val    = transitionWCET * vertexToValue[succID]
                        vertexToValue[vertexID] = val
                        Debug.debugMessage("Evaluating (%d, %d) with WCET %d and bound %d yields %d" % (sourceID, destinationID, transitionWCET, vertexToValue[succID], val), 1)
                    else:
                        vertexToValue[vertexID] = val
                        for succID in v.getSuccessorIDs():
                            val = transitionWCET * vertexToValue[succID]
                            Debug.debugMessage("Evaluating (%d, %d) with WCET %d and bound %d yields %d" % (sourceID, destinationID, transitionWCET, vertexToValue[succID], val), 1)
                            vertexToValue[vertexID] = max(val, vertexToValue[vertexID])
                        Debug.debugMessage("WCET (%d, %d) is %d" % (sourceID, destinationID, vertexToValue[vertexID]), 1)
                elif isinstance(v, MaxVertex):
                    vertexToValue[vertexID] = 0
                    for succID in v.getSuccessorIDs():
                        vertexToValue[vertexID] = max(vertexToValue[vertexID], vertexToValue[succID])
                    Debug.debugMessage("Overall WCET = %d" % (vertexToValue[vertexID]), 1)
                else:
                    assert False, "Unrecognised vertex type in iteration edge tree"
                    
    def __evaulateLoopExpr (self, expr, bound):
        import math
        if expr == IterationEdgeTree.constantBound:
            return 1
        elif expr == IterationEdgeTree.selfLoopBound:
            return bound - 1
        elif expr ==IterationEdgeTree.selfLoopMinusOneBound:
            return bound - 2
        elif expr == IterationEdgeTree.cycleBound:
            return math.ceil((bound -1)/2)
        elif expr == IterationEdgeTree.cycleBackBound:
            return bound - 1 - math.ceil((bound -1)/2)
        assert False, "Unhandled loop bound expression %s" % expr            
        
        