import Trees, Vertices, UDrawGraph, Debug, DirectedGraphs

class IPGTree (Trees.Tree):    
    def __init__(self, icfg, lnt):
        Trees.Tree.__init__(self)
        self.__headerToForwardIPG = {}
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, Vertices.HeaderVertex):
                    Debug.debugMessage("Analysing header %d" % v.getHeaderID(), 1)
                    forwardICFG = lnt.induceSubgraph(v)
                    UDrawGraph.makeUdrawFile (forwardICFG, "%s.Header%d.%s" % (icfg.getName(), v.getHeaderID(), "icfg"))
                    forwardIPG = ForwardIPG(forwardICFG)
                    self.__headerToForwardIPG[v.getHeaderID()] = forwardIPG
        UDrawGraph.makeUdrawFile (self, "%s.%s" % (icfg.getName(), "itree"))
        
    def getForwardIPG (self, headerID):
        assert headerID in self.__headerToForwardIPG, "Unable to find forward IPG region of loop %d" % (headerID)
        return self.__headerToForwardIPG[headerID]
    
class ForwardIPG (DirectedGraphs.FlowGraph):
    edgeID = 1
    
    def __init__ (self, icfg):
        DirectedGraphs.FlowGraph.__init__(self)
        self.__vertexToReachable = {}
        for v in icfg:
            vertexID = v.getVertexID()
            self.__vertexToReachable[vertexID] = set([])            
            # Ipoint actions
            if icfg.isIpoint(vertexID):
                ipointv = Vertices.Ipoint(vertexID, v.getIpointID())
                self.vertices[vertexID] = ipointv
        self.__addAcyclicEdges(icfg)    
        self.__guaranteeSingleEntry()    
        
    def __guaranteeSingleEntry(self):
        entries = []
        for v in self:
            if v.numberOfPredecessors() == 0:
                entries.append(v.getVertexID())
        if len(entries) > 1:
            entryID       = self.getNextVertexID()
            self._entryID = entryID
            ipointv       = Vertices.Ipoint(entryID, entryID)
            self.vertices[entryID] = ipointv
            for succID in entries:
                self.__addEdge(entryID, succID, True)
        else:
            self._entryID = entries[0]
                
    def __addAcyclicEdges(self, icfg):
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
                            if icfg.isIpoint(vertexID):
                                self.__addEdge(keyID, vertexID)
                            else:
                                self.__vertexToReachable[vertexID].add(keyID)
                            
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
            
class DoCalculation:
    def __init__ (self, itree, lnt, data):
        self.__vertexToWCETValues = {}
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, Vertices.HeaderVertex):
                    Debug.debugMessage("Doing calculation in region with header %d" % v.getHeaderID(), 1)
                    forwardIPG = itree.getForwardIPG(v.getHeaderID())
                    self.__doDataFlowAnalysis(forwardIPG, data)
    
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
                Debug.debugMessage("WCET(%d, %d) = %d" % (predID, vertexID, transitionWCET), 1)
                values         = [transitionWCET + x for x in self.__vertexToWCETValues[predID]]
                self.__vertexToWCETValues[vertexID].extend(values)
            else:
                allValues = []
                for predID in v.getPredecessorIDs():
                    transitionWCET = data.getTransitionWCET(predID, vertexID)
                    Debug.debugMessage("WCET(%d, %d) = %d" % (predID, vertexID, transitionWCET), 1)
                    values         = [transitionWCET + x for x in self.__vertexToWCETValues[predID]]
                    allValues.extend(values)
                maxValue = max(allValues)
                self.__vertexToWCETValues[vertexID].append(maxValue)
            
            print self.__vertexToWCETValues[vertexID]        
            
class CreateWCETData:    
    def __init__ (self, ipg):
        import random 
        self.__transitionWCET = {}
        for v in ipg:
            for succID in v.getSuccessorIDs():
                succe = v.getSuccessorEdge(succID)
                if not succe.isDummyEdge():
                    self.__transitionWCET[(v.getVertexID(), succID)] = random.randint(1, 100)
    
    def getTransitionWCET (self, predID, succID):
        if (predID, succID) in self.__transitionWCET:
            return self.__transitionWCET[(predID, succID)]
        else:
            return 0
        
        