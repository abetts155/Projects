import directed_graphs
import trees
import vertices
import edges
import debug

edge_ID = 1

class IPG (directed_graphs.FlowGraph):
    def __init__(self, icfg):
        directed_graphs.FlowGraph.__init__(self)
        self.name                = icfg.name
        self.__icfg              = icfg
        self.__ipointIDToVertex  = {}
        self.__vertexToReachable = {}
        self.__initialise()
        self.__addEdges()
        self.__setEntryAndExit()
        
    def getIpointVertex (self, ipointID):
        assert ipointID in self.__ipointIDToVertex, "Unable to find Ipoint with trace ID 0x%04X" % ipointID
        return self.__ipointIDToVertex[ipointID]
    
    def __initialise (self):
        for v in self.__icfg:
            self.__vertexToReachable[v.vertexID] = {}
            if self.__icfg.isIpoint(v.vertexID):
                ipointv = vertices.Ipoint(v.vertexID, v.ipointID)
                self.the_vertices[v.vertexID] = ipointv
                self.__ipointIDToVertex[v.ipointID] = ipointv 
                
    def __addEdges (self):
        changed = True
        dfs     = trees.DepthFirstSearch(self.__icfg, self.__icfg.getEntryID())
        # Do data-flow analysis
        while changed:
            changed = False
            for vertexID in reversed(dfs.getPostorder()):
                v = self.__icfg.getVertex(vertexID)
                for predID in v.getPredecessorIDs():
                    if self.__icfg.isIpoint(predID):
                        if predID not in self.__vertexToReachable[vertexID]:
                            changed = True
                            self.__vertexToReachable[vertexID][predID] = set([])
                            if not self.__icfg.isIpoint(vertexID):
                                self.__vertexToReachable[vertexID][predID].add(vertexID)
                    else:
                        for keyID in self.__vertexToReachable[predID]:
                            if keyID not in self.__vertexToReachable[vertexID]:
                                changed = True
                                self.__vertexToReachable[vertexID][keyID] = set([])
                                self.__vertexToReachable[vertexID][keyID].update(self.__vertexToReachable[predID][keyID])
                                if not self.__icfg.isIpoint(vertexID):
                                    self.__vertexToReachable[vertexID][keyID].add(vertexID)
                            else:
                                oldSize = len(self.__vertexToReachable[vertexID][keyID])
                                self.__vertexToReachable[vertexID][keyID].update(self.__vertexToReachable[predID][keyID])
                                newSize = len(self.__vertexToReachable[vertexID][keyID])
                                if newSize != oldSize:
                                    changed = True
        # Now add the edges 
        for vertexID, ipointToVertexSet in self.__vertexToReachable.iteritems():
            if self.__icfg.isIpoint(vertexID):
                for predID, vertex_set in ipointToVertexSet.iteritems():
                    self.__addEdge(predID, vertexID, vertex_set)
    
    def __addEdge (self, predID, succID, vertex_set):
        global edge_ID
        debug.debug_message("Adding IPG Edge %s => %s" % (predID, succID), __name__, 10)
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        predv.addIpointSuccessor (succv.ipointID, succID)
        prede = edges.IPGEdge(predID, edge_ID)
        succe = edges.IPGEdge(succID, edge_ID)
        prede.edge_label.update(vertex_set)
        succe.edge_label.update(vertex_set)
        predv.addSuccessorEdge(succe)
        succv.addPredecessorEdge(prede) 
        edge_ID += 1
                    
    def __setEntryAndExit (self):
        entryv = self.the_vertices[self.__icfg.getEntryID()]
        self._entryID = entryv.vertexID
        assert entryv.numberOfPredecessors() == 1, "Entry vertex %s of IPG has multiple predecessors" % self._entryID
        for predID in entryv.getPredecessorIDs():
            self._exitID = predID

    