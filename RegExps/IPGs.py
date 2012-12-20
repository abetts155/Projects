from DirectedGraphs import DirectedGraph, dummyVertexID
from Vertices import HeaderVertex, Ipoint
from Edges import IPGEdge
import Debug

edgeID = 1

class IPG (DirectedGraph):
    def __init__(self, icfg, lnt=None):
        DirectedGraph.__init__(self)
        self.__branchDivergence = False
        self.__branchDivergentEdges = []
        self.__entryID = dummyVertexID
        self.__exitID = dummyVertexID
        self.__icfg = icfg
        self.__lnt = lnt
        self.__ipointIDToVertex = {}
        self.__auxiliaryData = _AuxiliaryData()
        self.__visited = {}
        self.__initialise()
        self.__doDepthFirstSearch(self.__icfg.getEntryID())
        self.__addEdgesUsingLNT()
        self.__setEntryAndExit()
        self.setName(icfg.getName())
        del self.__visited
        
    def updateWithBranchDivergentPaths (self):
        self.__branchDivergence = True
        for level, vertices in self.__lnt.levelIterator():
            for v in vertices:
                if isinstance(v, HeaderVertex):
                    headerID = v.getHeaderID()
                    if not self.__lnt.isSelfLoopHeader(headerID):
                        if not isinstance(v, Ipoint):
                            self.__auxiliaryData.vertexToReachable[headerID][headerID] = [headerID]
                        self.__auxiliaryData.headerID = headerID
                        self.__solveDFF(headerID)
                        self.__collapseInnerLoops(headerID)
        
    def getIpointVertex (self, ipointID):
        assert ipointID in self.__ipointIDToVertex, "Unable to find Ipoint with trace ID 0x%04X" % ipointID
        return self.__ipointIDToVertex[ipointID]
    
    def getEntryID (self):
        assert self.__entryID != dummyVertexID, "Entry to IPG not found"
        return self.__entryID
    
    def getExitID (self):
        assert self.__exitID != dummyVertexID, "Exit to IPG not found"
        return self.__exitID
    
    def isBranchDivergentEdge (self, predID, succID):
        return (predID, succID) in self.__branchDivergentEdges
    
    def __initialise (self):
        for v in self.__icfg:
            vertexID = v.getVertexID()
            self.__visited[vertexID] = False
            self.__auxiliaryData.vertexToReachable[vertexID] = {}            
            # Ipoint actions
            if self.__icfg.isIpoint(vertexID):
                ipointv = Ipoint(vertexID, v.getIpointID())
                self.vertices[vertexID] = ipointv
                self.__ipointIDToVertex[v.getIpointID()] = ipointv
                if self.__lnt.isLoopHeader(vertexID):
                    self.__auxiliaryData.ipointToHeader[vertexID] = vertexID
                else:
                    headerv = self.__lnt.getInternalHeaderVertex(self.__lnt.getVertex(vertexID).getParentID())
                    self.__auxiliaryData.ipointToHeader[vertexID] = headerv.getHeaderID()
            # Header actions
            if self.__lnt.isLoopHeader(vertexID):
                # Ignore self loops
                if not self.__lnt.isSelfLoopHeader(vertexID):
                    self.__auxiliaryData.headerToIpoints[vertexID]   = self.__lnt.getIpointsInLoopBody(vertexID)
                    self.__auxiliaryData.headerToReachable[vertexID] = {}
                    self.__auxiliaryData.headerToTopSorts[vertexID] = []
                    self.__auxiliaryData.headerToIterationEdges[vertexID] = [] 
                
    def __doDepthFirstSearch (self, vertexID):
        self.__visited[vertexID] = True
        v = self.__icfg.getVertex(vertexID)
        for succID in v.getSuccessorIDs():
            if self.__visited[succID] == False:
                self.__doDepthFirstSearch(succID)
        # Get the root of the LNT
        rootv = self.__lnt.getInternalHeaderVertex(self.__lnt.getRootID())
        if vertexID != rootv.getHeaderID():
            treev = self.__lnt.getVertex(vertexID)
            headerv = self.__lnt.getInternalHeaderVertex(treev.getParentID())
            headerID = headerv.getHeaderID()
            # Insert the vertex visited at the front of the topological sort for the loop
            # in which it is contained
            self.__auxiliaryData.headerToTopSorts[headerID].insert(0, vertexID)
            if self.__lnt.isLoopHeader(vertexID):
                outerHeaderv  = self.__lnt.getInternalHeaderVertex(headerv.getParentID())
                outerHeaderID = outerHeaderv.getHeaderID()
                self.__auxiliaryData.headerToTopSorts[outerHeaderID].insert(0, vertexID)
        else:
            self.__auxiliaryData.headerToTopSorts[vertexID].insert(0, vertexID)
            
    def __addEdgesUsingLNT (self):
        for level, vertices in self.__lnt.levelIterator():
            for v in vertices:
                if isinstance(v, HeaderVertex):
                    headerID = v.getHeaderID()
                    if not self.__lnt.isSelfLoopHeader(headerID):
                        if not isinstance(v, Ipoint):
                            self.__auxiliaryData.vertexToReachable[headerID][headerID] = [headerID]
                        self.__auxiliaryData.headerID = headerID
                        self.__solveDFF(headerID)
                        self.__collapseInnerLoops(headerID)
    
    def __solveDFF (self, headerID):
        Debug.debugMessage("Building IPG in CFG* loop %s" % headerID, 5)
        self.__auxiliaryData.changed = True
        self.__auxiliaryData.iteration = 0
        
        while self.__auxiliaryData.changed:
            self.__auxiliaryData.changed = False
            self.__auxiliaryData.iteration += 1
            
            for vertexID in self.__auxiliaryData.headerToTopSorts[headerID]:
                v = self.__icfg.getVertex(vertexID)
                for predID in v.getPredecessorIDs():
                    analyseEdge = False
                    
                    if vertexID == headerID:
                        if self.__lnt.isLoopBackEdge(predID, headerID) and self.__auxiliaryData.iteration > 1:
                            analyseEdge = True
                    elif not self.__lnt.isLoopHeader(vertexID):
                        analyseEdge = True
                    elif not self.__lnt.isLoopBackEdge(predID, vertexID):
                        analyseEdge = True
                        
                    if analyseEdge:
                        Debug.debugMessage("Iteration %s, edge %s => %s" 
                                           % (self.__auxiliaryData.iteration, predID, vertexID), 15)
                        if self.__icfg.isIpoint(predID):
                            self.__update(predID, vertexID, headerID, predID)
                        else:
                            for keyID in self.__auxiliaryData.vertexToReachable[predID].keys():
                                if self.__icfg.isIpoint(keyID):
                                    self.__update(keyID, vertexID, headerID, predID)
                                    
                            if self.__auxiliaryData.iteration == 1 and headerID in self.__auxiliaryData.vertexToReachable[predID]:
                                self.__updateLoopReachability(headerID, vertexID, predID)
                                     
    def __update (self, ipointID, vertexID, headerID, predID):
        ipointv = self.getVertex(ipointID)
        
        if self.__icfg.isIpoint(vertexID):
            if not ipointv.hasSuccessor(vertexID):
                self.__auxiliaryData.changed = True
                self.__addEdge(ipointID, vertexID)
                if ipointID != predID:
                    self.__updateEdgeLabel(ipointID, vertexID,
                                           self.__auxiliaryData.vertexToReachable[predID][ipointID])
                
                if self.__auxiliaryData.iteration == 2:
                    self.__setIterationEdge(ipointID, vertexID)
            elif ipointID != predID:
                self.__updateEdgeLabel(ipointID, vertexID,
                                       self.__auxiliaryData.vertexToReachable[predID][ipointID])
        else:
            if self.__lnt.isLoopHeader(vertexID) and not self.__lnt.isSelfLoopHeader(vertexID) and vertexID != headerID:
                self.__updateLoopExits(vertexID, ipointID, predID)
                self.__addLoopEntryEdges(ipointID, vertexID, predID)
            else:
                if ipointID not in self.__auxiliaryData.vertexToReachable[vertexID]:
                    self.__auxiliaryData.changed = True
                    self.__auxiliaryData.vertexToReachable[vertexID][ipointID] = [vertexID]
                    if ipointID != predID:
                        self.__auxiliaryData.vertexToReachable[vertexID][ipointID].extend(self.__auxiliaryData.vertexToReachable[predID][ipointID])
                elif ipointID != predID:
                    self.__updateAndTestForIteration(self.__auxiliaryData.vertexToReachable[vertexID][ipointID], 
                                                     self.__auxiliaryData.vertexToReachable[predID][ipointID])
          
    def __updateAndTestForIteration (self, oldSet, extraElements):
        oldsize = len(set(oldSet))
        oldSet.extend(extraElements)
        newsize = len(set(oldSet))
        if oldsize != newsize:
            self.__auxiliaryData.changed = True
                        
    def __updateLoopReachability (self, headerID, predID, vertexID):
        if self.__icfg.isIpoint(vertexID):
            if vertexID not in self.__auxiliaryData.headerToReachable[headerID]:
                self.__auxiliaryData.changed = True
                self.__auxiliaryData.headerToReachable[headerID][vertexID].extend(self.__auxiliaryData.vertexToReachable[predID][headerID])
            else:
                pass
    
    def __updateLoopExits (self, headerID, keyID, predID):
        for exitID in self.__lnt.getLoopExits(headerID):
            if headerID in self.__auxiliaryData.vertexToReachable[exitID]:
                if keyID not in self.__auxiliaryData.vertexToReachable[exitID]:
                    self.__auxiliaryData.changed = True
                    self.__auxiliaryData.vertexToReachable[exitID][keyID] = []
                    if keyID != predID:
                        self.__auxiliaryData.vertexToReachable[exitID][keyID].extend(self.__auxiliaryData.vertexToReachable[predID][keyID])
                    self.__auxiliaryData.vertexToReachable[exitID][keyID].extend(self.__auxiliaryData.vertexToReachable[exitID][headerID])
                elif keyID != predID:
                    self.__updateAndTestForIteration(self.__auxiliaryData.vertexToReachable[exitID][keyID], 
                                                     self.__auxiliaryData.vertexToReachable[predID][keyID])
    
    def __addLoopEntryEdges (self, sourceID, headerID, predID):
        sourcev = self.getVertex(sourceID)
        for destinationID in self.__auxiliaryData.headerToReachable[headerID].keys():
            if not sourcev.hasSuccessor(destinationID):
                self.__auxiliaryData.changed = True
                self.__addEdge(sourceID, destinationID)
                if sourceID != predID:
                    self.__updateEdgeLabel(sourceID, destinationID,
                                           self.__auxiliaryData.vertexToReachable[predID][sourceID])
                self.__updateEdgeLabel(sourceID, destinationID, 
                                       self.__auxiliaryData.headerToReachable[headerID][destinationID])
                if self.__auxiliaryData.iteration == 2:
                    self.__setIterationEdge(sourceID, destinationID) 
            elif self.__auxiliaryData.iteration == 2:
                self.__setIterationEdge(sourceID, destinationID)
    
    def __addEdge (self, predID, succID):
        global edgeID
        Debug.debugMessage("Adding IPG Edge %s => %s" % (predID, succID), 10)
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        predv.addIpointSuccessor (succv.getIpointID(), succID)
        succe = IPGEdge(succID, edgeID)
        prede = IPGEdge(predID, edgeID)
        predv.addSuccessorEdge(succe)
        succv.addPredecessorEdge(prede) 
        edgeID += 1
        if self.__branchDivergence:
            self.__branchDivergentEdges.append((predID, succID))
        
    def __setIterationEdge (self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        succe = predv.getSuccessorEdge(succID)
        prede = succv.getPredecessorEdge(predID)
        succe.setIterationEdge()
        prede.setIterationEdge()
                
    def __updateEdgeLabel (self, predID, succID, extraElements):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        succe = predv.getSuccessorEdge(succID)
        prede = succv.getPredecessorEdge(predID)
        size1 = succe.getEdgeLabelSize()
        size2 = prede.getEdgeLabelSize()
        assert size1 == size2, "Discrepancy between size of edge labels for %s => %s. Found both %s and %s" % (predID, succID, size1, size2)
        succe.addToEdgeLabel(extraElements)
        prede.addToEdgeLabel(extraElements)
        succe.addToEdgeLabel([predID, succID])
        prede.addToEdgeLabel([predID, succID])
        newSize = succe.getEdgeLabelSize()
        if newSize != size1:
            self.__auxiliaryData.changed = True
            
    def __collapseInnerLoops (self, headerID):
        headerv = self.__lnt.getVertex(self.__lnt.getVertex(headerID).getParentID())
        for succID in headerv.getSuccessorIDs():
            if self.__lnt.isLoopHeader(succID) and not self.__lnt.isSelfLoopHeader(succID):
                ipoints = self.__auxiliaryData.headerToIpoints[succID]
                self.__auxiliaryData.headerToIpoints[headerID].extend(ipoints)
                for ipointID in ipoints:
                    self.__auxiliaryData.ipointToHeader[ipointID] = headerID
                    
    def __setEntryAndExit (self):
        entryv = self.vertices[self.__icfg.getEntryID()]
        self.__entryID = entryv.getVertexID()
        assert entryv.numberOfPredecessors() == 1, "Entry vertex %s of IPG has multiple predecessors" % self.__entryID
        for predID in entryv.getPredecessorIDs():
            self.__exitID = predID

class _AuxiliaryData ():
    def __init__(self):
        self.changed   = False
        self.iteration = 0
        self.vertexToReachable      = {}
        self.headerToReachable      = {}
        self.headerToTopSorts       = {}
        self.headerToIterationEdges = {}
        self.headerToIpoints        = {}
        self.ipointToHeader         = {}
    