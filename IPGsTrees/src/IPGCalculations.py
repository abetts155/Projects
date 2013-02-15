import Trees, Vertices, UDrawGraph, Debug, DirectedGraphs
import os

class BuildMiniIPGs ():    
    def __init__ (self, basename, icfg, lnt, ipg):
        self.__headerToMiniIPG        = {}
        self.__headerIterationEdgeIDs = {}
        self.__headerRelativeEdgeIDs  = {}
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, Vertices.HeaderVertex):
                    headerID = v.getHeaderID()
                    Debug.debugMessage("Analysing header %d" % headerID, 1)
                    forwardICFG = lnt.induceSubgraph(v)
                    UDrawGraph.makeUdrawFile (forwardICFG, "%s.%s.Header%d.%s" % (basename, icfg.getName(), headerID, "icfg"))
                    miniIPG = MiniIPG(headerID, self.__headerToMiniIPG, forwardICFG, lnt, ipg)
                    self.__headerToMiniIPG[headerID] = miniIPG 
                    UDrawGraph.makeUdrawFile (miniIPG, "%s.%s.Header%d.%s" % (basename, icfg.getName(), headerID, "ipg"))
            
    def getMiniIPG (self, headerID):
        assert headerID in self.__headerToMiniIPG, "Unable to find mini IPG region of loop %d" % (headerID)
        return self.__headerToMiniIPG[headerID]
    
class MiniIPG (DirectedGraphs.FlowGraph):    
    def __init__ (self, headerID, headerToMiniIPG, icfg, lnt, ipg):
        DirectedGraphs.FlowGraph.__init__(self)
        self.__headerID                  = headerID
        self.__headerToMiniIPG           = headerToMiniIPG
        self.__icfg                      = icfg
        self.__lnt                       = lnt
        self.__ipg                       = ipg
        self.__innerLoopIpoints          = {}
        self.__vertexToReachable         = {}
        self.__innerLoopEntryEdgeIDs     = {}
        self.__innerLoopExitEdgeIDs      = {}
        self.__iterationEdgeDestinations = set([])
        self.__iterationEdgeSources      = set([])
        self.__iterationEdgeIDs          = set([])
        self.__entryEdgeSources          = set([])
        self.__addIpoints()
        self.__addAcyclicEdges()
        self.__addIterationEdges()
        self.__outputEntryAndExitEdges()
            
    def __addIpoints (self):    
        for v in self.__icfg:
            vertexID = v.getVertexID()
            self.__vertexToReachable[vertexID] = set([])  
            if vertexID == self.__icfg.getEntryID() and not self.__ipg.hasVertex(vertexID):
                self.__vertexToReachable[vertexID].add(vertexID)          
            # Ipoint actions
            if self.__ipg.hasVertex(vertexID):
                ipointv = Vertices.Ipoint(vertexID, v.getIpointID())
                self.vertices[vertexID] = ipointv
                
        headerv = self.__lnt.getInternalHeaderVertex(self.__lnt.getVertex(self.__headerID).getParentID())
        for succID in headerv.getSuccessorIDs():
            succv = self.__lnt.getVertex(succID)
            if isinstance(succv, Trees.HeaderVertex):
                innerheaderID                               = succv.getHeaderID()
                self.__innerLoopEntryEdgeIDs[innerheaderID] = set([])
                self.__innerLoopExitEdgeIDs[innerheaderID]  = set([])
                innerMiniIPG                                = self.__headerToMiniIPG[innerheaderID]
                for vertexID in innerMiniIPG.getIterationEdgeDestinations():
                    if not self.hasVertex(vertexID):
                        v       = self.__ipg.getVertex(vertexID)
                        ipointv = Vertices.Ipoint(vertexID, v.getIpointID())
                        self.vertices[vertexID] = ipointv
                        self.__innerLoopIpoints[vertexID] = innerheaderID
                for exitID in self.__lnt.getLoopExits(innerheaderID):
                    if self.__ipg.hasVertex(exitID):
                        if not self.hasVertex(exitID):
                            v       = self.__ipg.getVertex(exitID)
                            ipointv = Vertices.Ipoint(exitID, v.getIpointID())
                            self.vertices[exitID] = ipointv
                            self.__innerLoopIpoints[exitID] = innerheaderID
                    else:
                        for keyID in innerMiniIPG.getReachableSet(exitID):
                            if not self.hasVertex(keyID) and self.__ipg.hasVertex(keyID):
                                v       = self.__ipg.getVertex(keyID)
                                ipointv = Vertices.Ipoint(keyID, v.getIpointID())
                                self.vertices[keyID] = ipointv
                                self.__innerLoopIpoints[keyID] = innerheaderID
    
    def __outputEntryAndExitEdges (self):   
        headerv = self.__lnt.getInternalHeaderVertex(self.__lnt.getVertex(self.__headerID).getParentID())
        for succID in headerv.getSuccessorIDs():
            succv = self.__lnt.getVertex(succID)
            if isinstance(succv, Trees.HeaderVertex):
                innerheaderID = succv.getHeaderID() 
                Debug.debugMessage("Loop-entry edges of %d = %s" % (innerheaderID, self.__innerLoopEntryEdgeIDs[innerheaderID]), 1)
                Debug.debugMessage("Loop-exit edges of %d = %s" % (innerheaderID, self.__innerLoopExitEdgeIDs[innerheaderID]), 1)
                
    def __addAcyclicEdges (self):
        # Compute a topological sort on the ICFG
        dfs = Trees.DepthFirstSearch(self.__icfg, self.__icfg.getEntryID())
        # If the header of the loop is an Ipoint, that is the only destination of an iteration edge
        if self.__ipg.hasVertex(self.__icfg.getEntryID()):
            self.__iterationEdgeDestinations.add(self.__icfg.getEntryID())
        # Perform data-flow analysis
        changed = True
        while changed:
            changed = False
            for vertexID in reversed(dfs.getPostorder()):
                v = self.__icfg.getVertex(vertexID)                
                if self.__lnt.isLoopHeader(vertexID) and vertexID != self.__icfg.getEntryID():
                    self.__addLoopEntryEdges(v)
                    self.__addIpointsToAbstractVertex(v)
                else:
                    for predID in v.getPredecessorIDs():
                        if self.__ipg.hasVertex(predID):
                            if self.__ipg.hasVertex(vertexID):
                                self.__addEdge(predID, vertexID)
                            else:
                                self.__vertexToReachable[vertexID].add(predID)
                        else:
                            for keyID in self.__vertexToReachable[predID]:
                                if self.__ipg.hasVertex(keyID) and self.__ipg.hasVertex(vertexID):
                                    self.__addEdge(keyID, vertexID)
                                elif not self.__ipg.hasVertex(keyID) and self.__ipg.hasVertex(vertexID):
                                    self.__iterationEdgeDestinations.add(vertexID)
                                else:
                                    self.__vertexToReachable[vertexID].add(keyID)
                # Loop tail detected. Any Ipoint that can reach here is an iteration edge source
                if vertexID in self.__lnt.getLoopTails(self.__headerID):
                    if self.__ipg.hasVertex(vertexID):
                        self.__iterationEdgeSources.add(vertexID)
                    else:
                        for keyID in self.__vertexToReachable[vertexID]:
                            if self.__ipg.hasVertex(keyID):
                                self.__iterationEdgeSources.add(keyID)
                            
    def __addLoopEntryEdges (self, v):
        innerheaderID = v.getVertexID()
        Debug.debugMessage("Inner header %d detected" % innerheaderID, 1)
        innerMiniIPG = self.__headerToMiniIPG[innerheaderID]        
        for predID in v.getPredecessorIDs():
            if self.__ipg.hasVertex(predID):
                innerMiniIPG.addEntryEdgeSource(predID)
                for succID in innerMiniIPG.getIterationEdgeDestinations():
                    self.__addEdge(predID, succID)
            else:
                for keyID in self.__vertexToReachable[predID]:
                    if self.__ipg.hasVertex(keyID):
                        innerMiniIPG.addEntryEdgeSource(keyID)
                        for succID in innerMiniIPG.getIterationEdgeDestinations():
                            self.__addEdge(keyID, succID)
                    else:
                        # The key is a header vertex. This means that all
                        # the destinations of iteration edges of the inner loop are
                        # also destinations of iterations edges of the outer loop 
                        for succID in innerMiniIPG.getIterationEdgeDestinations(): 
                            self.__iterationEdgeDestinations.add(succID)
    
    def __addIpointsToAbstractVertex (self, v):
        innerheaderID = v.getVertexID()
        innerMiniIPG  = self.__headerToMiniIPG[innerheaderID]       
        for exitID in self.__lnt.getLoopExits(innerheaderID):
            if self.__ipg.hasVertex(exitID):
                self.__vertexToReachable[innerheaderID].add(exitID)
            else:
                for keyID in innerMiniIPG.getReachableSet(exitID):
                    if self.__ipg.hasVertex(keyID):
                        self.__vertexToReachable[innerheaderID].add(keyID)
                            
    def __addIterationEdges (self):
        Debug.debugMessage("Iteration edge SOURCEs      = %s" % self.__iterationEdgeSources, 1)
        Debug.debugMessage("Iteration edge DESTINATIONS = %s" % self.__iterationEdgeDestinations, 1)
        for predID in self.__iterationEdgeSources:
            for succID in self.__iterationEdgeDestinations:
                self.__addEdge(predID, succID)
                originalpredv = self.__ipg.getVertex(predID)
                originaledge  = originalpredv.getSuccessorEdge(succID)
                edgeID        = originaledge.getEdgeID()
                self.__iterationEdgeIDs.add(edgeID)
        Debug.debugMessage("Iteration edges for %d is %s" % (self.__headerID, self.__iterationEdgeIDs), 1)
                            
    def __addEdge (self, predID, succID):
        from Edges import IPGEdge
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        if not predv.hasSuccessor(succID):
            Debug.debugMessage("Adding edge (%d, %d)" % (predID, succID), 1)
            assert not succv.hasPredecessor(predID), "Vertex %d has predecessor %d although vertex %d does not have successor %d" % (succID, predID, predID, succID)
            originalpredv = self.__ipg.getVertex(predID)
            originaledge  = originalpredv.getSuccessorEdge(succID)
            succe = IPGEdge(succID, originaledge.getEdgeID())
            prede = IPGEdge(predID, originaledge.getEdgeID())
            predv.addSuccessorEdge(succe)
            succv.addPredecessorEdge(prede)
            if predID in self.__innerLoopIpoints:
                innerheaderID = self.__innerLoopIpoints[predID]
                Debug.debugMessage("(%d, %d) is a loop-exit edge for loop with header %d" % (predID, succID, innerheaderID), 1)
                self.__innerLoopExitEdgeIDs[innerheaderID].add(originaledge.getEdgeID())
            elif succID in self.__innerLoopIpoints:
                innerheaderID = self.__innerLoopIpoints[succID]
                Debug.debugMessage("(%d, %d) is a loop-entry edge for loop with header %d" % (predID, succID, innerheaderID), 1)
                self.__innerLoopEntryEdgeIDs[innerheaderID].add(originaledge.getEdgeID())
            
    def getReachableSet (self, vertexID):
        assert vertexID in self.__vertexToReachable, "Unable to find %d in the ICFG" % vertexID
        return self.__vertexToReachable[vertexID]
    
    def addEntryEdgeSource (self, vertexID):
        self.__entryEdgeSources.add(vertexID)
        
    def getEntryEdgeSources (self):
        return self.__entryEdgeSources
    
    def getIterationEdgeSources (self):
        return self.__iterationEdgeSources
    
    def getIterationEdgeDestinations (self):
        return self.__iterationEdgeDestinations
    
    def getIterationEdgeIDs (self):
        return self.__iterationEdgeIDs
    
    def isIterationEdgeID (self, edgeID):
        return edgeID in self.__iterationEdgeIDs
    
    def getInnerLoopEntryEdgeIDs (self, innerheaderID):
        assert innerheaderID in self.__innerLoopEntryEdgeIDs, "Unable to find loop-entry edges for header %d" % innerheaderID
        return self.__innerLoopEntryEdgeIDs[innerheaderID]

    def getInnerLoopExitEdgeIDs (self, innerheaderID):
        assert innerheaderID in self.__innerLoopExitEdgeIDs, "Unable to find loop-exit edges for header %d" % innerheaderID
        return self.__innerLoopExitEdgeIDs[innerheaderID]
    
class CreateILP ():
    comma     = ","
    equals    = " = "
    ltOrEqual = " <= "
    plus      = " + "
    semiColon = ";"
    
    def __init__ (self, basepath, basename, data, ipg, lnt, miniIPGs):
        filename = "%s.%s.%s" % (basepath + os.sep + basename, ipg.getName(), "ilp")
        with open(filename, 'w') as self.__outfile:
            self.__createObjectiveFunction(data, ipg)
            self.__createStructuralConstraints(ipg)
            self.__createRelativeCapacityConstraints(data, lnt, miniIPGs)
            self.__createIntegerConstraints(ipg)

    def __createObjectiveFunction (self, data, ipg):
        self.__outfile.write("max: ")
        counter = ipg.numOfEdges()
        for v in ipg:
            for succID in v.getSuccessorIDs():                
                succe          = v.getSuccessorEdge(succID)
                edgeID         = succe.getEdgeID()
                transitionWCET = data.getTransitionWCET(v.getVertexID(), succID)
                self.__outfile.write("%d %s" % (transitionWCET, self.__getEdgeVariable(edgeID)))
                if counter > 1:
                    self.__outfile.write(CreateILP.plus)
                counter -= 1
        self.__outfile.write(CreateILP.semiColon)
        self.__outfile.write(self.__getNewLine(2))

    def __createStructuralConstraints (self, ipg):
        for v in ipg:
            self.__outfile.write(self.__getComment("Vertex %d" % v.getVertexID()))
            # Analyse the predecessors
            counter = v.numberOfPredecessors()
            for predID in v.getPredecessorIDs():                    
                prede  = v.getPredecessorEdge(predID)
                edgeID = prede.getEdgeID()
                self.__outfile.write(self.__getEdgeVariable(edgeID))
                if counter > 1:
                    self.__outfile.write(CreateILP.plus)
                counter -= 1
            self.__outfile.write(CreateILP.equals)
            # Analyse the successors
            counter = v.numberOfSuccessors()
            for succID in v.getSuccessorIDs():
                succe  = v.getSuccessorEdge(succID)
                edgeID = succe.getEdgeID()
                self.__outfile.write(self.__getEdgeVariable(edgeID))
                if counter > 1:
                    self.__outfile.write(CreateILP.plus)
                counter -= 1
            self.__outfile.write(CreateILP.semiColon)
            self.__outfile.write(self.__getNewLine(2))
            
    def __createRelativeCapacityConstraints (self, data, lnt, miniIPGs):
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, Vertices.HeaderVertex):
                    Debug.debugMessage("Analysing header %d" % v.getHeaderID(), 1)
                    # Get iteration edges for this header
                    if level > 0:
                        self.__createInnerLoopRelativeCapacityConstraint(data, lnt, miniIPGs, v)
                    else:
                        headerID = v.getHeaderID()
                        self.__outfile.write(self.__getComment("Relative capacity constraint for entry vertex %d" % (headerID)))
                        iterationEdgeIDs = miniIPGs.getMiniIPG(headerID).getIterationEdgeIDs()
                        assert len(iterationEdgeIDs) == 1, "There should be exactly one iteration edge for the entry vertex %d. There are %d." % (headerID, len(iterationEdgeIDs))
                        edgeID = iter(iterationEdgeIDs).next()
                        self.__outfile.write(self.__getEdgeVariable(edgeID))
                        self.__outfile.write(CreateILP.equals)
                        self.__outfile.write("1")
                        self.__outfile.write(CreateILP.semiColon)
                        self.__outfile.write(self.__getNewLine(2))
                        
    def __createInnerLoopRelativeCapacityConstraint (self, data, lnt, miniIPGs, v):
        headerID         = v.getHeaderID()
        miniIPG          = miniIPGs.getMiniIPG(headerID)
        iterationEdgeIDs = miniIPG.getIterationEdgeIDs()
        # Get the loop bound w.r.t. the parent loop
        parentv        = lnt.getVertex(v.getParentID())
        parentHeaderID = parentv.getHeaderID()
        self.__outfile.write(self.__getComment("Relative capacity constraint for header %d w.r.t to header %d" % (headerID, parentHeaderID)))
        bound = data.getLoopBound(headerID, parentHeaderID)
        # Write out the iteration edges
        counter = len(iterationEdgeIDs)
        for edgeID in iterationEdgeIDs:
            self.__outfile.write(self.__getEdgeVariable(edgeID))
            if counter > 1:
                self.__outfile.write(CreateILP.plus)
            counter -= 1
        self.__outfile.write(CreateILP.ltOrEqual)
        # Write out the relative edges
        outerMiniIPG    = miniIPGs.getMiniIPG(parentHeaderID)
        relativeEdgeIDs = outerMiniIPG.getInnerLoopEntryEdgeIDs(headerID)
        counter = len(relativeEdgeIDs)
        for edgeID in relativeEdgeIDs:
            self.__outfile.write("%d %s" % (bound, self.__getEdgeVariable(edgeID)))
            if counter > 1:
                self.__outfile.write(CreateILP.plus)
            counter -= 1
        self.__outfile.write(CreateILP.semiColon)
        self.__outfile.write(self.__getNewLine(2))
            
    def __createIntegerConstraints (self, ipg):
        self.__outfile.write("int")
        counter = ipg.numOfEdges()
        for v in ipg:
            for succID in v.getSuccessorIDs():                
                succe  = v.getSuccessorEdge(succID)
                edgeID = succe.getEdgeID()
                self.__outfile.write(" %s" % self.__getEdgeVariable(edgeID))
                if counter > 1:
                    self.__outfile.write(CreateILP.comma)
                counter -= 1
        self.__outfile.write(CreateILP.semiColon)
        self.__outfile.write(self.__getNewLine())
            
    def __getEdgeVariable (self, edgeID):
        return "e_%d" % edgeID
    
    def __getComment (self, comment):
        return "// " + comment + "\n"
    
    def __getNewLine (self, num=1):
        return "\n" * num        
        