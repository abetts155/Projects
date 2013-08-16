import Trees, Vertices, UDrawGraph, Debug, DirectedGraphs
import os

class BuildMiniIPGs ():    
    def __init__ (self, basename, icfg, lnt, ipg):
        self.__headerToMiniIPG         = {}
        self.__headerIterationEdgeIDs  = {}
        self.__headerRelativeEdgeIDs   = {}
        self.__headerToReconstructible = {}
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, Vertices.HeaderVertex):
                    headerID = v.getHeaderID()
                    Debug.debugMessage("Analysing header %d" % headerID, 1)
                    forwardICFG = lnt.induceSubgraph(v, self.__headerToReconstructible)
                    forwardICFG.setEdgeIDs()
                    self.__headerToReconstructible[headerID] = forwardICFG.isPathReconstructible()
                    Debug.debugMessage("Region in %d is %spath reconstructible" % (headerID, "" if self.__headerToReconstructible[headerID] else "NOT "), 1)
                    UDrawGraph.makeUdrawFile (forwardICFG, "%s.%s.Header%d.%s" % (basename, icfg.getName(), headerID, "icfg"))
                    miniIPG = MiniIPG(headerID, self.__headerToMiniIPG, forwardICFG, lnt, ipg)
                    self.__headerToMiniIPG[headerID] = miniIPG
            
    def isPathReconstructible (self, headerID):
        assert headerID in self.__headerToReconstructible, "Unable to find whether loop with header %d is path reconstructible" % (headerID)
        return self.__headerToReconstructible[headerID]
    
    def getMiniIPG (self, headerID):
        assert headerID in self.__headerToMiniIPG, "Unable to find the mini IPG of the loop with header %d" % (headerID)
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
                Debug.debugMessage("At vertex %d" % vertexID, 1)
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
        if not self.__icfg.isIpoint(innerheaderID):
            for predID in v.getPredecessorIDs():
                if not self.__ipg.hasVertex(predID):
                    if self.__headerID in self.__vertexToReachable[predID]:
                        self.__vertexToReachable[innerheaderID].add(self.__headerID)
                            
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

class LpSolve:
    comma        = ","
    edgePrefix   = "e_"
    equals       = " = "
    fileSuffix   = "ilp"
    int_         = "int"
    ltOrEqual    = " <= "
    max_         = "max: "
    plus         = " + "
    semiColon    = ";"
    vertexPrefix = "v_"
    
    @staticmethod
    def getEdgeVariable (edgeID):
        return "%s%d" % (LpSolve.edgePrefix, edgeID)
    
    @staticmethod
    def getVertexVariable (vertexID):
        return "%s%d" % (LpSolve.vertexPrefix, vertexID)
    
    @staticmethod
    def getComment (comment):
        return "// " + comment + "\n"
    
    @staticmethod
    def getNewLine (num=1):
        return "\n" * num  
    
class ILP ():
    def __init__ (self):
        self._wcet = -1
        self._variableToExecutionCount = {}
        
    def _solve(self, filename, variablePrefix):
        from subprocess import Popen, PIPE
        import shlex, decimal
        Debug.debugMessage("Solving ILP", 10)
        command = "lp_solve %s" % filename 
        proc = Popen(command, shell=True, executable="/bin/bash", stdout=PIPE, stderr=PIPE)
        returnCode = proc.wait()
        if returnCode != 0:
            Debug.exitMessage("Running '%s' failed" % command)
        for line in proc.stdout.readlines():
            if line.startswith("Value of objective function"):
                lexemes     = shlex.split(line)
                self._wcet = long(decimal.Decimal(lexemes[-1])) 
            elif line.startswith(LpSolve.edgePrefix) or line.startswith(LpSolve.vertexPrefix):
                lexemes = shlex.split(line)
                assert len(lexemes) == 2, "Incorrectly detected variable execution count line '%s'" % line
                prefix = lexemes[0][:2]
                if prefix == variablePrefix:
                    variable = int(lexemes[0][2:])
                    count    = int(lexemes[1]) 
                    self._variableToExecutionCount[variable] = count
        
class CreateIPGILP (ILP):
    def __init__ (self, basepath, basename, data, ipg, lnt, miniIPGs):
        ILP.__init__(self)
        filename = "%s.%s.%s.%s" % (basepath + os.sep + basename, ipg.getName(), "ipg", LpSolve.fileSuffix)
        with open(filename, 'w') as self.__outfile:
            self.__createObjectiveFunction(data, ipg)
            self.__createStructuralConstraints(ipg)
            self.__createRelativeCapacityConstraints(data, lnt, miniIPGs)
            self.__createIntegerConstraints(ipg)
        self._solve(filename, LpSolve.edgePrefix)
        Debug.debugMessage("WCET = %d" % self._wcet, 1)
        self.__printExecutionCounts(ipg)
        
    def __printExecutionCounts (self, ipg):
        for edgeID, count in self._variableToExecutionCount.iteritems():
                Debug.debugMessage("Execution count of variable %s = %d" % (LpSolve.getEdgeVariable(edgeID), count), 10)
        self.__basicBlockToExecutionCount = {}
        for v in ipg:
            for succID in v.getSuccessorIDs():                
                succe  = v.getSuccessorEdge(succID)
                edgeID = succe.getEdgeID() 
                if self._variableToExecutionCount[edgeID]:
                    for bbID in succe.getEdgeLabel():
                        if bbID not in self.__basicBlockToExecutionCount:
                            self.__basicBlockToExecutionCount[bbID] = 0
                        self.__basicBlockToExecutionCount[bbID] += self._variableToExecutionCount[edgeID]
        for bbID, count in self.__basicBlockToExecutionCount.iteritems():
                Debug.debugMessage("Execution count of variable %s = %d" % (LpSolve.getVertexVariable(bbID), count), 10)

    def __createObjectiveFunction (self, data, ipg):
        self.__outfile.write(LpSolve.max_)
        counter = ipg.numOfEdges()
        for v in ipg:
            for succID in v.getSuccessorIDs():                
                succe          = v.getSuccessorEdge(succID)
                edgeID         = succe.getEdgeID()
                transitionWCET = data.getTransitionWCET(v.getVertexID(), succID)
                self.__outfile.write("%d %s" % (transitionWCET, LpSolve.getEdgeVariable(edgeID)))
                if counter > 1:
                    self.__outfile.write(LpSolve.plus)
                counter -= 1
        self.__outfile.write(LpSolve.semiColon)
        self.__outfile.write(LpSolve.getNewLine(2))

    def __createStructuralConstraints (self, ipg):
        for v in ipg:
            vertexID = v.getVertexID()
            self.__outfile.write(LpSolve.getComment("Vertex %d" % vertexID))
            # Analyse the predecessors
            self.__outfile.write(LpSolve.getVertexVariable(vertexID))
            self.__outfile.write(LpSolve.equals)
            counter = v.numberOfPredecessors()
            for predID in v.getPredecessorIDs():                    
                prede  = v.getPredecessorEdge(predID)
                edgeID = prede.getEdgeID()
                self.__outfile.write(LpSolve.getEdgeVariable(edgeID))
                if counter > 1:
                    self.__outfile.write(LpSolve.plus)
                counter -= 1
            self.__outfile.write(LpSolve.semiColon)
            self.__outfile.write(LpSolve.getNewLine())  
            # Flow in, flow out w.r.t to predecessors and successors
            counter = v.numberOfPredecessors()
            for predID in v.getPredecessorIDs():                    
                prede  = v.getPredecessorEdge(predID)
                edgeID = prede.getEdgeID()
                self.__outfile.write(LpSolve.getEdgeVariable(edgeID))
                if counter > 1:
                    self.__outfile.write(LpSolve.plus)
                counter -= 1
            self.__outfile.write(LpSolve.equals)   
            # Analyse the successors
            counter = v.numberOfSuccessors()
            for succID in v.getSuccessorIDs():
                succe  = v.getSuccessorEdge(succID)
                edgeID = succe.getEdgeID()
                self.__outfile.write(LpSolve.getEdgeVariable(edgeID))
                if counter > 1:
                    self.__outfile.write(LpSolve.plus)
                counter -= 1
            self.__outfile.write(LpSolve.semiColon)
            self.__outfile.write(LpSolve.getNewLine(2))
            
    def __createRelativeCapacityConstraints (self, data, lnt, miniIPGs):
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, Vertices.HeaderVertex):
                    # Get iteration edges for this header
                    if level > 0:
                        self.__createInnerLoopRelativeCapacityConstraint(data, lnt, miniIPGs, v)
                    else:
                        headerID = v.getHeaderID()
                        self.__outfile.write(LpSolve.getComment("Relative capacity constraint for entry vertex %d" % (headerID)))
                        iterationEdgeIDs = miniIPGs.getMiniIPG(headerID).getIterationEdgeIDs()
                        assert len(iterationEdgeIDs) == 1, "There should be exactly one iteration edge for the entry vertex %d. There are %d." % (headerID, len(iterationEdgeIDs))
                        edgeID = iter(iterationEdgeIDs).next()
                        self.__outfile.write(LpSolve.getEdgeVariable(edgeID))
                        self.__outfile.write(LpSolve.equals)
                        self.__outfile.write("1")
                        self.__outfile.write(LpSolve.semiColon)
                        self.__outfile.write(LpSolve.getNewLine(2))
                        
    def __createInnerLoopRelativeCapacityConstraint (self, data, lnt, miniIPGs, v):
        Debug.debugMessage("Analysing header %d" % v.getHeaderID(), 1)
        headerID       = v.getHeaderID()
        miniIPG        = miniIPGs.getMiniIPG(headerID)
        entryIpoints   = set([])
        ieDestinations = miniIPG.getIterationEdgeDestinations()
        decrementBound = {}
        for succID in v.getSuccessorIDs():
            if succID in ieDestinations:
                entryIpoints.add(succID)
                if self.__inLoopExit(succID, miniIPG, lnt, headerID):
                    decrementBound[succID] = False
                else:
                    decrementBound[succID] = True
        for ancestorv in lnt.getAllProperAncestors(v.getVertexID()):
            # Get the loop bound w.r.t. the ancestor loop
            ancestorHeaderID = ancestorv.getHeaderID()
            self.__outfile.write(LpSolve.getComment("Relative capacity constraint for header %d w.r.t to header %d" % (headerID, ancestorHeaderID)))
            bound = data.getLoopBound(headerID, ancestorHeaderID)
            # Write out the relative edges
            outerMiniIPG        = miniIPGs.getMiniIPG(ancestorHeaderID)
            outerIpoints        = set([])
            ieOuterDestinations = outerMiniIPG.getIterationEdgeDestinations()
            for succID in ancestorv.getSuccessorIDs():
                if succID in ieOuterDestinations:
                    outerIpoints.add(succID)
            if len(outerIpoints) == 0:
                Debug.debugMessage("Loop with header %d does not have Ipoints at its nesting level which are at entry Ipoints" % (ancestorHeaderID), 1)
                ieOuterSources = outerMiniIPG.getIterationEdgeSources()
                for succID in ancestorv.getSuccessorIDs():
                    if succID in ieOuterSources:
                        outerIpoints.add(succID)
            # Write out the entry Ipoints
            for entryID in entryIpoints:
                self.__outfile.write(LpSolve.getVertexVariable(entryID))
                self.__outfile.write(LpSolve.ltOrEqual)
                counter = len(outerIpoints)
                for vertexID in outerIpoints:
                    if decrementBound[entryID] and not lnt.isDoWhileLoop(headerID):
                        self.__outfile.write("%d %s" % (bound - 1, LpSolve.getVertexVariable(vertexID)))
                    else:
                        self.__outfile.write("%d %s" % (bound, LpSolve.getVertexVariable(vertexID)))
                    if counter > 1:
                        self.__outfile.write(LpSolve.plus)
                    counter -= 1
                self.__outfile.write(LpSolve.semiColon)
                self.__outfile.write(LpSolve.getNewLine())
            self.__outfile.write(LpSolve.getNewLine())
    
    def __inLoopExit (self, ipointID, miniIPG, lnt, headerID):
        for exitID in lnt.getLoopExits(headerID):
            if ipointID in miniIPG.getReachableSet(exitID):
                return True
        return False
            
    def __createIntegerConstraints (self, ipg):
        self.__outfile.write(LpSolve.int_)
        counter = ipg.numOfEdges()
        for v in ipg:
            for succID in v.getSuccessorIDs():                
                succe  = v.getSuccessorEdge(succID)
                edgeID = succe.getEdgeID()
                self.__outfile.write(" %s" % LpSolve.getEdgeVariable(edgeID))
                if counter > 1:
                    self.__outfile.write(LpSolve.comma)
                counter -= 1
        self.__outfile.write(LpSolve.semiColon)
        self.__outfile.write(LpSolve.getNewLine())
                
class CreateICFGILP (ILP):
    def __init__ (self, basepath, basename, data, icfg, lnt):
        ILP.__init__(self)
        filename = "%s.%s.%s.%s" % (basepath + os.sep + basename, icfg.getName(), "icfg", LpSolve.fileSuffix)
        self.__vertexIDToExecutionCount = {}
        with open(filename, 'w') as self.__outfile:
            self.__createObjectiveFunction(data, icfg)
            self.__createStructuralConstraints(icfg)
            self.__createRelativeCapacityConstraints(data, lnt, icfg)
            self.__createIntegerConstraints(icfg)
        self._solve(filename, LpSolve.vertexPrefix)
        Debug.debugMessage("WCET = %d" % self._wcet, 1)             
        self.__printExecutionCounts(icfg)

    def __printExecutionCounts (self, icfg):
        for bbID, count in self._variableToExecutionCount.iteritems():
            if not icfg.isIpoint(bbID):
                Debug.debugMessage("Execution count of variable %s = %d" % (LpSolve.getVertexVariable(bbID), count), 10)

    def __createObjectiveFunction (self, data, icfg):
        self.__outfile.write(LpSolve.max_)        
        counter = icfg.numOfVertices()
        for v in icfg:
            vertexID   = v.getVertexID()        
            vertexWCET = data.getBasicBlockWCET(vertexID)
            self.__outfile.write("%d %s" % (vertexWCET, LpSolve.getVertexVariable(vertexID)))
            if counter > 1:
                self.__outfile.write(LpSolve.plus)
            counter -= 1
        self.__outfile.write(LpSolve.semiColon)
        self.__outfile.write(LpSolve.getNewLine(2))
            
    def __createStructuralConstraints (self, icfg):
        for v in icfg:
            vertexID = v.getVertexID()
            self.__outfile.write(LpSolve.getComment("Vertex %d" % vertexID))
            # Flow into vertex w.r.t. predecessors
            self.__outfile.write(LpSolve.getVertexVariable(vertexID))
            self.__outfile.write(LpSolve.equals)
            counter = v.numberOfPredecessors()
            for predID in v.getPredecessorIDs():                    
                prede  = v.getPredecessorEdge(predID)
                edgeID = prede.getEdgeID()
                self.__outfile.write(LpSolve.getEdgeVariable(edgeID))
                if counter > 1:
                    self.__outfile.write(LpSolve.plus)
                counter -= 1
            self.__outfile.write(LpSolve.semiColon)
            self.__outfile.write(LpSolve.getNewLine())     
            # Flow in, flow out w.r.t to predecessors and successors
            counter = v.numberOfPredecessors()
            for predID in v.getPredecessorIDs():                    
                prede  = v.getPredecessorEdge(predID)
                edgeID = prede.getEdgeID()
                self.__outfile.write(LpSolve.getEdgeVariable(edgeID))
                if counter > 1:
                    self.__outfile.write(LpSolve.plus)
                counter -= 1
            self.__outfile.write(LpSolve.equals)
            counter = v.numberOfSuccessors()
            for succID in v.getSuccessorIDs():
                succe  = v.getSuccessorEdge(succID)
                edgeID = succe.getEdgeID()
                self.__outfile.write(LpSolve.getEdgeVariable(edgeID))
                if counter > 1:
                    self.__outfile.write(LpSolve.plus)
                counter -= 1
            self.__outfile.write(LpSolve.semiColon)
            self.__outfile.write(LpSolve.getNewLine(2))       
            
    def __createRelativeCapacityConstraints (self, data, lnt, icfg):
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, Vertices.HeaderVertex):
                    # Get iteration edges for this header
                    if level > 0:
                        self.__createInnerLoopRelativeCapacityConstraint(data, icfg, lnt, v)
                    else:
                        headerID = v.getHeaderID()
                        self.__outfile.write(LpSolve.getComment("Relative capacity constraint for entry vertex %d" % (headerID)))
                        self.__outfile.write(LpSolve.getVertexVariable(icfg.getEntryID()))
                        self.__outfile.write(LpSolve.equals)
                        self.__outfile.write("1")
                        self.__outfile.write(LpSolve.semiColon)
                        self.__outfile.write(LpSolve.getNewLine(2))
                        
    def __createInnerLoopRelativeCapacityConstraint (self, data, icfg, lnt, v):
        Debug.debugMessage("Analysing header %d" % v.getHeaderID(), 1)
        headerID         = v.getHeaderID()
        parentv          = lnt.getVertex(v.getParentID())
        relativeHeaderID = headerID
        for ancestorv in lnt.getAllProperAncestors(v.getVertexID()):
            # Get the loop bound w.r.t. the ancestor loop
            ancestorHeaderID = ancestorv.getHeaderID()
            bound = data.getLoopBound(headerID, ancestorHeaderID)
            self.__outfile.write(LpSolve.getComment("Relative capacity constraint for header %d w.r.t to header %d" % (headerID, ancestorHeaderID)))
            self.__outfile.write(LpSolve.getComment("Bound = %d" % bound))
            self.__outfile.write(LpSolve.getVertexVariable(headerID))
            self.__outfile.write(LpSolve.ltOrEqual)
            # Write out the relative edges
            relativeEdgeIDs = self.__getLoopEntryEdges(icfg, lnt, relativeHeaderID)
            counter         = len(relativeEdgeIDs)
            for edgeID in relativeEdgeIDs:
                self.__outfile.write("%d %s" % (bound, LpSolve.getEdgeVariable(edgeID)))
                if counter > 1:
                    self.__outfile.write(LpSolve.plus)
                counter -= 1
            parentv          = lnt.getVertex(v.getParentID())
            relativeHeaderID = parentv.getHeaderID()
            self.__outfile.write(LpSolve.semiColon)
            self.__outfile.write(LpSolve.getNewLine(2))
            
    def __getLoopEntryEdges (self, icfg, lnt, headerID):
        edgeIDs = []
        v       = icfg.getVertex(headerID)
        for predID in v.getPredecessorIDs():
            if not lnt.isLoopBackEdge(predID, headerID):
                prede = v.getPredecessorEdge(predID)
                edgeIDs.append(prede.getEdgeID())
        assert edgeIDs, "Unable to find loop-entry edges into loop with header %d" % headerID
        return edgeIDs
    
    def __createIntegerConstraints (self, icfg):
        self.__outfile.write(LpSolve.int_)
        counter = icfg.numOfVertices() + icfg.numOfEdges()
        for v in icfg:
            vertexID = v.getVertexID()
            self.__outfile.write(" %s" % LpSolve.getVertexVariable(vertexID))
            if counter > 1:
                self.__outfile.write(LpSolve.comma)
            counter -= 1
            for succID in v.getSuccessorIDs():
                succe  = v.getSuccessorEdge(succID)
                edgeID = succe.getEdgeID()
                self.__outfile.write(" %s" % LpSolve.getEdgeVariable(edgeID))
                if counter > 1:
                    self.__outfile.write(LpSolve.comma)
                counter -= 1
        self.__outfile.write(LpSolve.semiColon)
        self.__outfile.write(LpSolve.getNewLine())
    