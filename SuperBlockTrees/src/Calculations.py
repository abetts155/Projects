from Vertices import HeaderVertex, AdditionVertex, MultiplicationVertex, MaximumVertex, SuperBlock
from Trees import DepthFirstSearch
from DirectedGraphs import DirectedGraph
import Debug, Visualisation
import os, re, timeit

class WCETCalculation:
    def __init__ (self, program, data, basepath, basename, repeatability):
        AETSolvingTime = 0
        ILPSolvingTime = 0
        AETConstructionTime = 0
        ILPConstructionTime = 0
        for i in xrange(0, repeatability):
            data.assignRandomWCETs()
            self.__contextDataTrees = {}
            self.__contextDataILPs  = {}
            contextg = program.getContextGraph()
            dfs      = DepthFirstSearch(contextg, contextg.getRootID())
            for vertexID in dfs.getPostorder():
                contextv     = contextg.getVertex(vertexID)
                functionName = contextv.getName()
                Debug.verboseMessage("Doing WCET calculation on %s" % functionName)
                cfg         = program.getCFG(functionName)
                lnt         = program.getLNT(functionName)
                superg      = program.getSuperBlockCFG(functionName)
                arithmetict = ArithmeticExpressionTree(functionName, superg, cfg, lnt)
                treeWCET    = arithmetict.evaluate(data, self.__contextDataTrees, contextv)
                Debug.verboseMessage("Tree:: WCET(%s)=%s (SOLVE TIME=%.5f) (CONSTRUCTION TIME=%.5f) (TOTAL TIME=%.5f)" % (functionName, 
                                                                                                        treeWCET, 
                                                                                                        arithmetict.solvingTime, 
                                                                                                        arithmetict.constructionTime,
                                                                                                        arithmetict.solvingTime + arithmetict.constructionTime))
                AETSolvingTime += arithmetict.solvingTime
                AETConstructionTime += arithmetict.constructionTime
                self.__contextDataTrees[contextv.getVertexID()] = treeWCET
                Visualisation.generateGraphviz(arithmetict, "%s.%s" % (functionName, "aet"))
                Visualisation.makeUdrawFile(arithmetict, "%s.%s" % (functionName, "aet"))
                ilp     = CreateCFGILP(basepath, basename, data, self.__contextDataILPs, contextv, cfg, lnt)
                ilpWCET = ilp.solve()
                Debug.verboseMessage("ILP::  WCET(%s)=%d (SOLVE TIME=%.5f) (CONSTRUCTION TIME=%.5f) (TOTAL TIME=%.5f)" % (functionName, 
                                                                                                        ilpWCET, 
                                                                                                        ilp.solvingTime, 
                                                                                                        ilp.constructionTime,
                                                                                                        ilp.solvingTime + ilp.constructionTime))
                ILPSolvingTime += ilp.solvingTime
                ILPConstructionTime += ilp.constructionTime
                self.__contextDataILPs[contextv.getVertexID()] = ilpWCET
                if i == 0:
                    Debug.verboseMessage("CFG size:: vertices=%d edges=%d" % (cfg.numOfVertices(), cfg.numOfEdges()))
                    Debug.verboseMessage("AET size:: vertices=%d edges=%d" % (arithmetict.numOfVertices(), arithmetict.numOfEdges()))
                    Debug.verboseMessage("ILP size:: constraints=%d variables=%d" % (ilp.numOfConstraints(), ilp.numOfVariables()))
        Debug.verboseMessage("TREE:: (OVERALL SOLVE TIME=%.5f) (OVERALL CONSTRUCTION TIME=%.5f) (OVERALL TIME=%.5f)" % 
                             (AETSolvingTime/repeatability, 
                              AETConstructionTime/repeatability,
                              (AETSolvingTime+AETConstructionTime)/repeatability))
        Debug.verboseMessage("ILP::  (OVERALL SOLVE TIME=%.5f) (OVERALL CONSTRUCTION TIME=%.5f) (OVERALL TIME=%.5f)" % 
                             (ILPSolvingTime/repeatability, 
                              ILPConstructionTime/repeatability,
                              (ILPSolvingTime+ILPConstructionTime)/repeatability))
                 
class ArithmeticExpressionTree (DirectedGraph):
    def __init__(self, functionName, superg, cfg, lnt):
        start = timeit.default_timer()
        DirectedGraph.__init__(self)
        self.__functionName = functionName
        self.__superg = superg
        self.__cfg = cfg
        self.__lnt = lnt
        self.__superBlocksToAETSuperBlocks = {}
        self.__headerSuperBlocks = set([])
        self.__addSuperBlocks()
        self.__addOperators()
        self.constructionTime = (timeit.default_timer() - start)
    
    def __addSuperBlocks (self):
        supervToHeaderSuperv = {}
        largestVertexID = 0
        for superv in self.__superg:
            self.__superBlocksToAETSuperBlocks[superv] = []
            bbIDs     = set([])
            headerIDs = set([])
            for bbID in superv.getBasicBlockIDs():
                if not self.__lnt.isLoopHeader(bbID) or superv.getLoopHeader() == bbID:
                    bbIDs.add(bbID)
                else:
                    headerIDs.add(bbID)
            if bbIDs or superv.numberOfEdges() > 0:
                supervID  = superv.getVertexID()
                newSuperv = SuperBlock(supervID)
                self.vertices[supervID] = newSuperv
                largestVertexID = max(largestVertexID, supervID)
                newSuperv.addBasicBlocks(bbIDs)
                newSuperv.setLoopHeader(superv.getLoopHeader())
                newSuperv.addEdges(superv.getEdges())
                self.__superBlocksToAETSuperBlocks[superv].append(newSuperv)
            if headerIDs:
                supervToHeaderSuperv[superv] = headerIDs
        self.__nextVertexID = largestVertexID
        for superv, headerIDs in supervToHeaderSuperv.iteritems():
            newSupervID = self.getNextVertexID()
            newSuperv   = SuperBlock(newSupervID)
            newSuperv.addBasicBlocks(headerIDs)
            self.vertices[newSupervID] = newSuperv
            self.__superBlocksToAETSuperBlocks[superv].append(newSuperv)
            self.__headerSuperBlocks.add(newSupervID)
            
    def getNextVertexID (self):
        self.__nextVertexID += 1
        return self.__nextVertexID
    
    def __addOperators (self):
        self.__supervToTreeVertex    = {}
        self.__iterationPathsRootIDs = {}
        self.__exitPathsRootIDs      = {}
        self.__iterationPathsAETs    = {}
        self.__exitPathsAETs         = {}
        for level, vertices in self.__lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, HeaderVertex):
                    headerID = v.getHeaderID()
                    if v.getVertexID() != self.__lnt.getRootID():
                        supergRegion     = self.__superg.getIterationPathsSuperBlockRegion(headerID)
                        supergRegionRoot = self.__superg.getIterationPathsSuperBlockRegionRoot(headerID)
                        subgraph, root   = self.__buildSubtree(headerID, supergRegion, supergRegionRoot, False)
                        self.__iterationPathsRootIDs[headerID] = root
                        self.__iterationPathsAETs[headerID] = subgraph
                        if not self.__lnt.isDoWhileLoop(headerID):
                            supergRegion2     = self.__superg.getExitPathsSuperBlockRegion(headerID)
                            supergRegionRoot2 = self.__superg.getExitPathsSuperBlockRegionRoot(headerID)
                            subgraph, root = self.__buildSubtree(headerID, supergRegion2, supergRegionRoot2, True)
                            self.__exitPathsRootIDs[headerID] = root
                            self.__exitPathsAETs[headerID] = subgraph
                    else:
                        supergRegion2 = self.__superg.getExitPathsSuperBlockRegion(headerID)
                        rootv2        = self.__superg.getExitPathsSuperBlockRegionRoot(headerID)
                        subgraph, root = self.__buildSubtree(headerID, supergRegion2, rootv2, True)
                        self.__iterationPathsRootIDs[headerID] = root
                        self.__iterationPathsAETs[headerID] = subgraph
                        
    def __addVertex (self, subgraph, vertexID, v):
        subgraph.vertices[vertexID] = v
        self.vertices[vertexID] = v
              
    def __buildSubtree (self, headerID, supergRegion, rootv, acyclicRegion):
        subgraph = DirectedGraph()
        dfs      = DepthFirstSearch(supergRegion, rootv.getVertexID())
        for supervID in dfs.getPostorder():
            originalSuperv = self.__superg.getVertex(supervID)
            supervs = self.__superBlocksToAETSuperBlocks[originalSuperv]
            assert supervs, "Unable to find super blocks in AET for super block %s" % originalSuperv
            if len(supervs) == 2:
                bbSuperv     = supervs[0]
                assert bbSuperv.getVertexID() == supervID
                headerSuperv = supervs[1]
                subgraph.vertices[bbSuperv.getVertexID()] = bbSuperv
                subgraph.vertices[headerSuperv.getVertexID()] = headerSuperv
                # Addition vertex to sum up WCETs of basic blocks within super block
                addvID = self.getNextVertexID()
                addv   = AdditionVertex(addvID, headerID, acyclicRegion)
                self.__addVertex(subgraph, addvID, addv)
                self.addEdge(addvID, supervID)
                # Multiplication vertex to factor WCET contribution of super block
                multiplyvID = self.getNextVertexID()
                multiplyv   = MultiplicationVertex(multiplyvID, headerID, acyclicRegion)
                self.__addVertex(subgraph, multiplyvID, multiplyv)
                self.addEdge(multiplyvID, addvID)
                self.__supervToTreeVertex[supervID] = multiplyvID
                # Addition vertex to include contribution of inner loops
                addvID2 = self.getNextVertexID()
                addv2   = AdditionVertex(addvID2, headerID, acyclicRegion)
                self.__addVertex(subgraph, addvID2, addv2)
                self.addEdge(addvID2, multiplyvID)
                self.addEdge(addvID2, headerSuperv.getVertexID())
                self.__supervToTreeVertex[supervID] = addvID2
            else:
                newSuperv = supervs[0]
                subgraph.vertices[newSuperv.getVertexID()] = newSuperv
                if newSuperv.getVertexID() not in self.__headerSuperBlocks:
                    # Addition vertex to sum up WCETs of basic blocks within super block
                    addvID = self.getNextVertexID()
                    addv   = AdditionVertex(addvID, headerID, acyclicRegion)
                    self.__addVertex(subgraph, addvID, addv)
                    self.addEdge(addvID, supervID)
                    # Multiplication vertex to factor WCET contribution of super block
                    multiplyvID = self.getNextVertexID()
                    multiplyv   = MultiplicationVertex(multiplyvID, headerID, acyclicRegion)
                    self.__addVertex(subgraph, multiplyvID, multiplyv)
                    self.addEdge(multiplyvID, addvID)
                    self.__supervToTreeVertex[supervID] = multiplyvID
                else:
                    # Addition vertex to include contribution of inner loops
                    addvID = self.getNextVertexID()
                    addv   = AdditionVertex(addvID, headerID, acyclicRegion)
                    self.__addVertex(subgraph, addvID, addv)
                    self.addEdge(addvID, newSuperv.getVertexID())
                    self.__supervToTreeVertex[supervID] = addvID
                    
            if originalSuperv.numberOfSuccessors(): 
                addvID = self.getNextVertexID()
                addv   = AdditionVertex(addvID, headerID, acyclicRegion)
                self.__addVertex(subgraph, addvID, addv)
                self.addEdge(addvID, self.__supervToTreeVertex[supervID])
                self.__supervToTreeVertex[supervID] = addvID
                for succEdges in originalSuperv.getBranchPartitions().values():
                    if len(succEdges) == 1:
                        succe  = succEdges[0]
                        succID = succe.getVertexID()
                        self.addEdge(addvID, self.__supervToTreeVertex[succID])
                    else:
                        maxvID = self.getNextVertexID()
                        maxv   = MaximumVertex(maxvID, headerID, acyclicRegion)
                        self.__addVertex(subgraph, maxvID, maxv)
                        self.addEdge(addvID, maxvID)
                        for succe in succEdges:
                            succID = succe.getVertexID()
                            multiplyvID = self.__supervToTreeVertex[succID]
                            self.addEdge(maxvID, multiplyvID)                          
        return subgraph, self.__supervToTreeVertex[rootv.getVertexID()]
        
    def __evaluateSuperBlock (self, superv, data, contextWCETs, contextv):
        wcet = 0
        for bbID in superv.getBasicBlockIDs():
            if not self.__lnt.isLoopHeader(bbID) or superv.getLoopHeader() == bbID:
                wcet += data.getExecutionTime(self.__functionName, bbID)
                if self.__cfg.isCallSite(bbID):
                    calleeContextID   = contextv.getSuccessorWithCallSite(bbID)
                    calleeContextWCET = contextWCETs[calleeContextID]
                    wcet += calleeContextWCET
            if self.__lnt.isLoopHeader(bbID) and superv.getLoopHeader() != bbID:
                wcet += self.__headerToWCET[bbID]
        return wcet
    
    def __propagateBounds (self, headerv, data):
        headerID = headerv.getHeaderID()
        if not self.__lnt.isDoWhileLoop(headerID) and headerv.getVertexID() != self.__lnt.getRootID():
            # If not a do-while loop and not the dummy loop, peel off the iterations which came from outside the loop body
            headerInvocations = data.getBound(self.__functionName, headerID)
            freshInvocations  = data.getFreshInvocations(self.__functionName, headerID)
            totalCount        = headerInvocations - freshInvocations
        else:
            totalCount = data.getBound(self.__functionName, headerID)
        iterationAET = self.__iterationPathsAETs[headerID]
        for v in iterationAET:
            if not isinstance(v, SuperBlock):
                v.setBound(totalCount)
        #  Now propagate bound downwards to acyclic region
        if not self.__lnt.isDoWhileLoop(headerID) and headerv.getVertexID() != self.__lnt.getRootID(): 
            totalCount = data.getFreshInvocations(self.__functionName, headerID)
            exitAET    = self.__exitPathsAETs[headerID]
            for v in exitAET:
                if not isinstance(v, SuperBlock):
                    v.setBound(totalCount)
                    
    def __evaluateDFS (self, dfs, data, contextWCETs, contextv):
        for vertexID in dfs.getPostorder():
            v = self.getVertex(vertexID)
            if isinstance(v, AdditionVertex):
                time  = 0
                for succID in v.getSuccessorIDs():
                    succv = self.getVertex(succID)
                    if isinstance(succv, SuperBlock):
                        time += self.__evaluateSuperBlock(succv, data, contextWCETs, contextv)
                    else:
                        time += succv.getWCET()
                v.setWCET(time)
            elif isinstance(v, MultiplicationVertex):
                # Get only child of multiplication vertex
                assert v.numberOfSuccessors() == 1
                addv = self.getVertex(v.getSuccessorIDs()[0])
                v.setWCET(addv.getWCET() * v.getBound())
            elif isinstance(v, MaximumVertex):
                time = 0
                for succID in v.getSuccessorIDs():
                    succv = self.getVertex(succID)
                    time  = max(time, succv.getWCET())
                v.setWCET(time)
    
    def evaluate (self, data, contextWCETs, contextv):
        start = timeit.default_timer()
        self.__headerToWCET = {}
        for level, vertices in self.__lnt.levelIterator(True):
            for lntv in vertices:
                if isinstance(lntv, HeaderVertex):
                    # Propagate bounds downwards in tree
                    self.__propagateBounds(lntv, data)
                    headerID = lntv.getHeaderID()
                    iterationAET       = self.__iterationPathsAETs[headerID]
                    iterationAETRootID = self.__iterationPathsRootIDs[headerID]
                    dfs                = DepthFirstSearch(iterationAET, iterationAETRootID)
                    self.__evaluateDFS(dfs, data, contextWCETs, contextv)
                    iterationAETWCET   = iterationAET.getVertex(iterationAETRootID).getWCET()
                    if not self.__lnt.isDoWhileLoop(headerID) and lntv.getVertexID() != self.__lnt.getRootID():
                        exitAET       = self.__exitPathsAETs[headerID]
                        exitAETRootID = self.__exitPathsRootIDs[headerID]
                        dfs2          = DepthFirstSearch(exitAET, exitAETRootID)
                        self.__evaluateDFS(dfs2, data, contextWCETs, contextv)
                        exitAETWCET   = exitAET.getVertex(exitAETRootID).getWCET()
                        self.__headerToWCET[headerID] = iterationAETWCET + exitAETWCET
                    else:
                        self.__headerToWCET[headerID] = iterationAETWCET
        lntRootv = self.__lnt.getVertex(self.__lnt.getRootID())
        self.solvingTime = (timeit.default_timer() - start)
        return self.__headerToWCET[lntRootv.getHeaderID()] 

def getNewLine (num=1):
    return "\n" * num 
           
class ECLIPSE:
    conjunct        = "," + getNewLine()
    clauseSep       = ":-"
    domainSep       = " #:: "
    equals          = " #= "
    fileSuffix      = "ecl"
    gt              = " #> "
    gtOrEqual       = " #>= "
    implies         = " => "
    lt              = " #< "
    ltOrEqual       = " #=< "
    multiply        = "*"
    plus            = " + "
    terminator      = "."
    
    @staticmethod
    def getComment (comment):
        return "% " + comment + getNewLine()
    
    @staticmethod
    def getEdgeCountVariable (sourceID, destinationID):
        return "E_%d_%d" % (sourceID, destinationID)
    
    @staticmethod
    def getVertexCountVariable (vertexID):
        return "V_%d" % (vertexID)
    
    @staticmethod
    def getVertexWCETVariable (vertexID):
        return "W_%d" % (vertexID)
    
    @staticmethod
    def getTempList (suffix):
        return "VARS%d" % suffix
    
class CLP ():
    WCET        = "WCET"
    PWCET       = "PWCET"
    BB_TIMES    = "BB_TIMES"
    BB_COUNTS   = "BB_COUNTS"
    EDGE_COUNTS = "EDGE_COUNTS"
    OUTPUT_PREDICATE_HEAD = "print_results"
    
    def __init__ (self):
        self._lines = []
        self.__addRequiredPackages()
        self.__goal = "solve(%s)" % (CLP.WCET)
        self._lines.append("%s%s%s" % (self.__goal, ECLIPSE.clauseSep, getNewLine()))
        
    def __addRequiredPackages (self):
        self._lines.append(ECLIPSE.getComment("Packages"))
        libs = ['ic', 'branch_and_bound', 'lists', 'util']
        for lib in libs:
            self._lines.append("%s%s(%s)%s%s" % (ECLIPSE.clauseSep, 'lib', lib, ECLIPSE.terminator, getNewLine()))
        self._lines.append(getNewLine())
        
    def _solve(self, basepath, basename, contextv, filename):
        from subprocess import Popen, PIPE
        import shlex
        with open(filename, 'w') as clpFile:
            for line in self._lines:
                clpFile.write(line)                
        Debug.debugMessage("Solving CLP in %s" % filename, 10)
        command    = 'jeclipse -b %s -e "%s."' % (filename, self.__goal) 
        proc       = Popen(command, shell=True, executable="/bin/bash", stdout=PIPE, stderr=PIPE)
        returnCode = proc.wait()
        WCET = 0
        if returnCode != 0:
            Debug.warningMessage("Running '%s' failed" % command)
        for line in proc.stdout.readlines():
            if line.startswith("Found a solution with cost"):
                lexemes = shlex.split(line)
                value   = int(lexemes[-1])
                WCET    = max(WCET, -1 * value)
        Debug.verboseMessage("CLP:: WCET(%s) = %d" % (contextv.getName(), WCET)) 
        return WCET
    
    def _addOutputPredicates (self, filename):
        fileHandle = "F"        
        self._lines.append('%s(%s,%s) %s' % (CLP.OUTPUT_PREDICATE_HEAD, CLP.BB_COUNTS, CLP.EDGE_COUNTS, ECLIPSE.clauseSep))
        self._lines.append(getNewLine())
        self._lines.append('open("%s",%s,%s)%s' % (os.path.basename(filename) + ".res", "write", fileHandle, ECLIPSE.conjunct))
        self._lines.append('print_list(%s,"%s: ",%s)%s' % (fileHandle, CLP.BB_COUNTS, CLP.BB_COUNTS, ECLIPSE.conjunct))
        self._lines.append('print_list(%s,"%s: ",%s)%s' % (fileHandle, CLP.EDGE_COUNTS, CLP.EDGE_COUNTS, ECLIPSE.conjunct))
        self._lines.append('close(%s)%s' % (fileHandle, ECLIPSE.terminator))
        self._lines.append(getNewLine(2))
        
        self._lines.append('print_list(_,_,[])%s' % (ECLIPSE.terminator))
        self._lines.append(getNewLine())
        self._lines.append('print_list(%s,Name,[H|T])%s' % (fileHandle, ECLIPSE.clauseSep))
        self._lines.append(getNewLine())
        self._lines.append('write(%s,Name)%s' % (fileHandle, ECLIPSE.conjunct))
        self._lines.append('write(%s,H)%s' % (fileHandle, ECLIPSE.conjunct))
        self._lines.append('print_list1(%s,T)%s' % (fileHandle, ECLIPSE.conjunct))
        self._lines.append('nl(%s)%s' % (fileHandle, ECLIPSE.terminator))
        self._lines.append(getNewLine(2))
        
        self._lines.append('print_list1(_,[])%s' % (ECLIPSE.terminator))
        self._lines.append(getNewLine())
        self._lines.append('print_list1(%s,[H|T])%s' % (fileHandle, ECLIPSE.clauseSep))
        self._lines.append(getNewLine())
        self._lines.append('write(%s,",")%s' % (fileHandle, ECLIPSE.conjunct))
        self._lines.append('write(%s,H)%s' % (fileHandle, ECLIPSE.conjunct))
        self._lines.append('print_list1(%s,T)%s' % (fileHandle, ECLIPSE.terminator))
        self._lines.append(getNewLine(2))
   
class CreateCFGCLP (CLP):
    def __init__ (self):
        CLP.__init__(self)

    def _addVariables (self, cfg):
        self._lines.append(ECLIPSE.getComment("Declarations"))
        bbCounts   = []
        bbTimes    = []
        edgeCounts = []
        for v in cfg:
            bbCounts.append(ECLIPSE.getVertexCountVariable(v.getVertexID()))
            bbTimes.append(ECLIPSE.getVertexWCETVariable(v.getVertexID()))
            for succID in v.getSuccessorIDs():
                edgeCounts.append(ECLIPSE.getEdgeCountVariable(v.getVertexID(), succID))
        self._lines.append("%s = [%s]%s" % (CLP.BB_COUNTS, ','.join(var for var in bbCounts), ECLIPSE.conjunct))
        self._lines.append("%s = [%s]%s" % (CLP.BB_TIMES, ','.join(var for var in bbTimes), ECLIPSE.conjunct))
        self._lines.append("%s = [%s]%s" % (CLP.EDGE_COUNTS, ','.join(var for var in edgeCounts),  ECLIPSE.conjunct))
        self._lines.append(getNewLine())
    
    def _addObjectiveFunction (self, cfg):
        self._lines.append(ECLIPSE.getComment("Objective function"))
        rhs   = ""
        count = 1
        for v in cfg:
            rhs += "%s%s%s" % (ECLIPSE.getVertexCountVariable(v.getVertexID()), ECLIPSE.multiply, ECLIPSE.getVertexWCETVariable(v.getVertexID()))
            if count < cfg.numOfVertices():
                rhs += ECLIPSE.plus
            count += 1
        self._lines.append("%s%s%s%s" % (CLP.WCET, ECLIPSE.equals, rhs, ECLIPSE.conjunct))
        self._lines.append(getNewLine())  
        
    def _addExecutionTimeDomains (self, data, contextWCETs, contextv, cfg):
        self._lines.append(ECLIPSE.getComment("Timing constraints"))
        for v in cfg:
            wcet = data.getExecutionTime(cfg.getName(), v.getOriginalVertexID())
            if cfg.isCallSite(v.getVertexID()):
                    calleeContextID   = contextv.getSuccessorWithCallSite(v.getVertexID())
                    calleeContextWCET = contextWCETs[calleeContextID]
                    wcet += calleeContextWCET
            self._lines.append("%s%s%d%s" % \
                                   (ECLIPSE.getVertexWCETVariable(v.getVertexID()), ECLIPSE.equals, wcet, ECLIPSE.conjunct))
        self._lines.append(getNewLine())   
        
    def _addStructuralConstraints (self, cfg):
        self._lines.append(ECLIPSE.getComment("Structural constraints"))
        for v in cfg:
            self._lines.append(ECLIPSE.getComment("Vertex %d" % v.getVertexID()))
            # Flow out to successor edges
            rhs   = ""
            count = 1
            for predID in v.getPredecessorIDs():
                rhs += ECLIPSE.getEdgeCountVariable(predID, v.getVertexID())
                if count < v.numberOfPredecessors():
                    rhs += ECLIPSE.plus
                count += 1
            self._lines.append("%s%s%s%s" % (ECLIPSE.getVertexCountVariable(v.getVertexID()), ECLIPSE.equals, rhs, ECLIPSE.conjunct))
            # Flow in/out through predecessor/successor edges
            lhs   = ""
            count = 1
            for succID in v.getSuccessorIDs():
                lhs += ECLIPSE.getEdgeCountVariable(v.getVertexID(), succID)
                if count < v.numberOfSuccessors():
                    lhs += ECLIPSE.plus
                count += 1
            rhs   = ""
            count = 1
            for predID in v.getPredecessorIDs():
                rhs += ECLIPSE.getEdgeCountVariable(predID, v.getVertexID())
                if count < v.numberOfPredecessors():
                    rhs += ECLIPSE.plus
                count += 1
            self._lines.append("%s%s%s%s" % (lhs, ECLIPSE.equals, rhs, ECLIPSE.conjunct))
        self._lines.append(getNewLine())
        
    def _addRelativeCapacityConstraints (self, data, cfg, lnt):
        self._lines.append(ECLIPSE.getComment("Relative capacity constraints"))
        for level, vertices in lnt.levelIterator(True):
            for treev in vertices:
                if isinstance(treev, HeaderVertex):
                    headerID = treev.getHeaderID()
                    self._lines.append(ECLIPSE.getComment("Capacity constraints on header %d" % treev.getHeaderID()))
                    if treev.getVertexID() == lnt.getRootID():
                        self._lines.append("%s%s1%s" % (ECLIPSE.getEdgeCountVariable(cfg.getExitID(), cfg.getEntryID()), ECLIPSE.equals, ECLIPSE.conjunct))
                    else:
                        headerv = cfg.getVertex(headerID)
                        for ancestorv in lnt.getAllProperAncestors(treev.getVertexID()):
                            if ancestorv.getVertexID() == treev.getParentID():
                                forwardPredIDs = []
                                for prede in headerv.getPredecessorEdges():
                                    if not lnt.isLoopBackEdge(prede.getVertexID(), headerID):
                                        forwardPredIDs.append((prede.getVertexID(), headerID))
                                rhs   = ""
                                count = 1
                                for edge in forwardPredIDs:
                                    bound = data.getLoopBound(cfg.getName(), headerID)
                                    rhs += "%d%s%s" % (bound, ECLIPSE.multiply, ECLIPSE.getEdgeCountVariable(edge[0], edge[1]))
                                    if count < len(forwardPredIDs):
                                        rhs += ECLIPSE.plus
                                    count += 1
                                self._lines.append("%s%s%s%s" % (ECLIPSE.getVertexCountVariable(headerID), ECLIPSE.ltOrEqual, rhs, ECLIPSE.conjunct))
                            else:
                                pass
        self._lines.append(getNewLine()) 
        
    def __computeUpperCapacityConstraint (self, data, cfg, lnt, loopv):
        bound = data.getLoopBound(cfg.getName(), loopv.getHeaderID())
        while loopv.getVertexID() != lnt.getRootID():
            loopv  = lnt.getVertex(loopv.getParentID())
            bound *= data.getLoopBound(cfg.getName(), loopv.getHeaderID())
        return bound
    
    def _addExecutionCountDomains (self, data, superg, pathg, cfg, lnt, addPathInformation=False):
        self._lines.append(ECLIPSE.getComment("Execution count domains"))
        # First add domains for basic blocks
        for headerID in lnt.getHeaderIDs():
            for superv in superg.getIterationPathSuperBlockRegion(headerID):
                if superv.getBasicBlockIDs():           
                    lowerBound = 0   
                    treev      = lnt.getVertex(superv.getRepresentativeID())
                    headerv    = lnt.getVertex(treev.getParentID())
                    upperBound = self.__computeUpperCapacityConstraint(data, cfg, lnt, headerv)
                    if addPathInformation and pathg.hasVertex(superv.getVertexID()):
                        if data.neverExecutes(pathg, superv):
                            upperBound = 0
                        else:
                            lowerBound = data.getMinimumExecutionCount(pathg, superv)
                    for vertexID in superv.getBasicBlockIDs():
                        if not (lnt.isLoopHeader(vertexID) and vertexID != headerID):
                            line = "%s%s[%d..%d]%s" % \
                            (ECLIPSE.getVertexCountVariable(vertexID), ECLIPSE.domainSep, lowerBound, upperBound, ECLIPSE.conjunct)
                            self._lines.append(line)   
                                
        # Now add domains for edges
        for v in cfg:            
            vertexID = v.getVertexID()
            for succID in v.getSuccessorIDs(): 
                treev1     = lnt.getVertex(vertexID)
                headerv1   = lnt.getVertex(treev1.getParentID())
                treev2     = lnt.getVertex(succID)
                headerv2   = lnt.getVertex(treev2.getParentID())  
                lowerBound = 0              
                if headerv1 == headerv2:
                    upperBound = self.__computeUpperCapacityConstraint(data, cfg, lnt, headerv1)
                else:
                    bound1 = self.__computeUpperCapacityConstraint(data, cfg, lnt, headerv1)
                    bound2 = self.__computeUpperCapacityConstraint(data, cfg, lnt, headerv2)
                    upperBound = max(bound1, bound2)  
                if addPathInformation and pathg.isMonitoredEdge(vertexID, succID):
                    superv = pathg.getMonitoredEdgeSuperBlock(vertexID, succID)
                    if data.neverExecutes(pathg, superv):
                        upperBound = 0
                    else:
                        lowerBound = data.getMinimumExecutionCount(pathg, superv)   
                line = "%s%s[%d..%d]%s" % \
                (ECLIPSE.getEdgeCountVariable(vertexID, succID), ECLIPSE.domainSep, lowerBound, upperBound, ECLIPSE.conjunct)
                self._lines.append(line)
        self._lines.append(getNewLine()) 
        
    def _addEpilogue (self):
        self._lines.append("%s%s%d%s%s%s" % (CLP.PWCET, ECLIPSE.equals, -1, ECLIPSE.multiply, CLP.WCET, ECLIPSE.conjunct))
        self._lines.append("append(%s,%s,%s)%s" % (CLP.EDGE_COUNTS, CLP.BB_COUNTS, ECLIPSE.getTempList(0), ECLIPSE.conjunct))
        self._lines.append("append(%s,%s,%s)%s" % (CLP.BB_TIMES, ECLIPSE.getTempList(0), ECLIPSE.getTempList(1), ECLIPSE.conjunct))
        self._lines.append("time(bb_min(search(%s,0,input_order,indomain_max,complete,[]),%s,bb_options{timeout:%d}))%s" % \
                           (ECLIPSE.getTempList(1), CLP.PWCET, 900, ECLIPSE.conjunct))
        self._lines.append("%s(%s, %s)%s" % \
                           (CLP.OUTPUT_PREDICATE_HEAD, CLP.BB_COUNTS, CLP.EDGE_COUNTS, ECLIPSE.terminator))
        self._lines.append(getNewLine(2))

class CreateCFGCLPVanilla (CreateCFGCLP):
    def __init__ (self, basepath, basename, data, contextWCETs, contextv, cfg, lnt, superg, pathg):
        CreateCFGCLP.__init__(self)
        filename = "%s.%s.context%s.%s.%s.vanilla" % (basepath + os.sep + basename, contextv.getName(), contextv.getVertexID(), "cfg", ECLIPSE.fileSuffix)
        self._addVariables(cfg)
        self._addObjectiveFunction(cfg)
        self._addExecutionTimeDomains(data, contextWCETs, contextv, cfg)
        self._addStructuralConstraints(cfg)
        self._addRelativeCapacityConstraints(data, cfg, lnt)
        self._addExecutionCountDomains(data, superg, pathg, cfg, lnt)
        self._addEpilogue()
        self._addOutputPredicates(filename)
        self._wcet = self._solve(basepath, basename, contextv, filename)

class CreateCFGCLPExtra (CreateCFGCLP):
    def __init__ (self, basepath, basename, data, contextWCETs, contextv, cfg, lnt, superg, pathg):
        CreateCFGCLP.__init__(self)
        filename = "%s.%s.context%s.%s.%s.extra" % (basepath + os.sep + basename, contextv.getName(), contextv.getVertexID(), "cfg", ECLIPSE.fileSuffix)
        self._addVariables(cfg)
        self._addObjectiveFunction(cfg)
        self._addExecutionTimeDomains(data, contextWCETs, contextv, cfg)
        self._addStructuralConstraints(cfg)
        self._addRelativeCapacityConstraints(data, cfg, lnt)
        self._addExecutionCountDomains(data, superg, pathg, cfg, lnt, True)
        self.__addInfeasiblePathConstraints(data, pathg, cfg, lnt)
        self._addEpilogue()
        self._addOutputPredicates(filename)
        self._wcet = self._solve(basepath, basename, contextv, filename)

    def __addInfeasiblePathConstraints (self, data, pathg, cfg, lnt):
        self._lines.append(ECLIPSE.getComment("Infeasible path constraints"))
        for superv in pathg:
            if superv.getBasicBlockIDs():
                countVariable1 = ECLIPSE.getVertexCountVariable(superv.getRepresentativeID())
            else:
                edge          = superv.getUniqueEdge()
                countVariable1 = ECLIPSE.getEdgeCountVariable(edge[0], edge[1])
            for succID in superv.getSuccessorIDs():
                succSuperv = pathg.getVertex(succID)
                if succSuperv.getBasicBlockIDs():
                    countVariable2 = ECLIPSE.getVertexCountVariable(succSuperv.getRepresentativeID())
                else:
                    edge           = succSuperv.getUniqueEdge()
                    countVariable2 = ECLIPSE.getEdgeCountVariable(edge[0], edge[1])
                self._lines.append("%s%s0%s%s%s0%s" % \
                               (countVariable1, ECLIPSE.gt, ECLIPSE.implies, countVariable2, ECLIPSE.equals, ECLIPSE.conjunct))
                self._lines.append("%s%s0%s%s%s0%s" % \
                               (countVariable2, ECLIPSE.gt, ECLIPSE.implies, countVariable1, ECLIPSE.equals, ECLIPSE.conjunct))
        self._lines.append(getNewLine())  

class LpSolve:
    comma        = ","
    dummyPrefix  = "d_"
    edgePrefix   = "e_"
    equals       = " = "
    fileSuffix   = "ilp"
    gt           = " > "
    gtOrEqual    = " >= "
    int_         = "int"
    lt           = " < "
    ltOrEqual    = " <= "
    max_         = "max: "
    plus         = " + "
    semiColon    = ";"
    vertexPrefix = "v_"
    
    @staticmethod
    def getDummyVariable (edgeID, index=0):
        if index:
            return "%s%s_%d" % (LpSolve.dummyPrefix, edgeID, index)
        return "%s%d" % (LpSolve.dummyPrefix, edgeID)
    
    @staticmethod
    def getEdgeVariable (sourceID, destinationID, index=0):
        if index:
            return "%s%d_%d_%d" % (LpSolve.edgePrefix, sourceID, destinationID, index)
        return "%s%d_%d" % (LpSolve.edgePrefix, sourceID, destinationID)
    
    @staticmethod
    def getVertexVariable (vertexID, index=0):
        if index:
            return "%s%s_%d" % (LpSolve.vertexPrefix, vertexID, index)
        return "%s%d" % (LpSolve.vertexPrefix, vertexID)
    
    @staticmethod
    def getComment (comment):
        return "// " + comment + "\n"
    
class ILP ():
    def __init__ (self):
        self._wcet = 0
        self._variableToExecutionCount = {}
        self._constraints = []
        self._variables = set([])
        self._filename = None
        
    def numOfConstraints (self):
        count = 0
        for constraint in self._constraints:
            if not re.match(r'//.*', constraint):
                count += 1
        return count
    
    def numOfVariables (self):
        return len(self._variables)
    
    def solve (self):
        import shlex, decimal
        from subprocess import Popen, PIPE
        assert self._filename, "ILP filename has not been set"
        Debug.debugMessage("Solving ILP for %s" % self._filename, 10)
        command    = "lp_solve %s -S1 -time" % self._filename 
        start = timeit.default_timer()
        proc       = Popen(command, shell=True, stdout=PIPE, stderr=PIPE, executable="/bin/bash")
        returnCode = proc.wait()
        self.solvingTime = (timeit.default_timer() - start)
        if returnCode != 0:
            Debug.warningMessage("Running '%s' failed" % command)
            return 0
        for line in proc.stdout.readlines():
            if line.startswith("Value of objective function"):
                lexemes = shlex.split(line)
                self._wcet = long(decimal.Decimal(lexemes[-1])) 
        assert self._wcet
        for line in proc.stderr.readlines():
            if line.startswith("CPU Time for Parsing"):
                lexemes = shlex.split(line)
                time    = lexemes[5][:-1]
                #self.solvingTime -= float(time)
        return self._wcet
                    
class CreateSuperBlockCFGILP (ILP):
    def __init__ (self, basepath, basename, superg, lnt, soughtSolutions=1):
        ILP.__init__(self)
        self._variables   = set([])
        self.__createConstraints(superg, lnt)
        self.__createObjectiveFunction(lnt)
        self.__createIntegerConstraint()
        filename = "%s.%s.%s.%s" % (basepath + os.sep + basename, superg.getName(), "superg", LpSolve.fileSuffix)
        solution = True
        while solution and soughtSolutions:
            soughtSolutions -= 1
            if self._wcet != -1:
                self.__addMaximumWCETConstraint()
            with open(filename, 'w') as ilpFile:
                for constraint in self._constraints:
                    ilpFile.write(constraint)        
            solution = self._solve(filename)
            if solution:
                Debug.verboseMessage("IPET:: WCET(%s) = %d" % (superg.getName(), self._wcet))
            
    def __addMaximumWCETConstraint (self):
        intConstraint = self._constraints.pop()
        constraint    = self._constraints[0][len(LpSolve.max_):-self.__endOfObjectiveFunction]
        constraint    += LpSolve.ltOrEqual
        constraint    += str(self._wcet - 1)
        constraint    += LpSolve.semiColon
        constraint    += getNewLine()
        self._constraints.append(constraint)
        self._constraints.append(intConstraint)
        
    def __createConstraints(self, superg, lnt):
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, HeaderVertex):
                    headerID = v.getHeaderID()
                    Debug.debugMessage("Analysing header %d" % headerID, 1)
                    supergRegion = superg.getIterationPathSuperBlockRegion(headerID)
                    self.__createCapacityConstraints(v, lnt)
                    self.__createFlowConstraints(headerID, supergRegion)
                    self.__addExclusiveConstraints(superg, headerID)
        
    def __addExclusiveConstraints (self, superg, headerID):
        for exclusiveTuple in superg.getSuperBlockPathInformationGraph().exclusiveTuples:
            comment = LpSolve.getComment("Mutual exclusive constraint")
            self._constraints.append(comment)
            constraint = ""
            sizeOfExclusiveSet = len(exclusiveTuple)
            num = 1
            for superv in exclusiveTuple:
                if superv.getBasicBlockIDs():
                    constraint += LpSolve.getVertexVariable(superv.getRepresentativeID(), headerID)
                else:
                    edges = superv.getEdges()
                    assert len(edges) == 1
                    edge = list(edges)[0]
                    constraint += LpSolve.getEdgeVariable(edge[0], edge[1], headerID)
                if num < sizeOfExclusiveSet:
                    constraint += LpSolve.plus
                num += 1
            constraint += LpSolve.ltOrEqual
            constraint += str(sizeOfExclusiveSet - 1)
            constraint += LpSolve.semiColon
            constraint += getNewLine(2)
            self._constraints.append(constraint)
    
    def __createCapacityConstraints (self, treev, lnt):
        comment = LpSolve.getComment("Capacity constraints on header %d" % treev.getHeaderID())
        self._constraints.append(comment)
        if treev.getVertexID() == lnt.getRootID():
            constraint = LpSolve.getVertexVariable(treev.getHeaderID(), treev.getHeaderID())
            constraint += LpSolve.equals
            constraint += "1"
            constraint += LpSolve.semiColon
            constraint += getNewLine(2)
            self._constraints.append(constraint)
        else:
            for ancestorv in lnt.getAllProperAncestors(treev.getVertexID()):
                if ancestorv.getVertexID() == treev.getParentID():
                    constraint = LpSolve.getVertexVariable(treev.getHeaderID(), treev.getHeaderID())
                    constraint += LpSolve.equals
                    constraint += "%d " % (ancestorv.getLevel() * 10 + 1)
                    constraint += LpSolve.getVertexVariable(treev.getHeaderID(), ancestorv.getHeaderID())
                    constraint += LpSolve.semiColon
                    constraint += getNewLine(2)
                    self._constraints.append(constraint)
                else:
                    pass
         
    def __createFlowConstraints (self, headerID, supergRegion):
        for superv in supergRegion:
            for basicBlockID in superv.getBasicBlockIDs():
                self._variables.add(LpSolve.getVertexVariable(basicBlockID, headerID))            
            # Handle super blocks with more than one basic block
            if superv.numberOfBasicBlocks() > 1:
                repID = superv.getRepresentativeID()
                comment = LpSolve.getComment("Intra-super block constraints for %d. Representative ID = %d" % (superv.getVertexID(), repID))
                self._constraints.append(comment)
                for basicBlockID in superv.getBasicBlockIDs():
                    if repID != basicBlockID:
                        constraint = LpSolve.getVertexVariable(basicBlockID, headerID)
                        constraint += LpSolve.equals
                        constraint += LpSolve.getVertexVariable(repID, headerID)
                        constraint += LpSolve.semiColon
                        constraint += getNewLine()
                        self._constraints.append(constraint)
                self._constraints.append(getNewLine())
                        
            # Handle super blocks with more than one successor
            if superv.numberOfSuccessors() > 1:
                self.__createBranchConstraints(supergRegion, superv, headerID)
            # Handle super blocks with more than one predecessor
            if superv.numberOfPredecessors() > 1:
                self.__createMergeConstraints(supergRegion, superv, headerID)
    
    def __createBranchConstraints (self, superg, superv, headerID):
        repID   = superv.getRepresentativeID()
        comment = LpSolve.getComment("Branch super block constraints for %d. Representative ID = %d" % (superv.getVertexID(), repID))
        self._constraints.append(comment)
        for branchID, superedges in superv.getBranchPartitions().iteritems():
            comment = LpSolve.getComment("Basic block branch = %d" % branchID)
            self._constraints.append(comment)
            # The LHS of the constraint
            constraint = LpSolve.getVertexVariable(repID, headerID)
            constraint += LpSolve.equals
            # The RHS of the constraint
            num = 1
            for supere in superedges:
                succSuperv = superg.getVertex(supere.getVertexID())
                if succSuperv.isUnstructuredMerge():
                    dummyvar = LpSolve.getDummyVariable(supere.getEdgeID(), headerID)
                    self._variables.add(dummyvar)    
                    constraint += dummyvar
                elif succSuperv.numberOfBasicBlocks() == 0:
                    edges = succSuperv.getEdges()
                    assert len(edges) == 1
                    edge = list(edges)[0]
                    dummyvar = LpSolve.getEdgeVariable(edge[0], edge[1], headerID)
                    self._variables.add(dummyvar)    
                    constraint += dummyvar
                else:
                    succRepID = succSuperv.getRepresentativeID()
                    constraint += LpSolve.getVertexVariable(succRepID, headerID)
                if num < len(superedges):
                    constraint += LpSolve.plus
                num += 1
            constraint += LpSolve.semiColon 
            constraint += getNewLine(2)
            self._constraints.append(constraint)
    
    def __createMergeConstraints (self, superg, superv, headerID):
        repID   = superv.getRepresentativeID()
        comment = LpSolve.getComment("Merge super block constraints for %d. Representative ID = %d" % (superv.getVertexID(), repID))
        self._constraints.append(comment)
        # The LHS of the constraint
        constraint = LpSolve.getVertexVariable(repID, headerID)
        constraint += LpSolve.equals
        # The RHS of the constraint
        num = 1
        for supere in superv.getPredecessorEdges():
            predSuperv = superg.getVertex(supere.getVertexID())
            if predSuperv.numberOfSuccessors() > 1:
                dummyvar = LpSolve.getDummyVariable(supere.getEdgeID(), headerID)
                self._variables.add(dummyvar)    
                constraint += dummyvar
            else:
                predRepID = predSuperv.getRepresentativeID()
                constraint += LpSolve.getVertexVariable(predRepID, headerID)
            if num < superv.numberOfPredecessors():
                constraint += LpSolve.plus
            num += 1
        constraint += LpSolve.semiColon 
        constraint += getNewLine(2)
        self._constraints.append(constraint)
       
    def __createObjectiveFunction (self, lnt):
        constraint = LpSolve.max_
        num        = 1
        for var in self._variables:
            if var.startswith(LpSolve.vertexPrefix):
                lIndex       = var.find('_')
                rIndex       = var.rfind('_')
                basicBlockID = int(var[lIndex+1:rIndex])
                if lnt.isLoopHeader(basicBlockID):
                    headerID = int(var[rIndex+1:])       
                    if not lnt.isDoWhileLoop(basicBlockID):
                        constraint += "%d %s" % (basicBlockID, var)
                    elif basicBlockID == headerID:
                        constraint += "%d %s" % (basicBlockID, var)
                    else:
                        constraint += "%d %s" % (0, var)
                else:
                    constraint += "%d %s" % (basicBlockID, var)
            else:
                constraint += "%d %s" % (0, var)
            if num < len(self._variables):
                constraint += LpSolve.plus
            if num % 10 == 0:
                constraint += getNewLine()
            num += 1
        constraint += LpSolve.semiColon 
        constraint += getNewLine(2)
        self._constraints.insert(0, constraint)
        self.__endOfObjectiveFunction = 3            
    
    def __createIntegerConstraint (self):
        constraint = LpSolve.int_ + " "
        num        = 1
        for var in self._variables:
            constraint += var
            if num < len(self._variables):
                constraint += LpSolve.comma
            num += 1
        constraint += LpSolve.semiColon + getNewLine()
        self._constraints.append(constraint)

class CreateCFGILP (ILP):
    def __init__ (self, basepath, basename, data, contextWCETs, contextv, cfg, lnt):
        start = timeit.default_timer()
        ILP.__init__(self)
        self.__createStructuralConstraints(cfg)
        self.__createExecutionCountConstraints(data, cfg, lnt)
        self.__createIntegerConstraint()
        self.__createObjectiveFunction(data, contextWCETs, contextv, cfg)
        self._filename = "%s.%s.context%s.%s.%s" % (basepath + os.sep + basename, contextv.getName(), contextv.getVertexID(), "cfg", LpSolve.fileSuffix)
        with open(self._filename, 'w') as ilpFile:
            for constraint in self._constraints:
                ilpFile.write(constraint)
        self.constructionTime = (timeit.default_timer() - start)
            
    def __createStructuralConstraints (self, cfg):
        for v in cfg:
            vertexID = v.getVertexID()
            comment  = LpSolve.getComment("Basic block = %d" % vertexID)
            self._constraints.append(comment)
            self._variables.add(LpSolve.getVertexVariable(vertexID))
            constraint1 = LpSolve.getVertexVariable(vertexID)
            constraint1 += LpSolve.equals
            num = 1
            for succe in v.getSuccessorEdges():
                succID = succe.getVertexID() 
                self._variables.add(LpSolve.getEdgeVariable(vertexID, succID))
                constraint1 += LpSolve.getEdgeVariable(vertexID, succID)
                if num < v.numberOfSuccessors():
                    constraint1 += LpSolve.plus
                num += 1
            constraint1 += LpSolve.semiColon
            constraint1 += getNewLine() 
            self._constraints.append(constraint1)
            
            constraint2 = ""
            num = 1
            for succe in v.getSuccessorEdges():
                succID = succe.getVertexID() 
                constraint2 += LpSolve.getEdgeVariable(vertexID, succID)
                if num < v.numberOfSuccessors():
                    constraint2 += LpSolve.plus
                num += 1
            constraint2 += LpSolve.equals
            num = 1
            for prede in v.getPredecessorEdges():
                predID = prede.getVertexID() 
                constraint2 += LpSolve.getEdgeVariable(predID, vertexID)
                if num < v.numberOfPredecessors():
                    constraint2 += LpSolve.plus
                num += 1
            constraint2 += LpSolve.semiColon
            constraint2 += getNewLine(2) 
            self._constraints.append(constraint2)
    
    def __createExecutionCountConstraints (self, data, cfg, lnt):
        for level, vertices in lnt.levelIterator(True):
            for lntv in vertices:
                if isinstance(lntv, HeaderVertex):
                    headerID = lntv.getHeaderID()
                    comment  = LpSolve.getComment("Capacity constraints on header %d" % headerID)
                    self._constraints.append(comment)
                    if lnt.getRootID() != lntv.getVertexID():
                        # Relative bound
                        forwardPredIDs = set([])
                        for predID in cfg.getVertex(headerID).getPredecessorIDs():
                            if not lnt.isLoopBackEdge(predID, headerID):
                                forwardPredIDs.add(predID)
                        constraint = ""
                        constraint += LpSolve.getVertexVariable(headerID)
                        constraint += LpSolve.ltOrEqual
                        count = 1
                        for predID in forwardPredIDs:
                            constraint += str(data.getRelativeBound(cfg.getName(), headerID))
                            constraint += LpSolve.getEdgeVariable(predID, headerID)
                            if count < len(forwardPredIDs):
                                constraint += LpSolve.plus
                            count += 1
                        constraint += LpSolve.semiColon
                        constraint += getNewLine()
                        self._constraints.append(constraint)
                        if lnt.isDoWhileLoop(headerID):
                            bound      = data.getBound(cfg.getName(), headerID)
                            constraint = LpSolve.getVertexVariable(headerID)
                            constraint += LpSolve.ltOrEqual
                            constraint += str(bound)
                            constraint += LpSolve.semiColon
                            constraint += getNewLine(2)
                            self._constraints.append(constraint)
                        else:                                
                            # Bound on back-edge executions
                            headerInvocations = data.getBound(cfg.getName(), headerID)
                            freshInvocations  = data.getFreshInvocations(cfg.getName(), headerID)
                            constraint = ""
                            count = 1
                            tails = lnt.getLoopTails(headerID)
                            for tailID in tails:
                                constraint += LpSolve.getEdgeVariable(tailID, headerID)
                                if count < len(tails):
                                    constraint += LpSolve.plus 
                            constraint += LpSolve.ltOrEqual
                            constraint += str(headerInvocations - freshInvocations)
                            constraint += LpSolve.semiColon
                            constraint += getNewLine()
                            self._constraints.append(constraint)
                            # Bound on exit-edge executions
                            constraint = ""
                            count = 1
                            exits = lnt.getLoopExits(headerID)
                            for predID, succID in exits:
                                constraint += LpSolve.getEdgeVariable(predID, succID)
                                if count < len(exits):
                                    constraint += LpSolve.plus 
                            constraint += LpSolve.ltOrEqual
                            constraint += str(freshInvocations)
                            constraint += LpSolve.semiColon
                            constraint += getNewLine(2)
                            self._constraints.append(constraint)                            
                    else:
                        bound      = data.getBound(cfg.getName(), headerID)
                        constraint = LpSolve.getVertexVariable(headerID)
                        constraint += LpSolve.equals
                        constraint += str(bound)
                        constraint += LpSolve.semiColon
                        constraint += getNewLine(2)
                        self._constraints.append(constraint)
                            
    def __createObjectiveFunction (self, data, contextWCETs, contextv, cfg):
        constraint = LpSolve.max_
        num        = 1
        for var in self._variables:
            if var.startswith(LpSolve.vertexPrefix):
                lIndex       = var.find('_')
                basicBlockID = int(var[lIndex+1:])
                wcet         = data.getExecutionTime(cfg.getName(), basicBlockID)
                if cfg.isCallSite(basicBlockID):
                    calleeContextID   = contextv.getSuccessorWithCallSite(basicBlockID)
                    calleeContextWCET = contextWCETs[calleeContextID]
                    wcet += calleeContextWCET
                constraint += "%d %s" % (wcet, var)
            else:
                constraint += "%d %s" % (0, var)
            if num < len(self._variables):
                constraint += LpSolve.plus
            if num % 10 == 0:
                constraint += getNewLine()
            num += 1
        constraint += LpSolve.semiColon 
        constraint += getNewLine(2)
        self._constraints.insert(0, constraint) 
    
    def __createIntegerConstraint (self):
        constraint = LpSolve.int_ + " "
        num        = 1
        for var in self._variables:
            constraint += var
            if num < len(self._variables):
                constraint += LpSolve.comma
            num += 1
        constraint += LpSolve.semiColon + getNewLine()
        self._constraints.append(constraint)

class InstructionWCETDatabase:
    ARM = {}
    ARM['nop']   = 1
    ARM['ldr']   = 20
    ARM['str']   = 15
    ARM['lsl']   = 2
    ARM['lsr']   = 2
    ARM['orr']   = 2
    ARM['mov']   = 2
    ARM['add']   = 2
    ARM['sub']   = 2
    ARM['cmp']   = 2
    ARM['b']     = 4
    ARM['push']  = 5
    ARM['pop']   = 5
    ARM['stmdb'] = 10
