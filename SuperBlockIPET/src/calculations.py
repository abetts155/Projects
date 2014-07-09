from Vertices import HeaderVertex
from Trees import DepthFirstSearch
import Debug
import os, timeit

class WCETCalculation:
    def __init__ (self, program, data, basepath, basename, repeatability):
        ILPSolvingTime_CFG        = 0
        ILPConstructionTime_CFG   = 0
        ILPSolvingTime_SBCFG      = 0
        ILPConstructionTime_SBCFG = 0
        for i in xrange(0, repeatability):
            data.assignRandomWCETs()
            self.__contextData_CFG   = {}
            self.__contextData_SBCFG = {}
            contextg = program.getContextGraph()
            dfs      = DepthFirstSearch(contextg, contextg.getRootID())
            for vertexID in dfs.getPostorder():
                contextv     = contextg.getVertex(vertexID)
                functionName = contextv.getName()
                Debug.verboseMessage("Doing WCET calculation on %s" % functionName)
                cfg         = program.getCFG(functionName)
                lnt         = program.getLNT(functionName)
                # The calculation based on the CFG
                ilp         = CreateCFGILP(basepath, basename, data, self.__contextData_CFG, contextv, cfg, lnt)
                ilpWCET     = ilp.solve()
                Debug.verboseMessage("ILP-CFG::    WCET(%s)=%d (SOLVE TIME=%.5f) (CONSTRUCTION TIME=%.5f) (TOTAL TIME=%.5f)" % (functionName, 
                                                                                                        ilpWCET, 
                                                                                                        ilp.solvingTime, 
                                                                                                        ilp.constructionTime,
                                                                                                        ilp.solvingTime + ilp.constructionTime))
                ILPSolvingTime_CFG += ilp.solvingTime
                ILPConstructionTime_CFG += ilp.constructionTime
                self.__contextData_CFG[contextv.getVertexID()] = ilpWCET
                # The calculation based on the super block CFG
                ilp2        = CreateSuperBlockCFGILP(basepath, basename, data, self.__contextData_SBCFG, contextv, cfg, lnt, program.getSuperBlockCFG(functionName))
                ilpWCET2    = ilp2.solve()
                Debug.verboseMessage("ILP-SBCFG::  WCET(%s)=%d (SOLVE TIME=%.5f) (CONSTRUCTION TIME=%.5f) (TOTAL TIME=%.5f)" % (functionName, 
                                                                                                        ilpWCET2, 
                                                                                                        ilp2.solvingTime, 
                                                                                                        ilp2.constructionTime,
                                                                                                        ilp2.solvingTime + ilp2.constructionTime))
                ILPSolvingTime_SBCFG += ilp2.solvingTime
                ILPConstructionTime_SBCFG += ilp2.constructionTime
                self.__contextData_SBCFG[contextv.getVertexID()] = ilpWCET2
                assert ilpWCET == ilpWCET2
                if i == 0:
                    Debug.verboseMessage("CFG size::       vertices=%d edges=%d" % (cfg.numOfVertices(), cfg.numOfEdges()))
                    Debug.verboseMessage("ILP-CFG size::   constraints=%d variables=%d" % (ilp.numOfConstraints(), ilp.numOfVariables()))
                    Debug.verboseMessage("ILP-SBCFG size:: constraints=%d variables=%d" % (ilp2.numOfConstraints(), ilp2.numOfVariables()))
        Debug.verboseMessage("ILP-CFG::    (OVERALL SOLVE TIME=%.5f) (OVERALL CONSTRUCTION TIME=%.5f) (OVERALL TIME=%.5f)" % 
                             (ILPSolvingTime_CFG/repeatability, 
                              ILPConstructionTime_CFG/repeatability,
                              (ILPSolvingTime_CFG+ILPConstructionTime_CFG)/repeatability))
        Debug.verboseMessage("ILP-SBCFG::  (OVERALL SOLVE TIME=%.5f) (OVERALL CONSTRUCTION TIME=%.5f) (OVERALL TIME=%.5f)" % 
                             (ILPSolvingTime_SBCFG/repeatability, 
                              ILPConstructionTime_SBCFG/repeatability,
                              (ILPSolvingTime_SBCFG+ILPConstructionTime_SBCFG)/repeatability))

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
        self._lines = []
        self._variables = set([])
        self._filename = None
        
    def numOfConstraints (self):
        nonconstraints = 0
        for line in self._lines:
            if line.startswith("max") or line.startswith("int") or line.startswith("\n") or line.startswith("//"):
                nonconstraints += 1
        return len(self._lines) - nonconstraints
    
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
    def __init__ (self, basepath, basename, data, contextWCETs, contextv, cfg, lnt, superg):
        start = timeit.default_timer()
        ILP.__init__(self)
        self.__createConstraints(data, lnt, superg)
        self.__createObjectiveFunction(data, contextWCETs, contextv, cfg)
        self.__createIntegerConstraint()
        self._filename = "%s.%s.context%s.%s.%s" % (basepath + os.sep + basename, contextv.getName(), contextv.getVertexID(), "superg", LpSolve.fileSuffix)
        with open(self._filename, 'w') as ilpFile:
            for line in self._lines:
                ilpFile.write(line)
        self.constructionTime = (timeit.default_timer() - start)
        
    def __createConstraints(self, data, lnt, superg):
        for level, vertices in lnt.levelIterator(True):
            for treev in vertices:
                if isinstance(treev, HeaderVertex):
                    headerID = treev.getHeaderID()
                    Debug.debugMessage("Analysing header %d" % headerID, 1)
                    supergRegion = superg.getSubregion(headerID)
                    self.__createCapacityConstraints(data, lnt, treev)
                    self.__createFlowConstraints(lnt, headerID, supergRegion)
                    for superv in supergRegion:
                        for bbID in superv.basicBlocks:
                            self._variables.add(LpSolve.getVertexVariable(bbID))
    
    def __createCapacityConstraints (self, data, lnt, treev):
        headerID = treev.getHeaderID()
        comment = LpSolve.getComment("Capacity constraints on header %d" % headerID)
        self._lines.append(comment)
        if treev.getVertexID() == lnt.getRootID():
            constraint = LpSolve.getVertexVariable(headerID)
            constraint += LpSolve.equals
            constraint += "1"
            constraint += LpSolve.semiColon
            constraint += getNewLine(2)
            self._lines.append(constraint)
        else:
            bound      = data.getRelativeBound(lnt.getName(), headerID)
            exitEdges  = lnt.getLoopExits(headerID)
            constraint = LpSolve.getVertexVariable(headerID)
            constraint += LpSolve.ltOrEqual
            num = 1
            for edge in exitEdges:
                constraint += "%d %s" % (bound, LpSolve.getEdgeVariable(edge[0], edge[1]))
                if num < len(exitEdges):
                    constraint += LpSolve.plus
                num += 1
            constraint += LpSolve.semiColon
            constraint += getNewLine(2)
            self._lines.append(constraint)
         
    def __createFlowConstraints (self, lnt, headerID, supergRegion):
        for superv in supergRegion:
            if superv.basicBlocks:        
                # Handle super blocks with more than one basic block
                if len(superv.basicBlocks) > 1: 
                    comment = LpSolve.getComment("Basic block constraints within super block %d" % superv.getVertexID())
                    self._lines.append(comment)
                    assert superv.repBasicBlock
                    for bbID in superv.basicBlocks:
                        if superv.repBasicBlock != bbID:
                            var = LpSolve.getVertexVariable(bbID)
                            self._variables.add(var)
                            constraint  = var
                            constraint += LpSolve.equals
                            constraint += LpSolve.getVertexVariable(superv.repBasicBlock)
                            constraint += LpSolve.semiColon
                            constraint += getNewLine()
                            self._lines.append(constraint)
                    self._lines.append(getNewLine())
            if superv.loopExitEdges and not superv.loopExitEdges.issubset(lnt.getLoopExits(headerID)) and (superv.repBasicBlock or superv.repEdge):
                comment = LpSolve.getComment("Loop-exit edges constraint within super block %d" % superv.getVertexID())
                self._lines.append(comment)
                for loopExitEdge in superv.loopExitEdges:
                    edgeVariable = LpSolve.getEdgeVariable(loopExitEdge[0], loopExitEdge[1])
                    constraint   = edgeVariable
                    constraint  += LpSolve.equals
                    if superv.basicBlocks:
                        constraint += LpSolve.getVertexVariable(superv.repBasicBlock)
                    else:
                        assert superv.repEdge
                        constraint += LpSolve.getEdgeVariable(superv.repEdge[0], superv.repEdge[1])
                    constraint += LpSolve.semiColon
                    constraint += getNewLine()
                    self._lines.append(constraint)
                
            # Handle super blocks with more than one successor
            if superv.numberOfSuccessors() > 1:
                self.__createBranchConstraints(lnt, headerID, supergRegion, superv)
            # Handle super blocks with more than one predecessor
            if superv.numberOfPredecessors() > 1:
                self.__createMergeConstraints(lnt, headerID, supergRegion, superv)
    
    def __createBranchConstraints (self, lnt, headerID, supergRegion, superv):
        comment = LpSolve.getComment("Branch constraints for super block %d" % superv.getVertexID())
        self._lines.append(comment)
        for branchID, superedges in superv.getBranchPartitions().iteritems():
            comment = LpSolve.getComment("Basic block branch = %d" % branchID)
            self._lines.append(comment)
            # The LHS of the constraint
            assert superv.repBasicBlock
            constraint = LpSolve.getVertexVariable(superv.repBasicBlock)
            constraint += LpSolve.equals
            # The RHS of the constraint
            num = 1
            for supere in superedges:
                succSuperv = supergRegion.getVertex(supere.getVertexID())
                constraint += self.__getVariableForSuperBlock(succSuperv)
                if num < len(superedges):
                    constraint += LpSolve.plus
                num += 1
            constraint += LpSolve.semiColon 
            constraint += getNewLine(2)
            self._lines.append(constraint)
    
    def __createMergeConstraints (self, lnt, headerID, supergRegion, superv):
        comment = LpSolve.getComment("Merge constraints for super block %d" % superv.getVertexID())
        self._lines.append(comment)
        # The LHS of the constraint
        constraint = self.__getVariableForSuperBlock(superv)
        constraint += LpSolve.equals
        # The RHS of the constraint
        num = 1
        for supere in superv.getPredecessorEdges():
            predSuperv = supergRegion.getVertex(supere.getVertexID())
            constraint += self.__getVariableForSuperBlock(predSuperv)
            if num < superv.numberOfPredecessors():
                constraint += LpSolve.plus
            num += 1
        constraint += LpSolve.semiColon 
        constraint += getNewLine(2)
        self._lines.append(constraint)
        
    def __getVariableForSuperBlock (self, superv):
        if superv.basicBlocks:
            assert superv.repBasicBlock
            return LpSolve.getVertexVariable(superv.repBasicBlock)
        elif superv.edges:
            assert superv.repEdge
            var = LpSolve.getEdgeVariable(superv.repEdge[0], superv.repEdge[1])
            self._variables.add(var)
            return var
        else:
            print superv
            assert False
            var = LpSolve.getEdgeVariable(superv.loopExitEdge[0], superv.loopExitEdge[1])
            self._variables.add(var)
            return var
       
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
        self._lines.insert(0, constraint)
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
        self._lines.append(constraint)

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
            for line in self._lines:
                ilpFile.write(line)
        self.constructionTime = (timeit.default_timer() - start)
            
    def __createStructuralConstraints (self, cfg):
        for v in cfg:
            vertexID = v.getVertexID()
            comment  = LpSolve.getComment("Basic block = %d" % vertexID)
            self._lines.append(comment)
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
            self._lines.append(constraint1)
            
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
            self._lines.append(constraint2)
    
    def __createExecutionCountConstraints (self, data, cfg, lnt):
        for level, vertices in lnt.levelIterator(True):
            for lntv in vertices:
                if isinstance(lntv, HeaderVertex):
                    headerID = lntv.getHeaderID()
                    comment  = LpSolve.getComment("Capacity constraints on header %d" % headerID)
                    self._lines.append(comment)
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
                            constraint += "%d %s" %  (data.getRelativeBound(cfg.getName(), headerID), LpSolve.getEdgeVariable(predID, headerID))
                            if count < len(forwardPredIDs):
                                constraint += LpSolve.plus
                            count += 1
                        constraint += LpSolve.semiColon
                        constraint += getNewLine(2)
                        self._lines.append(constraint)                            
                    else:
                        bound      = data.getBound(cfg.getName(), headerID)
                        constraint = LpSolve.getVertexVariable(headerID)
                        constraint += LpSolve.equals
                        constraint += str(bound)
                        constraint += LpSolve.semiColon
                        constraint += getNewLine(2)
                        self._lines.append(constraint)
                            
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
        self._lines.insert(0, constraint)
    
    def __createIntegerConstraint (self):
        constraint = LpSolve.int_ + " "
        num        = 1
        for var in self._variables:
            constraint += var
            if num < len(self._variables):
                constraint += LpSolve.comma
            num += 1
        constraint += LpSolve.semiColon + getNewLine()
        self._lines.append(constraint)
