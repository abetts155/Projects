from Vertices import HeaderVertex
from Edges import PathInformationEdge, PathInformationEdgeType
from Trees import DepthFirstSearch
import Debug
import os, timeit, sys

def getNewLine (num=1):
    return "\n" * num 

class WCETCalculation:
    def __init__ (self, program, data, basepath, basename):
        self.__VanillaContextIDToWCET = {}
        self.__ExtraContextIDToWCET = {}
        contextg = program.getContextGraph()
        dfs      = DepthFirstSearch(contextg, contextg.getRootID())
        for vertexID in dfs.getPostorder():
            contextv     = contextg.getVertex(vertexID)
            functionName = contextv.getName()
            if data.isExecutedFunction(functionName):
                Debug.verboseMessage("Doing WCET calculation on %s" % functionName)
                lnt   = program.getLNT(functionName)
                pathg = program.getPathInfoGraph(functionName)
                cfg   = program.getCFG(functionName)
                ilp1  = CreateCFGILPVanilla(basepath, basename, data, self.__VanillaContextIDToWCET, contextv, cfg, lnt)
                ilp1WCET, ilp1SolvingTime = ilp1.solve()
                Debug.verboseMessage("ILP(vanilla):: WCET(%s)=%s (SOLVE TIME=%.5f)" % (functionName, ilp1WCET, ilp1SolvingTime))
                self.__VanillaContextIDToWCET[contextv.getVertexID()] = ilp1WCET
                if not pathg.executionDependencies() and not pathg.mutualInclusionPairs() and not pathg.mutualExclusionPairs():
                    ilp2 = CreateCFGILPExtra(basepath, basename, data, self.__ExtraContextIDToWCET, contextv, cfg, lnt, pathg)
                    ilp2WCET, ilp2SolvingTime = ilp2.solve()
                    self.__ExtraContextIDToWCET[contextv.getVertexID()] = ilp2WCET
                    Debug.verboseMessage("ILP(extra):: WCET(%s)=%s (SOLVE TIME=%.5f)" % (functionName, ilp2WCET, ilp2SolvingTime))
                else:
                    clp2 = CreateCFGCLPExtra(basepath, basename, data, self.__ExtraContextIDToWCET, contextv, cfg, lnt, pathg)
                    clp2WCET, clp2SolvingTime = clp2.solve()
                    self.__ExtraContextIDToWCET[contextv.getVertexID()] = clp2WCET
                    Debug.verboseMessage("CLP(extra):: WCET(%s)=%s (SOLVE TIME=%.5f)" % (functionName, clp2WCET, clp2SolvingTime))
            else:
                Debug.verboseMessage("%s did not execute" % functionName)
                self.__VanillaContextIDToWCET[contextv.getVertexID()] = 0
                self.__ExtraContextIDToWCET[contextv.getVertexID()] = 0
            
class ECLIPSE:
    fileExtensions  = ['.res', '.vanilla', '.extra']
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
        self._filename = None
        self._wcet = 0
        self._solvingTime = 0
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
        
    def solve (self):
        from subprocess import Popen
        import re
        with open(self._filename, 'w') as clpFile:
            for line in self._lines:
                clpFile.write(line)          
        Debug.debugMessage("Solving CLP in %s" % self._filename, 10)
        command    = 'jeclipse -b %s -e "%s."' % (self._filename, self.__goal) 
        Debug.verboseMessage("Running command '%s'" % command)
        start = timeit.default_timer()
        proc       = Popen(command, shell=True, executable="/bin/bash")
        returnCode = proc.wait()
        self._solvingTime = (timeit.default_timer() - start)
        if returnCode != 0:
            Debug.warningMessage("Running '%s' failed" % command)
            return self._wcet, self._solvingTime
        else:
            with open(self._filename + '.res') as f:
                for line in f:
                    if re.match(r'%s' % CLP.WCET, line):
                        values = re.findall(r'[0-9]+', line)
                        assert len(values) == 1, "Unable to find WCET value in CLP output file"
                        self._wcet = int(values[0])
        assert self._wcet
        assert self._solvingTime
        return self._wcet, self._solvingTime
    
    def _addOutputPredicates (self):
        fileHandle = "F"        
        self._lines.append('%s(%s,%s,%s) %s' % (CLP.OUTPUT_PREDICATE_HEAD, CLP.BB_COUNTS, CLP.EDGE_COUNTS, CLP.WCET, ECLIPSE.clauseSep))
        self._lines.append(getNewLine())
        self._lines.append('open("%s",%s,%s)%s' % (os.path.basename(self._filename) + ".res", "write", fileHandle, ECLIPSE.conjunct))
        self._lines.append('print_list(%s,"%s: ",%s)%s' % (fileHandle, CLP.BB_COUNTS, CLP.BB_COUNTS, ECLIPSE.conjunct))
        self._lines.append('print_list(%s,"%s: ",%s)%s' % (fileHandle, CLP.EDGE_COUNTS, CLP.EDGE_COUNTS, ECLIPSE.conjunct))
        self._lines.append('write(%s, "%s: ")%s' % (fileHandle, CLP.WCET, ECLIPSE.conjunct))
        self._lines.append('write(%s,%s)%s' % (fileHandle, CLP.WCET, ECLIPSE.conjunct))
        self._lines.append('nl(%s)%s' % (fileHandle, ECLIPSE.conjunct))
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
                        v = cfg.getVertex(headerID)
                        forwardPredIDs = []
                        for prede in v.getPredecessorEdges():
                            if not lnt.isLoopBackEdge(prede.getVertexID(), headerID):
                                forwardPredIDs.append((prede.getVertexID(), headerID))
                        parentv = lnt.getVertex(treev.getParentID())
                        bound   = data.getLoopBound(cfg.getName(), headerID, parentv.getHeaderID())
                        rhs   = ""
                        count = 1
                        for edge in forwardPredIDs:
                            rhs += "%d%s%s" % (bound, ECLIPSE.multiply, ECLIPSE.getEdgeCountVariable(edge[0], edge[1]))
                            if count < len(forwardPredIDs):
                                rhs += ECLIPSE.plus
                            count += 1
                        self._lines.append("%s%s%s%s" % (ECLIPSE.getVertexCountVariable(headerID), ECLIPSE.ltOrEqual, rhs, ECLIPSE.conjunct))
        self._lines.append(getNewLine()) 
    
    def _addExecutionCountDomains (self, data, pathg, cfg, lnt, addPathInformation=False):
        self._lines.append(ECLIPSE.getComment("Execution count domains"))
        rootv = lnt.getVertex(lnt.getRootID())
        # First add domains for basic blocks
        for v in cfg:            
            vertexID = v.getVertexID()
            treev    = lnt.getVertex(vertexID)
            headerv  = lnt.getVertex(treev.getParentID())
            headerID = headerv.getHeaderID()
            if headerID == cfg.getEntryID():
                upperBound = 1
            else:
                upperBound = data.getLoopBound(cfg.getName(), headerID, rootv.getHeaderID()) 
            line = "%s%s[%d..%d]%s" % \
            (ECLIPSE.getVertexCountVariable(vertexID), ECLIPSE.domainSep, 0, upperBound, ECLIPSE.conjunct)
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
                    upperBound = data.getLoopBound(cfg.getName(), headerv1.getHeaderID(), rootv.getHeaderID())
                else:
                    rootv   = lnt.getVertex(lnt.getRootID())
                    bound1 = data.getLoopBound(cfg.getName(), headerv1.getHeaderID(), rootv.getHeaderID())
                    bound2 = data.getLoopBound(cfg.getName(), headerv2.getHeaderID(), rootv.getHeaderID())
                    upperBound = max(bound1, bound2)
                if addPathInformation:
                    pathv = pathg.isMonitoredEdge(vertexID, succID)
                    if pathv:
                        if pathv.getUpperBound() == 0:
                            upperBound = 0
                        else:
                            lowerBound = pathv.getLowerBound()
                line = "%s%s[%d..%d]%s" % \
                (ECLIPSE.getEdgeCountVariable(vertexID, succID), ECLIPSE.domainSep, lowerBound, upperBound, ECLIPSE.conjunct)
                self._lines.append(line)
        self._lines.append(getNewLine()) 
        
    def _addEpilogue (self):
        self._lines.append("%s%s%d%s%s%s" % (CLP.PWCET, ECLIPSE.equals, -1, ECLIPSE.multiply, CLP.WCET, ECLIPSE.conjunct))
        self._lines.append("append(%s,%s,%s)%s" % (CLP.EDGE_COUNTS, CLP.BB_COUNTS, ECLIPSE.getTempList(0), ECLIPSE.conjunct))
        self._lines.append("append(%s,%s,%s)%s" % (CLP.BB_TIMES, ECLIPSE.getTempList(0), ECLIPSE.getTempList(1), ECLIPSE.conjunct))
        self._lines.append("time(bb_min(search(%s,0,input_order,indomain_max,complete,[]),%s,bb_options{}))%s" % \
                           (ECLIPSE.getTempList(1), CLP.PWCET, ECLIPSE.conjunct))
        self._lines.append("%s(%s, %s, %s)%s" % \
                           (CLP.OUTPUT_PREDICATE_HEAD, CLP.BB_COUNTS, CLP.EDGE_COUNTS, CLP.WCET, ECLIPSE.terminator))
        self._lines.append(getNewLine(2))

class CreateCFGCLPVanilla (CreateCFGCLP):
    def __init__ (self, basepath, basename, data, contextWCETs, contextv, cfg, lnt, pathg):
        CreateCFGCLP.__init__(self)
        self._filename = "%s.%s.context%s.%s.%s.vanilla" % (basepath + os.sep + basename, contextv.getName(), contextv.getVertexID(), "cfg", ECLIPSE.fileSuffix)
        self._addVariables(cfg)
        self._addObjectiveFunction(cfg)
        self._addExecutionTimeDomains(data, contextWCETs, contextv, cfg)
        self._addStructuralConstraints(cfg)
        self._addRelativeCapacityConstraints(data, cfg, lnt)
        self._addExecutionCountDomains(data, pathg, cfg, lnt)
        self._addEpilogue()
        self._addOutputPredicates()

class CreateCFGCLPExtra (CreateCFGCLP):
    def __init__ (self, basepath, basename, data, contextWCETs, contextv, cfg, lnt, pathg):
        CreateCFGCLP.__init__(self)
        self._filename = "%s.%s.context%s.%s.%s.extra" % (basepath + os.sep + basename, contextv.getName(), contextv.getVertexID(), "cfg", ECLIPSE.fileSuffix)
        self._addVariables(cfg)
        self._addObjectiveFunction(cfg)
        self._addExecutionTimeDomains(data, contextWCETs, contextv, cfg)
        self._addStructuralConstraints(cfg)
        self._addRelativeCapacityConstraints(data, cfg, lnt)
        self._addExecutionCountDomains(data, pathg, cfg, lnt, True)
        self._addInfeasiblePathConstraints(data, pathg, cfg, lnt)
        self._addEpilogue()
        self._addOutputPredicates()

    def _addInfeasiblePathConstraints (self, data, pathg, cfg, lnt):
        self._lines.append(ECLIPSE.getComment("Infeasible path constraints"))
        for v in pathg:
            if v.getUpperBound() > 0:
                edge1          = v.getEdge()
                countVariable1 = ECLIPSE.getEdgeCountVariable(edge1[0], edge1[1])
                for succID in v.getSuccessorIDs():
                    succv          = pathg.getVertex(succID)
                    succe          = v.getSuccessorEdge(succID)
                    edge2          = succv.getEdge()
                    countVariable2 = ECLIPSE.getEdgeCountVariable(edge2[0], edge2[1])
                    assert isinstance(succe, PathInformationEdge)
                    if succe.getType() == PathInformationEdgeType.INCLUSION:
                        self._lines.append("%s%s0%s%s%s0%s" % \
                                       (countVariable1, ECLIPSE.gt, ECLIPSE.implies, countVariable2, ECLIPSE.gt, ECLIPSE.conjunct))
                    elif succe.getType() == PathInformationEdgeType.EXCLUSION:
                        self._lines.append("%s%s0%s%s%s0%s" % \
                                       (countVariable1, ECLIPSE.gt, ECLIPSE.implies, countVariable2, ECLIPSE.equals, ECLIPSE.conjunct))
                    else:
                        assert False
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
        self._solvingTime = 0
        self._variableToExecutionCount = {}
        self._constraints = []
        self._variables = set([])
        self._filename = None
        
    def solve (self):
        import shlex, decimal
        from subprocess import Popen, PIPE
        assert self._filename, "ILP filename has not been set"
        Debug.debugMessage("Solving ILP for %s" % self._filename, 10)
        command    = "lp_solve %s -S1 -time" % self._filename 
        start = timeit.default_timer()
        proc       = Popen(command, shell=True, stdout=PIPE, stderr=PIPE, executable="/bin/bash")
        returnCode = proc.wait()
        self._solvingTime = (timeit.default_timer() - start)
        if returnCode != 0:
            Debug.warningMessage("Running '%s' failed" % command)
            return self._wcet, self._solvingTime
        for line in proc.stdout.readlines():
            if line.startswith("Value of objective function"):
                lexemes = shlex.split(line)
                self._wcet = long(decimal.Decimal(lexemes[-1])) 
        assert self._wcet
        assert self._solvingTime
        for line in proc.stderr.readlines():
            if line.startswith("CPU Time for Parsing"):
                lexemes = shlex.split(line)
                time    = lexemes[5][:-1]
                #self.solvingTime -= float(time)
        return self._wcet, self._solvingTime
    
class CreateCFGILP (ILP):                
    def _createStructuralConstraints (self, cfg):
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
    
    def _createExecutionCountConstraints (self, data, cfg, lnt):
        for level, vertices in lnt.levelIterator(True):
            for lntv in vertices:
                if isinstance(lntv, HeaderVertex):
                    headerID = lntv.getHeaderID()
                    comment  = LpSolve.getComment("Capacity constraints on header %d" % headerID)
                    self._constraints.append(comment)
                    if lnt.getRootID() != lntv.getVertexID():
                        parentv = lnt.getVertex(lntv.getParentID())
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
                            constraint += str(data.getLoopBound(cfg.getName(), headerID, parentv.getHeaderID()))
                            constraint += LpSolve.getEdgeVariable(predID, headerID)
                            if count < len(forwardPredIDs):
                                constraint += LpSolve.plus
                            count += 1
                        constraint += LpSolve.semiColon
                        constraint += getNewLine()
                        self._constraints.append(constraint)                            
                    else:
                        constraint = LpSolve.getVertexVariable(headerID)
                        constraint += LpSolve.equals
                        constraint += str(1)
                        constraint += LpSolve.semiColon
                        constraint += getNewLine(2)
                        self._constraints.append(constraint)
                            
    def _createObjectiveFunction (self, data, contextWCETs, contextv, cfg):
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
    
    def _createIntegerConstraint (self):
        constraint = LpSolve.int_ + " "
        num        = 1
        for var in self._variables:
            constraint += var
            if num < len(self._variables):
                constraint += LpSolve.comma
            num += 1
        constraint += LpSolve.semiColon + getNewLine()
        self._constraints.append(constraint)
        
    def _outputConstraints (self):
        with open(self._filename, 'w') as ilpFile:
            for constraint in self._constraints:
                ilpFile.write(constraint)

class CreateCFGILPVanilla (CreateCFGILP):
    def __init__ (self, basepath, basename, data, contextWCETs, contextv, cfg, lnt):
        CreateCFGILP.__init__(self)
        self._filename = "%s.%s.context%s.%s.%s.vanilla" % (basepath + os.sep + basename, contextv.getName(), contextv.getVertexID(), "cfg", LpSolve.fileSuffix)
        self._createStructuralConstraints(cfg)
        self._createExecutionCountConstraints(data, cfg, lnt)
        self._createIntegerConstraint()
        self._createObjectiveFunction(data, contextWCETs, contextv, cfg)
        self._outputConstraints()
        
class CreateCFGILPExtra (CreateCFGILP):
    def __init__ (self, basepath, basename, data, contextWCETs, contextv, cfg, lnt, pathg):
        CreateCFGILP.__init__(self)
        self._filename = "%s.%s.context%s.%s.%s.extra" % (basepath + os.sep + basename, contextv.getName(), contextv.getVertexID(), "cfg", LpSolve.fileSuffix)
        self._createStructuralConstraints(cfg)
        self._createExecutionCountConstraints(data, cfg, lnt)
        self._createLowerAndUpperBoundConstraints(data, cfg, lnt, pathg)
        self._createIntegerConstraint()
        self._createObjectiveFunction(data, contextWCETs, contextv, cfg)
        self._outputConstraints()
        
    def _createLowerAndUpperBoundConstraints (self, data, cfg, lnt, pathg):
        comment = LpSolve.getComment("Infeasible path constraints")
        self._constraints.append(comment)
        for v in pathg:
            edge        = v.getEdge()
            constraint1 = LpSolve.getEdgeVariable(edge[0], edge[1])
            constraint1 += LpSolve.gtOrEqual
            constraint1 += str(v.getLowerBound())
            constraint1 += LpSolve.semiColon;
            constraint1 += getNewLine()
            self._constraints.append(constraint1)
            if v.getUpperBound() < sys.maxint:
                constraint2 = LpSolve.getEdgeVariable(edge[0], edge[1])
                constraint2 += LpSolve.ltOrEqual
                constraint2 += str(v.getUpperBound())
                constraint2 += LpSolve.semiColon;
                constraint2 += getNewLine(2)
                self._constraints.append(constraint2)        
        
        