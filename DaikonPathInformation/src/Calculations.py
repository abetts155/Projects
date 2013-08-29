from Vertices import HeaderVertex
from Trees import DepthFirstSearch
import Debug
import os, timeit

def getNewLine (num=1):
    return "\n" * num 

class WCETCalculation:
    def __init__ (self, program, data, basepath, basename):
        self.__CLPVanillaContextIDToWCET = {}
        self.__CLPExtraContextIDToWCET = {}
        contextg = program.getContextGraph()
        dfs      = DepthFirstSearch(contextg, contextg.getRootID())
        for vertexID in dfs.getPostorder():
            contextv     = contextg.getVertex(vertexID)
            functionName = contextv.getName()
            Debug.verboseMessage("Doing WCET calculation on %s" % functionName)
            lnt          = program.getLNT(functionName)
            superg       = program.getSuperBlockCFG(functionName)
            pathg        = superg.getSuperBlockPathInformationGraph()
            cfg          = program.getICFG(functionName)
            clp1         = CreateCFGCLPVanilla(basepath, basename, data, self.__CLPVanillaContextIDToWCET, contextv, cfg, lnt, superg, pathg)
            Debug.verboseMessage("CLP(vanilla):: WCET(%s)=%s (SOLVE TIME=%.5f)" % (functionName, clp1.wcet, clp1.solvingTime))
            self.__CLPVanillaContextIDToWCET[contextv.getVertexID()] = clp1.wcet
            clp2         = CreateCFGCLPExtra(basepath, basename, data, self.__CLPExtraContextIDToWCET, contextv, cfg, lnt, superg, pathg)
            self.__CLPExtraContextIDToWCET[contextv.getVertexID()] = clp2.wcet
            Debug.verboseMessage("CLP(extra):: WCET(%s)=%s (SOLVE TIME=%.5f)" % (functionName, clp2.wcet, clp2.solvingTime))
            
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
        self.wcet  = -1
        self.solvingTime = 0
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
        from subprocess import Popen
        import re
        with open(filename, 'w') as clpFile:
            for line in self._lines:
                clpFile.write(line)                
        Debug.debugMessage("Solving CLP in %s" % filename, 10)
        command    = 'jeclipse -b %s -e "%s."' % (filename, self.__goal) 
        Debug.verboseMessage("Running command '%s'" % command)
        start = timeit.default_timer()
        proc       = Popen(command, shell=True, executable="/bin/bash")
        returnCode = proc.wait()
        self.solvingTime = (timeit.default_timer() - start)
        if returnCode != 0:
            Debug.warningMessage("Running '%s' failed" % command)
        else:
            with open(filename + '.res') as f:
                for line in f:
                    if re.match(r'%s' % CLP.WCET, line):
                        values = re.findall(r'[0-9]+', line)
                        assert len(values) == 1, "Unable to find WCET value in CLP output file"
                        self.wcet = int(values[0])
    
    def _addOutputPredicates (self, filename):
        fileHandle = "F"        
        self._lines.append('%s(%s,%s,%s) %s' % (CLP.OUTPUT_PREDICATE_HEAD, CLP.BB_COUNTS, CLP.EDGE_COUNTS, CLP.WCET, ECLIPSE.clauseSep))
        self._lines.append(getNewLine())
        self._lines.append('open("%s",%s,%s)%s' % (os.path.basename(filename) + ".res", "write", fileHandle, ECLIPSE.conjunct))
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
            for superv in superg.getSuperBlockRegion(headerID):
                if superv.getBasicBlockIDs() and superv.hasRepresentativeID():           
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
        self._lines.append("time(bb_min(search(%s,0,input_order,indomain_max,complete,[]),%s,bb_options{}))%s" % \
                           (ECLIPSE.getTempList(1), CLP.PWCET, ECLIPSE.conjunct))
        self._lines.append("%s(%s, %s, %s)%s" % \
                           (CLP.OUTPUT_PREDICATE_HEAD, CLP.BB_COUNTS, CLP.EDGE_COUNTS, CLP.WCET, ECLIPSE.terminator))
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
        self._solve(basepath, basename, contextv, filename)

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
        self._solve(basepath, basename, contextv, filename)

    def __addInfeasiblePathConstraints (self, data, pathg, cfg, lnt):
        self._lines.append(ECLIPSE.getComment("Infeasible path constraints"))
        for superv in pathg:
            for edge1 in superv.getEdges():
                countVariable1 = ECLIPSE.getEdgeCountVariable(edge1[0], edge1[1])
                for succID in superv.getSuccessorIDs():
                    succv = pathg.getVertex(succID)
                    for edge2 in succv.getEdges():
                        countVariable2 = ECLIPSE.getEdgeCountVariable(edge2[0], edge2[1])
                        self._lines.append("%s%s0%s%s%s0%s" % \
                                       (countVariable1, ECLIPSE.gt, ECLIPSE.implies, countVariable2, ECLIPSE.equals, ECLIPSE.conjunct))
                        self._lines.append("%s%s0%s%s%s0%s" % \
                                       (countVariable2, ECLIPSE.gt, ECLIPSE.implies, countVariable1, ECLIPSE.equals, ECLIPSE.conjunct))
        self._lines.append(getNewLine())

class TreeBasedCalculation:
    def __init__ (self, superg, lnt, longestPaths=1):
        self.__longestPaths  = longestPaths
        self.__supervToWCETs = {}
        for superv in superg:
            self.__supervToWCETs[superv] = set([])
        self.__doCalculation(superg, lnt)
        rootSuperv = superg.getRootSuperBlock()
        Debug.verboseMessage("TREE:: WCET(%s) = %s" % (superg.getName(), sorted(self.__supervToWCETs[rootSuperv])))
        
    def __doCalculation (self, superg, lnt):
        rootSuperv = superg.getRootSuperBlock()
        dfs        = DepthFirstSearch(superg, rootSuperv.getVertexID())
        for vertexID in dfs.getPostorder():
            superv = superg.getVertex(vertexID)
            if superv.numberOfBasicBlocks() == 0:
                self.__supervToWCETs[superv].add(0)
            else:
                intraBlockWCET = self.__calculateIntraBlockValue(superv, lnt)
                forwardPartitions = superv.getBranchPartitions()
                loopPartitions    = superv.getLoopPartition()
                if forwardPartitions:
                    self.__calculateForwardControlFlow(superg, superv, forwardPartitions, intraBlockWCET)
                if loopPartitions:
                    self.__calculateLoopControlFlow(lnt, superg, superv, loopPartitions,intraBlockWCET)
                if not forwardPartitions and not loopPartitions:
                    self.__supervToWCETs[superv] = set([intraBlockWCET])
                print "%d %s" % (superv.getVertexID(), self.__supervToWCETs[superv])
                        
    def __calculateLoopControlFlow (self, lnt, superg, superv, loopPartitions,intraBlockWCET):
        for supere in loopPartitions:
            succSuperv = superg.getVertex(supere.getVertexID())
            headerIDs  = succSuperv.getBasicBlockIDs().intersection(lnt.getHeaderIDs())
            assert len(headerIDs) == 1, "Header ID not found in super block %d" % (succSuperv.getVertexID())
            headerID = list(headerIDs)[0]
            treev    = lnt.getVertex(lnt.getVertex(headerID).getParentID())
            parentv  = lnt.getVertex(treev.getParentID())
            bound    = parentv.getLevel() * 10 + 1
            self.__supervToWCETs[superv] = set([intraBlockWCET + val * bound for val in self.__supervToWCETs[succSuperv]])
                    
    def __calculateIntraBlockValue (self, superv, lnt):
        intraBlockWCET = 0
        for basicBlockID in superv.getBasicBlockIDs():
            # Since every header except the dummy header appears in 2 super blocks we need to decide when to add
            # its contribution to the super block. 
            # In a for loop, we add it BOTH times. 
            # In a do-while loop, we only add it when the root super block obtained during construction of its loop subgraph is analysed
            if lnt.isLoopHeader(basicBlockID):
                if not lnt.isDoWhileLoop(basicBlockID):
                    intraBlockWCET += basicBlockID
                else:
                    headerID = superv.getLoopHeader()
                    if headerID and headerID == basicBlockID:
                        intraBlockWCET += basicBlockID
            else:
                intraBlockWCET += basicBlockID
        return intraBlockWCET
    
    def __calculateForwardControlFlow (self, superg, superv, forwardPartitions, intraBlockWCET):
        values = set([])
        for branchID, superedges in forwardPartitions.iteritems():
            if not values:
                for supere in superedges:
                    succSuperv = superg.getVertex(supere.getVertexID())
                    values.update(self.__supervToWCETs[succSuperv])
            else:
                newValues = set([])
                for supere in superedges:
                    succSuperv = superg.getVertex(supere.getVertexID())
                    for val1 in self.__supervToWCETs[succSuperv]:
                        for val2 in values:
                            newValues.add(val1+val2)
                values = newValues
        for val in values:
            self.__supervToWCETs[superv].add(val+intraBlockWCET)
            
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
        self.wcet = -1
        self._variableToExecutionCount = {}
        
    def _solve(self, filename):
        from subprocess import Popen, PIPE
        import shlex, decimal
        Debug.debugMessage("Solving ILP for %s" % filename, 10)
        command    = "lp_solve %s" % filename 
        proc       = Popen(command, shell=True, executable="/bin/bash", stdout=PIPE, stderr=PIPE)
        returnCode = proc.wait()
        if returnCode != 0:
            Debug.warningMessage("Running '%s' failed" % command)
            return False
        for line in proc.stdout.readlines():
            if line.startswith("Value of objective function"):
                lexemes     = shlex.split(line)
                self.wcet = long(decimal.Decimal(lexemes[-1])) 
            elif line.startswith(LpSolve.edgePrefix) or line.startswith(LpSolve.vertexPrefix):
                lexemes = shlex.split(line)
                assert len(lexemes) == 2, "Incorrectly detected variable execution count line '%s'" % line
                variable = lexemes[0]
                count    = lexemes[1]
                self._variableToExecutionCount[variable] = count
        return True
                    
class CreateSuperBlockCFGILP (ILP):
    def __init__ (self, basepath, basename, superg, lnt, soughtSolutions=1):
        ILP.__init__(self)
        self.__constraints = []
        self.__variables   = set([])
        self.__createConstraints(superg, lnt)
        self.__createObjectiveFunction(lnt)
        self.__createIntegerConstraint()
        filename = "%s.%s.%s.%s" % (basepath + os.sep + basename, superg.getName(), "superg", LpSolve.fileSuffix)
        solution = True
        while solution and soughtSolutions:
            soughtSolutions -= 1
            if self.wcet != -1:
                self.__addMaximumWCETConstraint()
            with open(filename, 'w') as ilpFile:
                for constraint in self.__constraints:
                    ilpFile.write(constraint)        
            solution = self._solve(filename)
            if solution:
                Debug.verboseMessage("IPET:: WCET(%s) = %d" % (superg.getName(), self.wcet))
            
    def __addMaximumWCETConstraint (self):
        intConstraint = self.__constraints.pop()
        constraint    = self.__constraints[0][len(LpSolve.max_):-self.__endOfObjectiveFunction]
        constraint    += LpSolve.ltOrEqual
        constraint    += str(self.wcet - 1)
        constraint    += LpSolve.semiColon
        constraint    += getNewLine()
        self.__constraints.append(constraint)
        self.__constraints.append(intConstraint)
        
    def __createConstraints(self, superg, lnt):
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, HeaderVertex):
                    headerID = v.getHeaderID()
                    Debug.debugMessage("Analysing header %d" % headerID, 1)
                    supergRegion = superg.getSuperBlockRegion(headerID)
                    self.__createCapacityConstraints(v, lnt)
                    self.__createFlowConstraints(headerID, supergRegion)
                    self.__addExclusiveConstraints(superg, headerID)
        
    def __addExclusiveConstraints (self, superg, headerID):
        for exclusiveTuple in superg.getSuperBlockPathInformationGraph().exclusiveTuples:
            comment = LpSolve.getComment("Mutual exclusive constraint")
            self.__constraints.append(comment)
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
            self.__constraints.append(constraint)
    
    def __createCapacityConstraints (self, treev, lnt):
        comment = LpSolve.getComment("Capacity constraints on header %d" % treev.getHeaderID())
        self.__constraints.append(comment)
        if treev.getVertexID() == lnt.getRootID():
            constraint = LpSolve.getVertexVariable(treev.getHeaderID(), treev.getHeaderID())
            constraint += LpSolve.equals
            constraint += "1"
            constraint += LpSolve.semiColon
            constraint += getNewLine(2)
            self.__constraints.append(constraint)
        else:
            for ancestorv in lnt.getAllProperAncestors(treev.getVertexID()):
                if ancestorv.getVertexID() == treev.getParentID():
                    constraint = LpSolve.getVertexVariable(treev.getHeaderID(), treev.getHeaderID())
                    constraint += LpSolve.equals
                    constraint += "%d " % (ancestorv.getLevel() * 10 + 1)
                    constraint += LpSolve.getVertexVariable(treev.getHeaderID(), ancestorv.getHeaderID())
                    constraint += LpSolve.semiColon
                    constraint += getNewLine(2)
                    self.__constraints.append(constraint)
                else:
                    pass
         
    def __createFlowConstraints (self, headerID, supergRegion):
        for superv in supergRegion:
            for basicBlockID in superv.getBasicBlockIDs():
                self.__variables.add(LpSolve.getVertexVariable(basicBlockID, headerID))            
            # Handle super blocks with more than one basic block
            if superv.numberOfBasicBlocks() > 1:
                repID = superv.getRepresentativeID()
                comment = LpSolve.getComment("Intra-super block constraints for %d. Representative ID = %d" % (superv.getVertexID(), repID))
                self.__constraints.append(comment)
                for basicBlockID in superv.getBasicBlockIDs():
                    if repID != basicBlockID:
                        constraint = LpSolve.getVertexVariable(basicBlockID, headerID)
                        constraint += LpSolve.equals
                        constraint += LpSolve.getVertexVariable(repID, headerID)
                        constraint += LpSolve.semiColon
                        constraint += getNewLine()
                        self.__constraints.append(constraint)
                self.__constraints.append(getNewLine())
                        
            # Handle super blocks with more than one successor
            if superv.numberOfSuccessors() > 1:
                self.__createBranchConstraints(supergRegion, superv, headerID)
            # Handle super blocks with more than one predecessor
            if superv.numberOfPredecessors() > 1:
                self.__createMergeConstraints(supergRegion, superv, headerID)
    
    def __createBranchConstraints (self, superg, superv, headerID):
        repID   = superv.getRepresentativeID()
        comment = LpSolve.getComment("Branch super block constraints for %d. Representative ID = %d" % (superv.getVertexID(), repID))
        self.__constraints.append(comment)
        for branchID, superedges in superv.getBranchPartitions().iteritems():
            comment = LpSolve.getComment("Basic block branch = %d" % branchID)
            self.__constraints.append(comment)
            # The LHS of the constraint
            constraint = LpSolve.getVertexVariable(repID, headerID)
            constraint += LpSolve.equals
            # The RHS of the constraint
            num = 1
            for supere in superedges:
                succSuperv = superg.getVertex(supere.getVertexID())
                if succSuperv.isUnstructuredMerge():
                    dummyvar = LpSolve.getDummyVariable(supere.getEdgeID(), headerID)
                    self.__variables.add(dummyvar)    
                    constraint += dummyvar
                elif succSuperv.numberOfBasicBlocks() == 0:
                    edges = succSuperv.getEdges()
                    assert len(edges) == 1
                    edge = list(edges)[0]
                    dummyvar = LpSolve.getEdgeVariable(edge[0], edge[1], headerID)
                    self.__variables.add(dummyvar)    
                    constraint += dummyvar
                else:
                    succRepID = succSuperv.getRepresentativeID()
                    constraint += LpSolve.getVertexVariable(succRepID, headerID)
                if num < len(superedges):
                    constraint += LpSolve.plus
                num += 1
            constraint += LpSolve.semiColon 
            constraint += getNewLine(2)
            self.__constraints.append(constraint)
    
    def __createMergeConstraints (self, superg, superv, headerID):
        repID   = superv.getRepresentativeID()
        comment = LpSolve.getComment("Merge super block constraints for %d. Representative ID = %d" % (superv.getVertexID(), repID))
        self.__constraints.append(comment)
        # The LHS of the constraint
        constraint = LpSolve.getVertexVariable(repID, headerID)
        constraint += LpSolve.equals
        # The RHS of the constraint
        num = 1
        for supere in superv.getPredecessorEdges():
            predSuperv = superg.getVertex(supere.getVertexID())
            if predSuperv.numberOfSuccessors() > 1:
                dummyvar = LpSolve.getDummyVariable(supere.getEdgeID(), headerID)
                self.__variables.add(dummyvar)    
                constraint += dummyvar
            else:
                predRepID = predSuperv.getRepresentativeID()
                constraint += LpSolve.getVertexVariable(predRepID, headerID)
            if num < superv.numberOfPredecessors():
                constraint += LpSolve.plus
            num += 1
        constraint += LpSolve.semiColon 
        constraint += getNewLine(2)
        self.__constraints.append(constraint)
       
    def __createObjectiveFunction (self, lnt):
        constraint = LpSolve.max_
        num        = 1
        for var in self.__variables:
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
            if num < len(self.__variables):
                constraint += LpSolve.plus
            if num % 10 == 0:
                constraint += getNewLine()
            num += 1
        constraint += LpSolve.semiColon 
        constraint += getNewLine(2)
        self.__constraints.insert(0, constraint)
        self.__endOfObjectiveFunction = 3            
    
    def __createIntegerConstraint (self):
        constraint = LpSolve.int_ + " "
        num        = 1
        for var in self.__variables:
            constraint += var
            if num < len(self.__variables):
                constraint += LpSolve.comma
            num += 1
        constraint += LpSolve.semiColon + getNewLine()
        self.__constraints.append(constraint)

class CreateCFGILP (ILP):
    def __init__ (self, basepath, basename, data, contextWCETs, contextv, cfg, lnt, pathg):
        ILP.__init__(self)
        self.__constraints = []
        self.__variables   = set([])
        self.__createStructuralConstraints(cfg)
        self.__createLoopConstraints(data, cfg, lnt)
        self.__createIntegerConstraint()
        self.__createObjectiveFunction(data, contextWCETs, contextv, cfg)
        filename = "%s.%s.context%s.%s.%s" % (basepath + os.sep + basename, contextv.getName(), contextv.getVertexID(), "cfg", LpSolve.fileSuffix)
        with open(filename, 'w') as ilpFile:
            for constraint in self.__constraints:
                ilpFile.write(constraint)        
        if self._solve(filename):
            Debug.verboseMessage("ILP:: WCET(%s) = %d" % (cfg.getName(), self.wcet))
#            
    def __createStructuralConstraints (self, cfg):
        for v in cfg:
            vertexID = v.getVertexID()
            comment  = LpSolve.getComment("Basic block = %d" % vertexID)
            self.__constraints.append(comment)
            self.__variables.add(LpSolve.getVertexVariable(vertexID))
            constraint1 = LpSolve.getVertexVariable(vertexID)
            constraint1 += LpSolve.equals
            num = 1
            for succe in v.getSuccessorEdges():
                succID = succe.getVertexID() 
                self.__variables.add(LpSolve.getEdgeVariable(vertexID, succID))
                constraint1 += LpSolve.getEdgeVariable(vertexID, succID)
                if num < v.numberOfSuccessors():
                    constraint1 += LpSolve.plus
                num += 1
            constraint1 += LpSolve.semiColon
            constraint1 += getNewLine() 
            self.__constraints.append(constraint1)
            
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
            self.__constraints.append(constraint2)
    
    def __createLoopConstraints (self, data, cfg, lnt):
        for level, vertices in lnt.levelIterator(True):
            for treev in vertices:
                if isinstance(treev, HeaderVertex):
                    headerID = treev.getHeaderID()
                    Debug.debugMessage("Analysing header %d" % headerID, 1)
                    comment = LpSolve.getComment("Capacity constraints on header %d" % treev.getHeaderID())
                    self.__constraints.append(comment)
                    if treev.getVertexID() == lnt.getRootID():
                        constraint = LpSolve.getVertexVariable(treev.getHeaderID())
                        constraint += LpSolve.equals
                        constraint += "1"
                        constraint += LpSolve.semiColon
                        constraint += getNewLine(2)
                        self.__constraints.append(constraint)
                    else:
                        headerv = cfg.getVertex(headerID)
                        for ancestorv in lnt.getAllProperAncestors(treev.getVertexID()):
                            if ancestorv.getVertexID() == treev.getParentID():
                                forwardPredIDs = []
                                for prede in headerv.getPredecessorEdges():
                                    if not lnt.isLoopBackEdge(prede.getVertexID(), headerID):
                                        forwardPredIDs.append((prede.getVertexID(), headerID))
                                constraint = LpSolve.getVertexVariable(treev.getHeaderID())
                                constraint += LpSolve.ltOrEqual
                                num = 1
                                for edge in forwardPredIDs:
                                    bound = data.getLoopBound(cfg.getName(), headerID)
                                    bound = max(bound, 1)
                                    constraint += "%d " % bound 
                                    constraint += LpSolve.getEdgeVariable(edge[0], edge[1])
                                    if num < len(forwardPredIDs):
                                        constraint += LpSolve.plus
                                    num += 1
                                constraint += LpSolve.semiColon
                                constraint += getNewLine(2)
                                self.__constraints.append(constraint)
                            else:
                                pass 
                            
    def __createObjectiveFunction (self, data, contextWCETs, contextv, cfg):
        constraint = LpSolve.max_
        num        = 1
        for var in self.__variables:
            if var.startswith(LpSolve.vertexPrefix):
                lIndex       = var.find('_')
                basicBlockID = int(var[lIndex+1:])
                v            = cfg.getVertex(basicBlockID)
                wcet         = data.getExecutionTime(cfg.getName(), v.getOriginalVertexID())
                if cfg.isCallSite(basicBlockID):
                    calleeContextID   = contextv.getSuccessorWithCallSite(basicBlockID)
                    calleeContextWCET = contextWCETs[calleeContextID]
                    wcet += calleeContextWCET
                constraint += "%d %s" % (wcet, var)
            else:
                constraint += "%d %s" % (0, var)
            if num < len(self.__variables):
                constraint += LpSolve.plus
            if num % 10 == 0:
                constraint += getNewLine()
            num += 1
        constraint += LpSolve.semiColon 
        constraint += getNewLine(2)
        self.__constraints.insert(0, constraint) 
    
    def __getWCETOfBasicBlock (self, v):
        import re
        wcet = 0
        for instr in v.getInstructions():
            match = False
            for ARM_OP, value in InstructionWCETDatabase.ARM.iteritems():
                if re.match(r'%s.*' % ARM_OP, instr.getOp()):
                    wcet += value
                    match = True
                    break
            if not match:
                Debug.warningMessage("Unable to match instruction %s against ARM instruction" % instr.getOp())
                wcet += 1
        if not wcet:
            # Always return a value of at least 1
            return 1
        return wcet
    
    def __createIntegerConstraint (self):
        constraint = LpSolve.int_ + " "
        num        = 1
        for var in self.__variables:
            constraint += var
            if num < len(self.__variables):
                constraint += LpSolve.comma
            num += 1
        constraint += LpSolve.semiColon + getNewLine()
        self.__constraints.append(constraint)

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
