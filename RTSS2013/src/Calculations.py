from Vertices import HeaderVertex
from Trees import DepthFirstSearch
import Debug
import os

def getNewLine (num=1):
    return "\n" * num 

class WCETCalculation:
    def __init__ (self, program, data, basepath, basename):
        self.__contextIDToWCET = {}
        contextg = program.getContextGraph()
        dfs      = DepthFirstSearch(contextg, contextg.getRootID())
        for vertexID in dfs.getPostorder():
            contextv     = contextg.getVertex(vertexID)
            functionName = contextv.getName()
            Debug.verboseMessage("Doing WCET calculation on %s" % functionName)
            lnt          = program.getLNT(functionName)
            pathg        = program.getSuperBlockCFG(functionName).getSuperBlockPathInformationGraph()
            cfg          = program.getICFG(functionName)
            ilp          = CreateCFGILP(basepath, basename, data, self.__contextIDToWCET, contextv, cfg, lnt, pathg)
            self.__contextIDToWCET[contextv.getVertexID()] = ilp._wcet
            CreateCFGCLP(basepath, basename, data, self.__contextIDToWCET, contextv, cfg, lnt, pathg)
            
class ECLIPSE:
    conjunct        = "," + getNewLine()
    countPrefix     = "C" 
    domainSeparator = " #:: "
    equals          = " #= "
    fileSuffix      = "ecl"
    implies         = ":-"
    multiply        = "*"
    plus            = " + "
    terminator      = "."
    wcetPrefix      = "W"
    
    @staticmethod
    def getComment (comment):
        return "% " + comment + getNewLine()
    
    @staticmethod
    def getEdgeCountVariable (sourceID, destinationID):
        return "%s%d_%s%d" % (ECLIPSE.countPrefix, sourceID, ECLIPSE.countPrefix, destinationID)
    
    @staticmethod
    def getVertexCountVariable (vertexID):
        return "%s%d" % (ECLIPSE.countPrefix, vertexID)
    
    @staticmethod
    def getVertexWCETVariable (vertexID):
        return "%s%d" % (ECLIPSE.wcetPrefix, vertexID)
    
    @staticmethod
    def getTempList (suffix):
        return "VARS%d" % suffix
    
class CLP ():
    WCET        = "WCET"
    PWCET       = "PWCET"
    BB_TIMES    = "BB_TIMES"
    BB_COUNTS   = "BB_COUNTS"
    EDGE_COUNTS = "EDGE_COUNTS"
    
    def __init__ (self):
        self._wcet  = -1
        self._lines = []
        
    def _solve(self, filename, goal):
        from subprocess import Popen, PIPE
        import shlex, decimal
        Debug.debugMessage("Solving CLP in %s" % filename, 10)
        command    = 'jeclipse -b %s -e "%s."' % (filename, goal) 
        proc       = Popen(command, shell=True, executable="/bin/bash", stdout=PIPE, stderr=PIPE)
        returnCode = proc.wait()
        if returnCode != 0:
            Debug.warningMessage("Running '%s' failed" % command)
            return False
        for line in proc.stdout.readlines():
            print line,
        return True
                    
class CreateCFGCLP (CLP):
    def __init__ (self, basepath, basename, data, contextWCETs, contextv, cfg, lnt, pathg):
        CLP.__init__(self)
        self.__addRequiredPackages()
        goal = "solve(%s,%s)" % (CLP.WCET, CLP.BB_TIMES)
        self._lines.append("%s%s%s" % (goal, ECLIPSE.implies, getNewLine()))
        self.__addVariables(cfg)
        self.__addExecutionCountDomains(cfg)
        self.__addStructuralConstraints(cfg)
        self.__addExecutionTimeDomains(data, cfg)
        self.__addObjectiveFunction(cfg)
        self.__addEpilogue()
        filename = "%s.%s.context%s.%s.%s" % (basepath + os.sep + basename, contextv.getName(), contextv.getVertexID(), "cfg", ECLIPSE.fileSuffix)
        with open(filename, 'w') as clpFile:
            for line in self._lines:
                clpFile.write(line)     
        self._solve(filename, goal)   
    
    def __addRequiredPackages (self):
        self._lines.append(ECLIPSE.getComment("Packages"))
        libs = ['ic', 'branch_and_bound', 'lists', 'util']
        for lib in libs:
            self._lines.append("%s%s(%s)%s%s" % (ECLIPSE.implies, 'lib', lib, ECLIPSE.terminator, getNewLine()))
        self._lines.append(getNewLine())
        
    def __addVariables (self, cfg):
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
    
    def __addExecutionCountDomains (self, cfg):
        self._lines.append(ECLIPSE.getComment("Execution count domains"))
        for v in cfg:
            self._lines.append("%s%s[%d..%d]%s" % \
                               (ECLIPSE.getVertexCountVariable(v.getVertexID()), ECLIPSE.domainSeparator, 0, 1, ECLIPSE.conjunct))
        for v in cfg:
            for succID in v.getSuccessorIDs():
                self._lines.append("%s%s[%d..%d]%s" % \
                                   (ECLIPSE.getEdgeCountVariable(v.getVertexID(), succID), ECLIPSE.domainSeparator, 0, 1, ECLIPSE.conjunct))
        self._lines.append(getNewLine())
    
    def __addStructuralConstraints (self, cfg):
        self._lines.append(ECLIPSE.getComment("Structural constraints"))
        for v in cfg:
            self._lines.append(ECLIPSE.getComment("Vertex %d" % v.getVertexID()))
            if v.getVertexID() == cfg.getEntryID():
                self._lines.append("%s%s1%s" % \
                                   (ECLIPSE.getVertexCountVariable(v.getVertexID()), ECLIPSE.equals, ECLIPSE.conjunct))
            else:
                # Flow out to successor edges
                rhs   = ""
                count = 1
                for succID in v.getSuccessorIDs():
                    rhs += ECLIPSE.getEdgeCountVariable(v.getVertexID(), succID)
                    if count < v.numberOfSuccessors():
                        rhs += ECLIPSE.plus
                    count += 1
                self._lines.append("%s%s%s%s" % (ECLIPSE.getVertexCountVariable(v.getVertexID()), ECLIPSE.equals, rhs, ECLIPSE.conjunct))
                # Flow in through predecessor edges
                rhs   = ""
                count = 1
                for predID in v.getPredecessorIDs():
                    rhs += ECLIPSE.getEdgeCountVariable(predID, v.getVertexID())
                    if count < v.numberOfPredecessors():
                        rhs += ECLIPSE.plus
                    count += 1
                self._lines.append("%s%s%s%s" % (ECLIPSE.getVertexCountVariable(v.getVertexID()), ECLIPSE.equals, rhs, ECLIPSE.conjunct))
        self._lines.append(getNewLine())    
    
    def __addExecutionTimeDomains (self, data, cfg):
        self._lines.append(ECLIPSE.getComment("Timing constraints"))
        # First write out WCET of each basic block
        highestWCET = 0
        for v in cfg:
            wcet = data.getExecutionTime(cfg.getName(), v.getOriginalVertexID())
            self._lines.append("%s%s%d%s" % \
                                   (ECLIPSE.getVertexWCETVariable(v.getVertexID()), ECLIPSE.equals, wcet, ECLIPSE.conjunct))
            highestWCET = max(highestWCET, wcet) 
        # Now write out the domain of the WCETs 
        for v in cfg:
            self._lines.append("%s%s[%d..%d]%s" % \
                                   (ECLIPSE.getVertexWCETVariable(v.getVertexID()), ECLIPSE.domainSeparator, 0, highestWCET, ECLIPSE.conjunct))
        self._lines.append(getNewLine())    
        
    def __addObjectiveFunction (self, cfg):
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
        
    def __addEpilogue (self):
        self._lines.append("%s%s%d%s%s%s" % (CLP.PWCET, ECLIPSE.equals, -1, ECLIPSE.multiply, CLP.WCET, ECLIPSE.conjunct))
        self._lines.append("append(%s,%s,%s)%s" % (CLP.EDGE_COUNTS, CLP.BB_COUNTS, ECLIPSE.getTempList(0), ECLIPSE.conjunct))
        self._lines.append("append(%s,%s,%s)%s" % (CLP.BB_TIMES, ECLIPSE.getTempList(0), ECLIPSE.getTempList(1), ECLIPSE.conjunct))
        self._lines.append("time(bb_min(search(%s,0,input_order,indomain_max,complete,[]),%s,bb_options{timeout:%d}))%s" % \
                           (ECLIPSE.getTempList(1), CLP.PWCET, 900, ECLIPSE.terminator))

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
        self._wcet = -1
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
                self._wcet = long(decimal.Decimal(lexemes[-1])) 
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
            if self._wcet != -1:
                self.__addMaximumWCETConstraint()
            with open(filename, 'w') as ilpFile:
                for constraint in self.__constraints:
                    ilpFile.write(constraint)        
            solution = self._solve(filename)
            if solution:
                Debug.verboseMessage("IPET:: WCET(%s) = %d" % (superg.getName(), self._wcet))
            
    def __addMaximumWCETConstraint (self):
        intConstraint = self.__constraints.pop()
        constraint    = self.__constraints[0][len(LpSolve.max_):-self.__endOfObjectiveFunction]
        constraint    += LpSolve.ltOrEqual
        constraint    += str(self._wcet - 1)
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
        filename = "%s.%s.context%s.%s.a.%s" % (basepath + os.sep + basename, contextv.getName(), contextv.getVertexID(), "cfg", LpSolve.fileSuffix)
        with open(filename, 'w') as ilpFile:
            for constraint in self.__constraints:
                ilpFile.write(constraint)        
        if self._solve(filename):
            Debug.verboseMessage("No constraints:: WCET(%s) = %d" % (cfg.getName(), self._wcet)) 
        
#        intConstraint = self.__constraints.pop()
#        self.__alwaysConstraints = set([])
#        self.__alwaysBasicBlocks = set([])
#        self.__alwaysEdges       = set([])
#        self.__addAlwaysConstraints(data, pathg)
#        self.__constraints.append(intConstraint)
#        filename = "%s.%s.context%s.%s.b.%s" % (basepath + os.sep + basename, contextv.getName(), contextv.getVertexID(), "cfg", LpSolve.fileSuffix)
#        with open(filename, 'w') as ilpFile:
#            for constraint in self.__constraints:
#                ilpFile.write(constraint)        
#        if self._solve(filename):
#            Debug.verboseMessage("Always execute constraints:: WCET(%s) = %d" % (cfg.getName(), self._wcet))
#        del self.__constraints[-(len(self.__alwaysConstraints)+1):]
#        self.__constraints.append(intConstraint)
#            
#        self.__addMutualExclusionConstraints(basepath, basename, contextv, data, pathg, lnt)
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
                            
    def __addMutualExclusionConstraints (self, basepath, basename, contextv, data, pathg, lnt):
        for superv in pathg:
                self.__mutualExclusionConstraints = []
                if superv.numberOfSuccessors() > 0 and self.__isMutualExclusionCandidate(superv, lnt, pathg, data):
                    for succID in superv.getSuccessorIDs():
                        succv = pathg.getVertex(succID)
                        if self.__isMutualExclusionCandidate(succv, lnt, pathg, data):
                            comment = LpSolve.getComment("Mutual exclusion constraint between %d and %d" % (superv.getVertexID(), succID))
                            self.__mutualExclusionConstraints.append(comment)
                            if superv.getBasicBlockIDs():
                                constraint1 = LpSolve.getVertexVariable(superv.getRepresentativeID())
                            else:
                                edge        = superv.getUniqueEdge()
                                constraint1 = LpSolve.getEdgeVariable(edge[0], edge[1])
                            constraint1 += LpSolve.gt
                            constraint1 += str(0)
                            constraint1 += LpSolve.semiColon
                            constraint1 += getNewLine()  
                            self.__mutualExclusionConstraints.append(constraint1)   
                            if succv.getBasicBlockIDs():
                                constraint2 = LpSolve.getVertexVariable(succv.getRepresentativeID())
                            else:
                                edge        = succv.getUniqueEdge()
                                constraint2 = LpSolve.getEdgeVariable(edge[0], edge[1])
                            constraint2 += LpSolve.equals
                            constraint2 += str(0)
                            constraint2 += LpSolve.semiColon
                            constraint2 += getNewLine(2)     
                            self.__mutualExclusionConstraints.append(constraint2)
                            
                if self.__mutualExclusionConstraints:
                    filename = "%s.%s.context%s.%s.%d.%s" % (basepath + os.sep + basename, contextv.getName(), contextv.getVertexID(), "cfg", superv.getVertexID(), LpSolve.fileSuffix)
                    intConstraint = self.__constraints.pop()
                    for constraint in self.__mutualExclusionConstraints:
                        self.__constraints.append(constraint)
                    self.__constraints.append(intConstraint)
                    with open(filename, 'w') as ilpFile:
                        for constraint in self.__constraints:
                            ilpFile.write(constraint)        
                    if self._solve(filename):
                        Debug.verboseMessage("Mutual exclusion constraints:: WCET(%s) = %d" % (pathg.getName(), self._wcet))  
                    del self.__constraints[-(len(self.__mutualExclusionConstraints)+1):]
                    self.__constraints.append(intConstraint)
    
    def __isMutualExclusionCandidate (self, superv, lnt, pathg, data):
        if superv.getBasicBlockIDs():
            return \
                not data.neverExecutes(pathg, superv)
        else:
            edge = superv.getUniqueEdge()
            return \
                not data.neverExecutes(pathg, superv) and \
                not lnt.isLoopBackEdge(edge[0], edge[1]) and \
                not lnt.isLoopHeader(edge[0])
    
    def __addAlwaysConstraints (self, data, pathg):
        for partitionID in range(1, pathg.partitionID+1):
            if pathg.isAcyclicPartition(partitionID):
                always = []
                for superv in pathg.partitionToSuperBlocks[partitionID]:
                    executionCount = data.getMinimumExecutionCount(pathg, superv)
                    if executionCount > 0:
                        always.append(superv)
                if len(always) > 1:
                    Debug.verboseMessage("Not adding always constraint in partition %d since super blocks {%s} can fire in this partition" % \
                                         (partitionID, ', '.join(str(superv.getVertexID()) for superv in always)))
                elif len(always) == 1:
                    uniqueSuperv = always[0]
                    self.__addAlwaysConstraint(uniqueSuperv, data.getMinimumExecutionCount(pathg, uniqueSuperv))
            else:
                for superv in pathg.partitionToSuperBlocks[partitionID]:
                    executionCount = data.getMinimumExecutionCount(pathg, superv)
                    if executionCount > 0:
                        self.__addAlwaysConstraint(superv, executionCount)
                            
    def __addAlwaysConstraint (self, superv, value):
        comment = LpSolve.getComment("Always executes constraint")
        self.__constraints.append(comment)
        if superv.getBasicBlockIDs():
            constraint = LpSolve.getVertexVariable(superv.getRepresentativeID())
            self.__alwaysBasicBlocks.add(superv.getRepresentativeID())
        else:
            edge = superv.getUniqueEdge()
            constraint = LpSolve.getEdgeVariable(edge[0], edge[1])
            self.__alwaysBasicBlocks.add(edge[0])
            self.__alwaysBasicBlocks.add(edge[1])
            self.__alwaysEdges.add(edge)
        constraint += LpSolve.gtOrEqual
        constraint += str(value)
        constraint += LpSolve.semiColon
        constraint += getNewLine(2)
        self.__constraints.append(constraint)        
                            
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
