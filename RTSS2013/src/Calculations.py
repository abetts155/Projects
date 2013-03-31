from Vertices import HeaderVertex
from Edges import SuperBlockLoopEdge
from Trees import DepthFirstSearch
import Debug
import os

class WCETCalculation:
    def __init__ (self, program, basepath, basename):
        callg = program.getCallGraph()
        dfs   = DepthFirstSearch(callg, callg.getRootID())
        for vertexID in dfs.getPostorder():
            callv  = callg.getVertex(vertexID)
            lnt    = program.getLNT(callv.getName())
            superg = program.getSuperBlockCFG(callv.getName())
            CreateSuperBlockCFGILP(basepath, basename, superg, lnt)
            TreeBasedCalculation(superg, lnt)
            
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
                intraBlockWCET = self.__calculateIntraBlockValue(superv)
                forwardPartitions = superv.getBranchPartitions()
                loopPartitions    = superv.getLoopPartition()
                if forwardPartitions:
                    self.__calculateForwardControlFlow(superg, superv, forwardPartitions, intraBlockWCET)
                if loopPartitions:
                    self.__calculateLoopControlFlow(lnt, superg, superv, loopPartitions)
                if not forwardPartitions and not loopPartitions:
                    self.__supervToWCETs[superv] = set([intraBlockWCET])
                        
    def __calculateLoopControlFlow (self, lnt, superg, superv, loopPartitions):
        for supere in loopPartitions:
            succSuperv = superg.getVertex(supere.getVertexID())
            headerIDs  = succSuperv.getBasicBlockIDs().intersection(lnt.getHeaderIDs())
            assert len(headerIDs) == 1, "Header ID not found in super block %d" % (succSuperv.getVertexID())
            headerID = list(headerIDs)[0]
            treev    = lnt.getVertex(lnt.getVertex(headerID).getParentID())
            parentv  = lnt.getVertex(treev.getParentID())
            bound    = parentv.getLevel() * 10 + 1
            self.__supervToWCETs[succSuperv] = set([val * bound for val in self.__supervToWCETs[succSuperv]])
            values = set([])
            for val1 in self.__supervToWCETs[superv]:
                for val2 in self.__supervToWCETs[succSuperv]:
                    values.add(val1+val2)
            self.__supervToWCETs[superv] = values
                    
    def __calculateIntraBlockValue (self, superv):
        intraBlockWCET = 0
        for basicBlockID in superv.getBasicBlockIDs():
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
    edgePrefix   = "e_"
    equals       = " = "
    fileSuffix   = "ilp"
    int_         = "int"
    lt           = " < "
    ltOrEqual    = " <= "
    max_         = "max: "
    plus         = " + "
    semiColon    = ";"
    vertexPrefix = "v_"
    
    @staticmethod
    def getEdgeVariable (edgeID, index=0):
        if index:
            return "%s%s_%d" % (LpSolve.edgePrefix, edgeID, index)
        return "%s%d" % (LpSolve.edgePrefix, edgeID)
    
    @staticmethod
    def getVertexVariable (vertexID, index=0):
        if index:
            return "%s%s_%d" % (LpSolve.vertexPrefix, vertexID, index)
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
        
    def _solve(self, filename):
        from subprocess import Popen, PIPE
        import shlex, decimal
        Debug.debugMessage("Solving ILP", 10)
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
                count    = int(lexemes[1]) 
                self._variableToExecutionCount[variable] = count
        return True
                    
class CreateSuperBlockCFGILP (ILP):
    def __init__ (self, basepath, basename, superg, lnt):
        ILP.__init__(self)
        self.__constraints = []
        self.__variables   = set([])
        self.__createConstraints(superg, lnt)
        self.__createObjectiveFunction()
        self.__createIntegerConstraint()
        filename = "%s.%s.%s.%s" % (basepath + os.sep + basename, superg.getName(), "superg", LpSolve.fileSuffix)
        solution = True
        while solution:
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
        constraint    += LpSolve.getNewLine()
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
    
    def __createCapacityConstraints (self, treev, lnt):
        comment = LpSolve.getComment("Capacity constraints on header %d" % treev.getHeaderID())
        self.__constraints.append(comment)
        if treev.getVertexID() == lnt.getRootID():
            constraint = LpSolve.getVertexVariable(treev.getHeaderID(), treev.getHeaderID())
            constraint += LpSolve.equals
            constraint += "1"
            constraint += LpSolve.semiColon
            constraint += LpSolve.getNewLine(2)
            self.__constraints.append(constraint)
        else:
            for ancestorv in lnt.getAllProperAncestors(treev.getVertexID()):
                if ancestorv.getVertexID() == treev.getParentID():
                    constraint = LpSolve.getVertexVariable(treev.getHeaderID(), treev.getHeaderID())
                    constraint += LpSolve.ltOrEqual
                    constraint += "%d " % (ancestorv.getLevel() * 10 + 1)
                    constraint += LpSolve.getVertexVariable(treev.getHeaderID(), ancestorv.getHeaderID())
                    constraint += LpSolve.semiColon
                    constraint += LpSolve.getNewLine(2)
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
                        constraint += LpSolve.getNewLine()
                        self.__constraints.append(constraint)
                self.__constraints.append(LpSolve.getNewLine())
                        
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
                    dummyvar = LpSolve.getEdgeVariable(supere.getEdgeID(), headerID)
                    self.__variables.add(dummyvar)    
                    constraint += dummyvar
                elif succSuperv.numberOfBasicBlocks() == 0:
                    dummyvar = LpSolve.getEdgeVariable(supere.getEdgeID(), headerID)
                    self.__variables.add(dummyvar)    
                    constraint += dummyvar
                else:
                    succRepID = succSuperv.getRepresentativeID()
                    constraint += LpSolve.getVertexVariable(succRepID, headerID)
                if num < len(superedges):
                    constraint += LpSolve.plus
                num += 1
            constraint += LpSolve.semiColon 
            constraint += LpSolve.getNewLine(2)
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
                dummyvar = LpSolve.getEdgeVariable(supere.getEdgeID(), headerID)
                self.__variables.add(dummyvar)    
                constraint += dummyvar
            else:
                predRepID = predSuperv.getRepresentativeID()
                constraint += LpSolve.getVertexVariable(predRepID, headerID)
            if num < superv.numberOfPredecessors():
                constraint += LpSolve.plus
            num += 1
        constraint += LpSolve.semiColon 
        constraint += LpSolve.getNewLine(2)
        self.__constraints.append(constraint)
       
    def __createObjectiveFunction (self):
        constraint = LpSolve.max_
        num        = 1
        for var in self.__variables:
            if var.startswith(LpSolve.vertexPrefix):
                lIndex       = var.find('_')
                rIndex       = var.rfind('_')
                basicBlockID = var[lIndex+1:rIndex]
                constraint += "%s %s" % (basicBlockID, var)
            else:
                constraint += "%d %s" % (0, var)
            if num < len(self.__variables):
                constraint += LpSolve.plus
            if num % 10 == 0:
                constraint += LpSolve.getNewLine()
            num += 1
        constraint += LpSolve.semiColon 
        constraint += LpSolve.getNewLine(2)
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
        constraint += LpSolve.semiColon + LpSolve.getNewLine()
        self.__constraints.append(constraint)
                    
