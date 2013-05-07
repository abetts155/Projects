from Vertices import HeaderVertex
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
            cfg    = program.getICFG(callv.getName())
            CreateCFGILP(basepath, basename, cfg, lnt, superg)
            #CreateSuperBlockCFGILP(basepath, basename, superg, lnt)
            #TreeBasedCalculation(superg, lnt)
            
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
            constraint += LpSolve.getNewLine(2)
            self.__constraints.append(constraint)
    
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
                    constraint += LpSolve.equals
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
        constraint += LpSolve.getNewLine(2)
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

class CreateCFGILP (ILP):
    def __init__ (self, basepath, basename, cfg, lnt, superg):
        ILP.__init__(self)
        self.__constraints = []
        self.__variables   = set([])
        self.__createStructuralConstraints(cfg)
        self.__createLoopConstraints(cfg, lnt)
        self.__addExclusiveConstraints(superg)
        self.__addAlwaysConstraints(superg)
        self.__createObjectiveFunction()
        self.__createIntegerConstraint()
        filename = "%s.%s.%s.%s" % (basepath + os.sep + basename, cfg.getName(), "cfg", LpSolve.fileSuffix)
        with open(filename, 'w') as ilpFile:
            for constraint in self.__constraints:
                ilpFile.write(constraint)        
        if self._solve(filename):
            Debug.verboseMessage("IPET:: WCET(%s) = %d" % (cfg.getName(), self._wcet)) 
    
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
            constraint1 += LpSolve.getNewLine() 
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
            constraint2 += LpSolve.getNewLine(2) 
            self.__constraints.append(constraint2)
    
    def __createLoopConstraints (self, cfg, lnt):
        import random
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
                        constraint += LpSolve.getNewLine(2)
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
                                    constraint += "%d " % (random.randint(5,25))
                                    constraint += LpSolve.getEdgeVariable(edge[0], edge[1])
                                    if num < len(forwardPredIDs):
                                        constraint += LpSolve.plus
                                    num += 1
                                constraint += LpSolve.semiColon
                                constraint += LpSolve.getNewLine(2)
                                self.__constraints.append(constraint)
                            else:
                                pass
    
    def __addExclusiveConstraints (self, superg):
        for exclusiveTuple in superg.getSuperBlockPathInformationGraph().exclusiveTuples:
            comment = LpSolve.getComment("Mutual exclusive constraint")
            self.__constraints.append(comment)
            constraint = ""
            sizeOfExclusiveSet = len(exclusiveTuple)
            num = 1
            for superv in exclusiveTuple:
                if superv.getBasicBlockIDs():
                    constraint += LpSolve.getVertexVariable(superv.getRepresentativeID())
                else:
                    edges = superv.getEdges()
                    assert len(edges) == 1
                    edge = list(edges)[0]
                    constraint += LpSolve.getEdgeVariable(edge[0], edge[1])
                if num < sizeOfExclusiveSet:
                    constraint += LpSolve.plus
                num += 1
            constraint += LpSolve.ltOrEqual
            constraint += str(sizeOfExclusiveSet - 1)
            constraint += LpSolve.semiColon
            constraint += LpSolve.getNewLine(2)
            self.__constraints.append(constraint)
    
    def __addAlwaysConstraints (self, superg):
        for superv in superg.getSuperBlockPathInformationGraph().alwaysSuperBlocks:
            comment = LpSolve.getComment("Always executes constraint")
            self.__constraints.append(comment)
            if superv.getBasicBlockIDs():
                constraint = LpSolve.getVertexVariable(superv.getRepresentativeID())
            else:
                edges = superv.getEdges()
                assert len(edges) == 1
                edge = list(edges)[0]
                constraint = LpSolve.getEdgeVariable(edge[0], edge[1])
            constraint += LpSolve.gtOrEqual
            constraint += str(1)
            constraint += LpSolve.semiColon
            constraint += LpSolve.getNewLine(2)
            self.__constraints.append(constraint)
    
                
    def __createObjectiveFunction (self):
        constraint = LpSolve.max_
        num        = 1
        for var in self.__variables:
            if var.startswith(LpSolve.vertexPrefix):
                lIndex       = var.find('_')
                basicBlockID = var[lIndex+1:]
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
