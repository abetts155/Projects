import Debug, Vertices
import random, os, shlex

newTrace = "=>"
endTrace = "<="

class GenerateTraces:
    maxNumberOfCalls  = 20
    maxLoopIterations = 50
    
    def __init__ (self, basepath, basename, program, numberOfTraces=1):
        self.__program = program
        filename       = basepath + os.sep + basename + ".traces"
        with open(filename, 'w') as self.__outfile:
            for trace in xrange(1, numberOfTraces+1):
                Debug.debugMessage("Generating trace #%d" % trace, 1)
                self.__outfile.write("%s\n" % newTrace)
                self.__generateTrace() 
                self.__outfile.write("\n%s\n" % endTrace)
    
    def __generateTrace (self):
        # To keep track of loop tail iteration count
        self.__functionToTailCount = {}
        # To keep trace of the number of function calls
        self.__numberOfCalls = 0
        callg = self.__program.getCallGraph()
        rootv = callg.getVertex(callg.getRootID())
        self.__currentCallv = rootv
        self.__currentCFG   = self.__program.getCFG(rootv.getName())
        self.__currentLNT   = self.__program.getLNT(rootv.getName())
        self.__currentv     = self.__currentCFG.getVertex(self.__currentCFG.getEntryID())
        self.__vertexID     = self.__currentv.getVertexID()
        self.__callStack    = []
        while True: 
            self.__outfile.write("%d " % self.__vertexID)
            if self.__vertexID == self.__currentCFG.getExitID():
                if callg.getVertexWithName(self.__currentCFG.getName()) == rootv:
                    # End of the program reached
                    break
                else:
                    # End of function call
                    Debug.debugMessage("Returning from %s" % self.__currentCallv.getName(), 5)
                    self.__currentCallv, self.__currentCFG, self.__currentLNT, self.__currentv = self.__callStack.pop()
                    self.__vertexID  = self.__currentv.getVertexID()
                    # Go past the call site
                    self.__chooseSuccessorInCFG()
            elif self.__currentLNT.isLoopTail(self.__vertexID):
                tupleKey = self.__currentCFG.getName(), self.__vertexID
                if tupleKey not in self.__functionToTailCount:
                    self.__functionToTailCount[tupleKey] = 1
                    self.__chooseSuccessorInCFG()
                elif self.__currentv.numberOfSuccessors() == 1:
                    self.__functionToTailCount[tupleKey] += 1
                    self.__chooseSuccessorInCFG()
                elif self.__functionToTailCount[tupleKey] < GenerateTraces.maxLoopIterations:
                    self.__functionToTailCount[tupleKey] += 1
                    self.__chooseSuccessorInCFG()
                else:
                    self.__chooseNonLoopBackEdgeSuccessorInCFG()
            elif self.__currentCFG.isCallSite(self.__vertexID):
                # Make the call. First save state then move to the callee CFG
                self.__callStack.append((self.__currentCallv, self.__currentCFG, self.__currentLNT, self.__currentv))
                succID              = self.__currentCallv.getSuccessorWithCallSite(self.__vertexID)
                self.__currentCallv = callg.getVertex(succID)
                calleeName          = self.__currentCallv.getName()
                Debug.debugMessage("Calling %s" % self.__currentCallv.getName(), 5)
                self.__currentCFG  = self.__program.getCFG(calleeName)
                self.__currentLNT   = self.__program.getLNT(calleeName)
                self.__currentv     = self.__currentCFG.getVertex(self.__currentCFG.getEntryID())
                self.__vertexID     = self.__currentv.getVertexID()
            else:
                self.__chooseSuccessorInCFG()
    
    def __chooseSuccessorInCFG (self):
        succIndex = random.randint(0, self.__currentv.numberOfSuccessors() - 1)
        succID    = self.__currentv.getSuccessorIDs()[succIndex]
        self.__currentv = self.__currentCFG.getVertex(succID)
        self.__vertexID = self.__currentv.getVertexID()
        
    def __chooseNonLoopBackEdgeSuccessorInCFG (self):
        succIDs = [succID for succID in self.__currentv.getSuccessorIDs() if not self.__currentLNT.isLoopBackEdge(self.__vertexID, succID)]
        succIndex = random.randint(0, len(succIDs) - 1)
        succID    = succIDs[succIndex]
        self.__currentv = self.__currentCFG.getVertex(succID)
        self.__vertexID = self.__currentv.getVertexID()
        
class TraceInformation:
    def __init__ (self, program):
        self._program = program
        self._allruns = set([])
        self._initialiseRequiredWCETInformation()
    
    def _initialiseRequiredWCETInformation (self):
        self._longestTime = 0
        self._executionTimes = {}
        self._iterationBounds = {}
        self._iterationBoundsCurrent = {}
        self._exitBounds = {}
        self._exitBoundsCurrent = {}
        self._innerHeaderToOutermostHeader = {}
        for cfg in self._program.getCFGs():
            functionName = cfg.getName()
            for v in cfg:
                tupleKey = (functionName, v.getVertexID())
                self._executionTimes[tupleKey] = 0
                self._iterationBounds[tupleKey]        = {}
                self._iterationBoundsCurrent[tupleKey] = {}
                self._exitBounds[tupleKey]        = {}
                self._exitBoundsCurrent[tupleKey] = {}
            lnt = self._program.getLNT(functionName)
            for level, vertices in lnt.levelIterator(True):
                if level > 1:
                    for lntv in vertices:
                        if isinstance(lntv, Vertices.HeaderVertex): 
                            parentv = lnt.getVertex(lntv.getParentID())
                            while parentv.getLevel() > 1:
                                parentv = lnt.getVertex(parentv.getParentID())
                            tupleKey = (functionName, lntv.getHeaderID())
                            self._innerHeaderToOutermostHeader[tupleKey] = parentv.getHeaderID()
                            
    def _completeLoopBoundProfiles (self):
        for lnt in self._program.getLNTs():
            functionName = lnt.getName()
            for level, vertices in lnt.levelIterator(True):
                for lntv in vertices:
                    if isinstance(lntv, Vertices.HeaderVertex):
                        headerID = lntv.getHeaderID()
                        if level > 1: 
                            # Ensure that the iteration space has the same dimension as the outermost loop's iteration space
                            # We fill in the gaps by taking the maximum
                            tupleKey = (functionName, headerID)
                            maxBound = 0
                            for bound in self._iterationBounds[tupleKey].values():
                                maxBound = max(bound, maxBound)
                            outerHeaderID = self._innerHeaderToOutermostHeader[tupleKey]
                            tupleKey2     = (functionName, outerHeaderID)
                            for iteration in xrange(1, len(self._iterationBounds[tupleKey2]) + 1):
                                if iteration not in self._iterationBounds[tupleKey]:
                                    self._iterationBounds[tupleKey][iteration] = maxBound
                            # Do the same for the exit path iteration space
                            if not lnt.isDoWhileLoop(headerID):
                                maxBound      = 0
                                for bound in self._exitBounds[tupleKey].values():
                                    maxBound = max(bound, maxBound)
                                for iteration in xrange(1, len(self._iterationBounds[tupleKey2]) + 1):
                                    if iteration not in self._exitBounds[tupleKey]:
                                        self._exitBounds[tupleKey][iteration] = maxBound

    def getExecutionTime (self, functionName, vertexID):
        tupleKey = (functionName, vertexID)
        assert tupleKey in self._executionTimes
        return self._executionTimes[tupleKey]
    
    def assignRandomWCETs (self):
        for cfg in self._program.getCFGs():
            functionName = cfg.getName()
            for v in cfg:
                tupleKey = (functionName, v.getVertexID())
                self._executionTimes[tupleKey] = random.randint(1,200)
                
    def getIterationPathsProfile (self, functionName, headerID):
        tupleKey = (functionName, headerID)
        assert tupleKey in self._iterationBounds
        lnt     = self._program.getLNT(functionName)
        headerv = lnt.getVertex(lnt.getVertex(headerID).getParentID())
        if headerv.getLevel() == 1 and not lnt.isDoWhileLoop(headerID):
            clone  = dict(self._iterationBounds[tupleKey])
            lastIt = len(clone)
            del clone[lastIt]
            return clone
        else:  
            return self._iterationBounds[tupleKey]
    
    def getExitPathsProfile (self, functionName, headerID):
        tupleKey = (functionName, headerID)
        assert tupleKey in self._exitBounds
        lnt     = self._program.getLNT(functionName)
        headerv = lnt.getVertex(lnt.getVertex(headerID).getParentID())
        if headerv.getLevel() == 1 and not lnt.isDoWhileLoop(headerID):
            clone  = {}
            lastIt = len(self._iterationBounds[tupleKey])
            clone[lastIt] = self._iterationBounds[tupleKey][lastIt]
            return clone
        else:  
            return self._exitBounds[tupleKey]
    
    def output (self):
        for cfg in self._program.getCFGs():
            functionName = cfg.getName()
            for v in cfg:
                tupleKey = (functionName,  v.getVertexID())
                Debug.debugMessage("%s: WCET = %d" % (tupleKey, self._executionTimes[tupleKey]), 10)
                if self._iterationBounds[tupleKey]:
                    Debug.debugMessage("Iteration profile = %s\nExit profile = %s" % (self._iterationBounds[tupleKey], self._exitBounds[tupleKey]), 10)
                    
    
class ParseTraces (TraceInformation):
    def __init__ (self, basename, tracefile, program):
        TraceInformation.__init__(self, program)
        self.__initialise()
        self.__parse(tracefile)
        self._completeLoopBoundProfiles()
        self.output()
                
    def __initialise (self):
        self.__currentContextv = None
        self.__currentCFG      = None
        self.__currentLNT      = None
        self.__predBB          = None
        self.__currentBB       = None
        self.__currentHeaderID = None
        self.__currentSuperg   = None
        self.__stack           = []
        self.__contextg        = self._program.getContextGraph()
        rootv                  = self.__contextg.getVertex(self.__contextg.getRootID())
        self.__rootCFG         = self._program.getCFG(rootv.getName())
        self.__entryToCFG      = {}        
        for cfg in self._program.getCFGs():
            self.__entryToCFG[cfg.getEntryID()] = cfg
    
    def __reset (self):
        self.__currentContextv = self.__contextg.getVertex(self.__contextg.getRootID())
        self.__currentCFG      = self._program.getCFG(self.__currentContextv.getName())
        self.__currentLNT      = self._program.getLNT(self.__currentContextv.getName())
        self.__currentSuperg   = self._program.getSuperBlockCFG(self.__currentContextv.getName())
        self.__predBB          = None
        self.__currentBB       = None
        
    def __parse (self, tracefile):
        runID = 0
        with open(tracefile, 'r') as f:
            for line in f:
                if line.startswith(newTrace):
                    runID += 1
                    self._allruns.add(runID)
                    self.__reset()
                elif line.startswith(endTrace):
                    pass
                else:
                    lexemes = shlex.split(line)
                    for lex in lexemes:
                        nextID = int(lex)
                        if nextID == self.__rootCFG.getEntryID():
                            self.__currentBB = self.__currentCFG.getVertex(nextID)
                        else:
                            found = False
                            for succID in self.__currentBB.getSuccessorIDs():
                                if succID == nextID:
                                    self.__predBB    = self.__currentBB
                                    self.__currentBB = self.__currentCFG.getVertex(succID)       
                                    found = True
                                    break
                            if not found:
                                if self.__currentBB.getVertexID() == self.__currentCFG.getExitID():
                                    self.__handleReturn()
                                    succIDs = self.__currentBB.getSuccessorIDs()
                                    assert len(succIDs) == 1
                                    succv            = self.__currentCFG.getVertex(succIDs[0])
                                    self.__predBB    = self.__currentBB
                                    self.__currentBB = succv
                                else:
                                    self.__handleCall(nextID) 
                        self.__computeExecutionCounts()    
    
    def __computeExecutionCounts (self):
        vertexID = self.__currentBB.getVertexID()
        if self.__currentLNT.isLoopHeader(vertexID):
            treev   = self.__currentLNT.getVertex(vertexID)
            headerv = self.__currentLNT.getVertex(treev.getParentID())
            if headerv.getLevel() <= 1:
                # An outermost enclosing loop has been discovered
                # Extend its iteration space profile by one 
                tupleKey         = (self.__currentCFG.getName(), vertexID)
                currentIteration = len(self._iterationBoundsCurrent[tupleKey]) + 1
                self._iterationBoundsCurrent[tupleKey][currentIteration] = 1 
            else:
                # An inner loop has been discovered
                # Increment the counter associated with the iteration number of the outer loop
                tupleKey          = (self.__currentCFG.getName(), vertexID)
                outermostHeaderID = self._innerHeaderToOutermostHeader[tupleKey]
                tupleKey2          = (self.__currentCFG.getName(), outermostHeaderID)
                currentIteration  = len(self._iterationBoundsCurrent[tupleKey2])
                if currentIteration not in self._iterationBoundsCurrent[tupleKey]:
                    self._iterationBoundsCurrent[tupleKey][currentIteration] = 0
                self._iterationBoundsCurrent[tupleKey][currentIteration] += 1
        # See if we are exiting an outermost loop
        if self.__predBB:
            predID = self.__predBB.getVertexID()
            succID = self.__currentBB.getVertexID()
            if self.__currentLNT.isLoopExitEdge(predID, succID):
                treev    = self.__currentLNT.getVertex(predID)
                headerv  = self.__currentLNT.getVertex(treev.getParentID())
                headerID = headerv.getHeaderID()
                if headerv.getLevel() <= 1:
                    # First analyse the bound for the outermost loop 
                    tupleKey = (self.__currentCFG.getName(), headerID)
                    maxIterations = len(self._iterationBoundsCurrent[tupleKey])
                    if maxIterations > len(self._iterationBounds[tupleKey]):
                        self._iterationBounds[tupleKey] = self._iterationBoundsCurrent[tupleKey]
                    self._iterationBoundsCurrent[tupleKey] = {}
                    # Now analyse all loops which are nested in the outermost loop
                    for headerID2 in self.__currentLNT.getHeaderIDs():
                        tupleKey2 = (self.__currentCFG.getName(), headerID2)
                        # First make sure we have an inner loop and that the loop is nested in the outermost loop from which we are exiting
                        if tupleKey2 in self._innerHeaderToOutermostHeader and headerID == self._innerHeaderToOutermostHeader[tupleKey2]:
                            if not self.__currentLNT.isDoWhileLoop(headerID2):
                                for iteration, bound in self._exitBoundsCurrent[tupleKey2].iteritems():
                                    if iteration not in self._exitBounds[tupleKey2]:
                                        self._exitBounds[tupleKey2][iteration] = bound
                                    else:
                                        self._exitBounds[tupleKey2][iteration] = max(bound, self._exitBounds[tupleKey2][iteration])
                            for iteration, bound in self._iterationBoundsCurrent[tupleKey2].iteritems():
                                if not self.__currentLNT.isDoWhileLoop(headerID2):
                                    bound -= self._exitBoundsCurrent[tupleKey2][iteration]
                                if iteration not in self._iterationBounds[tupleKey2]:
                                    self._iterationBounds[tupleKey2][iteration] = bound
                                else:
                                    self._iterationBounds[tupleKey2][iteration] = max(bound, self._iterationBounds[tupleKey2][iteration])
                            self._iterationBoundsCurrent[tupleKey2] = {}
                            self._exitBoundsCurrent[tupleKey2] = {}
                else:
                    tupleKey          = (self.__currentCFG.getName(), headerID)
                    outermostHeaderID = self._innerHeaderToOutermostHeader[tupleKey]
                    tupleKey2         = (self.__currentCFG.getName(), outermostHeaderID)
                    currentIteration  = len(self._iterationBoundsCurrent[tupleKey2])
                    if currentIteration not in self._exitBoundsCurrent[tupleKey]:
                        self._exitBoundsCurrent[tupleKey][currentIteration] = 0
                    self._exitBoundsCurrent[tupleKey][currentIteration] += 1
    
    def __handleReturn (self):
        Debug.debugMessage("Returning because of basic block %d" % self.__currentBB.getVertexID(), 1)
        (self.__currentContextv, self.__currentCFG, self.__currentLNT, self.__predBB, self.__currentBB, self.__currentSuperg) = self.__stack.pop()
            
    def __handleCall (self, nextID):
        callerFrame = (self.__currentContextv, self.__currentCFG, self.__currentLNT, self.__predBB, self.__currentBB, self.__currentSuperg)
        self.__stack.append(callerFrame)
        newContextID           = self.__currentContextv.getSuccessorWithCallSite(self.__currentBB.getVertexID())
        self.__currentContextv = self.__contextg.getVertex(newContextID)
        self.__currentCFG      = self.__entryToCFG[nextID]
        self.__currentLNT      = self._program.getLNT(self.__currentCFG.getName())
        self.__currentSuperg   = self._program.getSuperBlockCFG(self.__currentCFG.getName())
        self.__predBB          = None
        self.__currentBB       = self.__currentCFG.getVertex(self.__currentCFG.getEntryID())
            