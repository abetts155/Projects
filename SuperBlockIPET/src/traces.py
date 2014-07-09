import Debug
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
        self._bounds = {}
        self._boundsCurrent = {}
        self._freshInvocations = {}
        self._freshInvocationsCurrent = {}
        self._relativeBoundToInnermostLoop = {}
        for superg in self._program.getSuperBlockCFGs():
            functionName = superg.getName()
            for v in superg:
                for bbID in v.basicBlocks:
                    self._executionTimes[(functionName, bbID)] = 0
                    self._bounds[(functionName, bbID)] = 0
                    self._boundsCurrent[(functionName, bbID)] = 0  
                if not v.basicBlocks:    
                    for edge in v.edges:
                        self._bounds[(functionName, edge)] = 0
                        self._boundsCurrent[(functionName, edge)] = 0
        for lnt in self._program.getLNTs():
            functionName = lnt.getName()
            for headerID in lnt.getHeaderIDs():
                self._freshInvocations[(functionName, headerID)] = 0
                self._freshInvocationsCurrent[(functionName, headerID)] = 0  
                self._relativeBoundToInnermostLoop[(functionName, headerID)] = 0
            
    def getBound (self, functionName, vertexID):
        tupleKey = (functionName, vertexID)
        assert tupleKey in self._bounds
        return self._bounds[tupleKey]
    
    def getRelativeBound (self, functionName, headerID):
        tupleKey = (functionName, headerID)
        assert tupleKey in self._relativeBoundToInnermostLoop
        return self._relativeBoundToInnermostLoop[tupleKey]
    
    def getFreshInvocations (self, functionName, headerID):
        tupleKey = (functionName, headerID)
        assert tupleKey in self._freshInvocations
        return self._freshInvocations[tupleKey]

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
    
    def output (self):
        for cfg in self._program.getCFGs():
            functionName = cfg.getName()
            lnt = self._program.getLNT(functionName)
            for v in cfg:
                vertexID = v.getVertexID()
                tupleKey = (functionName, vertexID)
                if not lnt.isLoopHeader(vertexID):
                    Debug.debugMessage("%s: WCET = %d\tCount = %d" % (tupleKey, 
                                                                        self._executionTimes[tupleKey], 
                                                                        self._bounds[tupleKey]), 10)
                else:
                    Debug.debugMessage("%s: WCET = %d\tCount = %d\tInvocations = %d\tRelative = %d" % (tupleKey, 
                                                                        self._executionTimes[tupleKey], 
                                                                        self._bounds[tupleKey],
                                                                        self._freshInvocations[tupleKey],
                                                                        self._relativeBoundToInnermostLoop[tupleKey]), 10)
    
class ParseTraces (TraceInformation):
    def __init__ (self, basename, tracefile, program):
        TraceInformation.__init__(self, program)
        self.__initialise()
        self.__parse(tracefile)
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
        self.__relativeBounds  = []
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
        self.__relativeBounds  = []
        
    def __parse (self, tracefile):
        runID = 0
        with open(tracefile, 'r') as f:
            for line in f:
                if line.startswith(newTrace):
                    assert not self.__relativeBounds, self.__relativeBounds.__str__()
                    runID += 1
                    self._allruns.add(runID)
                    self.__reset()
                elif line.startswith(endTrace):
                    self.__computeMaximumBounds()
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
                                    self.__computeMaximumBounds()
                                    self.__handleReturn()
                                    succIDs = self.__currentBB.getSuccessorIDs()
                                    assert len(succIDs) == 1
                                    succv            = self.__currentCFG.getVertex(succIDs[0])
                                    self.__predBB    = self.__currentBB
                                    self.__currentBB = succv
                                else:
                                    self.__handleCall(nextID) 
                        self.__incrementBounds()    
    
    def __incrementBounds (self):
        tupleKey = (self.__currentCFG.getName(), self.__currentBB.getVertexID())
        self._boundsCurrent[tupleKey] += 1 
        if self.__currentLNT.isLoopHeader(self.__currentBB.getVertexID()):
            headerID = self.__currentBB.getVertexID()
            if not self.__predBB or not self.__currentLNT.isLoopBackEdge(self.__predBB.getVertexID(), headerID):
                self._freshInvocationsCurrent[tupleKey] += 1
        if self.__predBB:
            # Record bounds on specific edges
            tupleKey2 = (self.__currentCFG.getName(), (self.__predBB.getVertexID(), self.__currentBB.getVertexID()))
            if tupleKey2 in self._boundsCurrent:
                self._boundsCurrent[tupleKey2] += 1
            # Increment relative loop bounds
            if self.__currentLNT.isLoopHeader(self.__predBB.getVertexID()):
                headerID = self.__predBB.getVertexID()
                # Ignore the entry vertex
                if self.__currentCFG.getEntryID() != headerID:
                    if headerID in [elem[0] for elem in self.__relativeBounds]:
                        tos = self.__relativeBounds.pop()
                        assert tos[0] == headerID
                        bound = tos[1] + 1
                        self.__relativeBounds.append((headerID, bound))
                    else:
                        self.__relativeBounds.append((headerID, 1))
            # Compute relative loop bounds if a loop-exit edge is detected
            headerID = self.__currentLNT.isLoopExitEdge(self.__predBB.getVertexID(), self.__currentBB.getVertexID())
            if headerID:
                assert self.__relativeBounds, "Relative bound stack is empty"
                tos = self.__relativeBounds.pop()
                assert tos[0] == headerID, "Top of relative bound stack and header ID for loop-exit edge do not match. Found %d and %d respectively" % (tos[0], headerID)
                tupleKey = (self.__currentCFG.getName(), headerID)
                self._relativeBoundToInnermostLoop[tupleKey] = max(self._relativeBoundToInnermostLoop[tupleKey], tos[1])               

    def __computeMaximumBounds (self):
        for tupleKey in self._bounds:
            self._bounds[tupleKey] = max(self._boundsCurrent[tupleKey], self._bounds[tupleKey])
            self._boundsCurrent[tupleKey] = 0
        for tupleKey in self._freshInvocationsCurrent:
            self._freshInvocations[tupleKey] = max(self._freshInvocationsCurrent[tupleKey], self._freshInvocations[tupleKey])
            self._freshInvocationsCurrent[tupleKey] = 0
    
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
            