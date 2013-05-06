import Debug, ARM, Vertices
import random, os, hashlib, shlex

newTrace = "=>"
endTrace = "<="

class GenerateTraces:
    maxNumberOfCalls  = 20
    maxLoopIterations = 10
    
    def __init__ (self, basepath, basename, program, numberOfTraces=1):
        self.__program = program
        filename       = basepath + os.sep + basename + ".traces"
        with open(filename, 'w') as self.__outfile:
            self.__outfile.write("%s\n" % hashlib.sha1(basename).hexdigest())
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
        self.__currentICFG  = self.__program.getICFG(rootv.getName())
        self.__currentLNT   = self.__program.getLNT(rootv.getName())
        self.__currentv     = self.__currentICFG.getVertex(self.__currentICFG.getEntryID())
        self.__vertexID     = self.__currentv.getVertexID()
        self.__callStack    = []
        while True: 
            self.__outfile.write("%d " % self.__vertexID)
            if self.__vertexID == self.__currentICFG.getExitID():
                if callg.getVertexWithName(self.__currentICFG.getName()) == rootv:
                    # End of the program reached
                    break
                else:
                    # End of function call
                    Debug.debugMessage("Returning from %s" % self.__currentCallv.getName(), 5)
                    self.__currentCallv, self.__currentICFG, self.__currentLNT, self.__currentv = self.__callStack.pop()
                    self.__vertexID  = self.__currentv.getVertexID()
                    # Go past the call site
                    self.__chooseSuccessorInICFG()
            elif self.__currentLNT.isLoopTail(self.__vertexID):
                tupleIndex = self.__currentICFG.getName(), self.__vertexID
                if tupleIndex not in self.__functionToTailCount:
                    self.__functionToTailCount[tupleIndex] = 1
                    self.__chooseSuccessorInICFG()
                elif self.__functionToTailCount[tupleIndex] < GenerateTraces.maxLoopIterations:
                    self.__functionToTailCount[tupleIndex] += 1
                    self.__chooseSuccessorInICFG()
                else:
                    self.__chooseNonLoopBackEdgeSuccessorInICFG()
            elif self.__currentCallv.isCallSite(self.__vertexID):
                # If the number of calls have been exceeded or we non-deterministically choose not to make the call here
                # then select a successor in the current ICFG 
                if self.__numberOfCalls > GenerateTraces.maxNumberOfCalls or not bool(random.getrandbits(1)):
                    self.__chooseSuccessorInICFG()
                else:
                    # Make the call. First save state then move to the callee ICFG
                    self.__callStack.append((self.__currentCallv, self.__currentICFG, self.__currentLNT, self.__currentv))
                    succe               = self.__currentCallv.getSuccessorEgdeWithCallSiteID(self.__vertexID)
                    self.__currentCallv = callg.getVertex(succe.getVertexID())
                    calleeName          = self.__currentCallv.getName()
                    Debug.debugMessage("Calling %s" % self.__currentCallv.getName(), 5)
                    self.__currentICFG  = self.__program.getICFG(calleeName)
                    self.__currentLNT   = self.__program.getLNT(calleeName)
                    self.__currentv     = self.__currentICFG.getVertex(self.__currentICFG.getEntryID())
                    self.__vertexID     = self.__currentv.getVertexID()
            else:
                self.__chooseSuccessorInICFG()
    
    def __chooseSuccessorInICFG (self):
        succIndex = random.randint(0, self.__currentv.numberOfSuccessors() - 1)
        succID    = self.__currentv.getSuccessorIDs()[succIndex]
        self.__currentv = self.__currentICFG.getVertex(succID)
        self.__vertexID = self.__currentv.getVertexID()
        
    def __chooseNonLoopBackEdgeSuccessorInICFG (self):
        succIDs = [succID for succID in self.__currentv.getSuccessorIDs() if not self.__currentLNT.isLoopBackEdge(self.__vertexID, succID)]
        succIndex = random.randint(0, len(succIDs) - 1)
        succID    = succIDs[succIndex]
        self.__currentv = self.__currentICFG.getVertex(succID)
        self.__vertexID = self.__currentv.getVertexID()
        
class SuperBlockPathInformation:
    def __init__ (self, program):
        self._program = program
        self._allruns = set([])
        self._superBlockCFGInformation = {}
        for cfg in program.getICFGs():
            superg = program.getSuperBlockCFG(cfg.getName())
            self._superBlockCFGInformation[superg] = {}
    
    def _analyseSuperBlock (self, superg, predID, succID, runID):
        if superg.isMonitoredBasicBlock(succID):
            superv = superg.getMonitoredBasicBlockSuperBlock(succID)
            if superv not in self._superBlockCFGInformation[superg]:
                self._superBlockCFGInformation[superg][superv] = set([])
            self._superBlockCFGInformation[superg][superv].add(runID)
        elif superg.isMonitoredEdge(predID, succID):
            superv = superg.getMonitoredEdgeSuperBlock(predID, succID)
            if superv not in self._superBlockCFGInformation[superg]:
                self._superBlockCFGInformation[superg][superv] = set([])
            self._superBlockCFGInformation[superg][superv].add(runID)
    
    def _computePathInformation (self):
        for superg in self._program.getSuperBlockCFGs():
            superg.computePathInformation(self._superBlockCFGInformation[superg], self._allruns)
        
class ParseTraces (SuperBlockPathInformation):
    def __init__ (self, basename, tracefile, program):
        SuperBlockPathInformation.__init__(self, program)
        self.__callg   = self._program.getCallGraph()
        self.__rootv   = self.__callg.getVertex(self.__callg.getRootID())
        self.__bbToCFG = {}
        self.__verifyMagicNumber(basename, tracefile)
        self.__initialise()
        self.__parse(tracefile)
        self._computePathInformation()          
                    
    def __verifyMagicNumber (self, basename, tracefile):
        magicNumber = hashlib.sha1(basename).hexdigest()
        # First assert that the magic numbers match
        with open(tracefile, 'r') as f:
            line = f.readline()
            fileMagicNumber = line.strip()
            assert fileMagicNumber == magicNumber, "The magic number of the trace file does not compute"
    
    def __initialise (self):
        for icfg in self._program.getICFGs():
            for v in icfg:
                self.__bbToCFG[v.getVertexID()] = icfg
    
    def __parse (self, tracefile):
        runID = 0
        with open(tracefile, 'r') as f:
        # Move past the magic number line
            next(f)
            for line in f:
                if line.startswith(newTrace):
                    runID += 1
                    self._allruns.add(runID)
                    self.__resetToRoot()
                elif line.startswith(endTrace):
                    pass
                else:
                    lexemes = shlex.split(line)
                    lastID = None
                    for lex in lexemes:
                        nextID = int(lex)
                        if self.__currentCFG.hasVertex(nextID):
                            self._analyseSuperBlock(self.__currentSuperg, lastID, nextID, runID)
                        else:
                            self.__handleCallAndReturn(lastID, nextID)
                        lastID = nextID
    
    def __resetToRoot (self):
        self.__callStack     = []
        self.__currentCallv  = self.__rootv
        self.__currentCFG    = self._program.getICFG(self.__rootv.getName())
        self.__currentSuperg = self._program.getSuperBlockCFG(self.__rootv.getName())
        
    def __handleCallAndReturn (self, lastID, nextID):
        assert lastID, "The last ID was not set"
        if self.__currentCFG.getExitID() == lastID:
            Debug.debugMessage("Returning because of basic block %d" % lastID, 1)
            self.__currentCallv, self.__currentCFG, self.__currentSuperg = self.__callStack.pop()
        else:
            callerFrame = (self.__currentCallv, self.__currentCFG, self.__currentSuperg)
            self.__callStack.append(callerFrame)
            self.__currentCFG    = self.__bbToCFG[nextID]
            self.__currentCallv  = self.__callg.getVertexWithName(self.__currentCFG.getName())
            self.__currentSuperg = self._program.getSuperBlockCFG(self.__currentCFG.getName())

class Gem5Parser (SuperBlockPathInformation):
    def __init__ (self, program, traceFiles):
        SuperBlockPathInformation.__init__(self, program)
        self.__firstAddr        = None
        self.__lastAddr         = None
        self.__rootCFG          = None
        self.__rootsuperg       = None
        self.__currentCFG       = None
        self.__predBB           = None
        self.__currentBB        = None
        self.__currentSuperg    = None
        self.__stack            = []
        self.__buildAddressInformation()
        self.__parse(traceFiles)
        self._computePathInformation()
        
    def __buildAddressInformation (self):
        self.__rootCFG    = self._program.getRootICFG()
        self.__rootsuperg = self._program.getRootSuperBlockCFG()
        self.__firstAddr  = self.__rootCFG.getFirstInstruction().getAddress()
        lastbb            = self.__rootCFG.getVertex(self.__rootCFG.getExitID())
        for instruction in reversed(lastbb.getInstructions()):
            if instruction.getOp() not in ARM.ARMInstructionSet.Nops:
                self.__lastAddr = instruction.getAddress()
                break
        assert self.__lastAddr, "Unable to find last address"
        Debug.debugMessage("Start address of root function '%s' is %s" % (self.__rootCFG.getName(), hex(self.__firstAddr)), 1)    
        Debug.debugMessage("End address of root function '%s' is %s" % (self.__rootCFG.getName(), hex(self.__lastAddr)), 1)
        
    def __parse (self, traceFiles):
        runID = 0
        for filename in traceFiles:
            parsing = False 
            with open(filename, 'r') as f:
                runID += 1
                self._allruns.add(runID)
                Debug.debugMessage("Analysing gem5 trace file '%s'" % filename, 1)
                for line in f:
                    lexemes  = shlex.split(line)
                    PCLexeme = lexemes[-1]
                    assert len(PCLexeme) == 11, "Unable to parse program counter %s" % PCLexeme
                    try:
                        PCLexeme = PCLexeme[5:]
                        PC       = int(PCLexeme, 16)
                        if PC == self.__firstAddr:
                            parsing              = True
                            self.__currentCFG    = self.__rootCFG
                            self.__predBB        = None
                            self.__currentBB     = self.__currentCFG.getVertex(self.__currentCFG.getEntryID())
                            self.__currentSuperg = self.__rootsuperg 
                        if parsing:
                            self.__parseAddress (PC, runID)
                        if PC == self.__lastAddr:
                            parsing              = False
                            self.__currentCFG    = None
                            self.__predBB        = None
                            self.__currentBB     = None
                            self.__currentSuperg = None
                    except ValueError:
                        Debug.exitMessage("Cannot cast %s into an integer: it is not a hexadecimal string" % PCLexeme)

    def __getCFGWithAddress (self, address):
        for cfg in self._program.getICFGs():
            firstAddress = cfg.getFirstInstruction().getAddress()
            if firstAddress == address:
                return cfg
        assert False, "Unable to find CFG with start address %s" % hex(address)
    
    def __parseAddress (self, address, runID):
        oldBB = self.__currentBB
        if not self.__currentBB.hasAddress(address):
            for succID in self.__currentBB .getSuccessorIDs():
                succv = self.__currentCFG.getVertex(succID)
                if succv.hasAddress(address):
                    self.__predBB    = self.__currentBB
                    self.__currentBB = succv
                    break
            # Unable to find a successor of the current basic block with that address
            # It must be a call or a return
            if oldBB == self.__currentBB:
                if self.__currentCFG.getExitID() ==  self.__currentBB.getVertexID() and self.__currentCFG != self.__rootCFG:
                    (self.__currentCFG, self.__currentBB, self.__currentSuperg) = self.__stack.pop()
                else:
                    callerFrame = (self.__currentCFG, self.__currentBB, self.__currentSuperg)
                    self.__stack.append(callerFrame)
                    self.__currentCFG    = self.__getCFGWithAddress(address)
                    self.__currentBB     = self.__currentCFG.getVertex(self.__currentCFG.getEntryID())
                    self.__currentSuperg = self._program.getSuperBlockCFG(self.__currentCFG.getName())
                    assert self.__currentBB.hasAddress(address), "Calling into '%s' because of address %s but basic block does not contain an instruction with that address" % (self.__currentCFG.getName(), hex(address))      
            if not self.__predBB:
                self._analyseSuperBlock(self.__currentSuperg, Vertices.dummyVertexID, self.__currentBB.getVertexID(), runID)     
            else:
                self._analyseSuperBlock(self.__currentSuperg, self.__predBB.getVertexID(), self.__currentBB.getVertexID(), runID)     
            
            Debug.debugMessage("Now in CFG '%s' at basic block %d" % (self.__currentCFG.getName(),  self.__currentBB.getVertexID()), 10)    

        
        