import Debug, SuperBlocks
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
        
class ParseTraces:
    def __init__ (self, basename, tracefile, program):
        self.__program = program
        self.__callg   = self.__program.getCallGraph()
        self.__rootv   = self.__callg.getVertex(self.__callg.getRootID())
        self.__verifyMagicNumber(basename, tracefile)
        self.__parse(tracefile)
    
    def __verifyMagicNumber (self, basename, tracefile):
        magicNumber = hashlib.sha1(basename).hexdigest()
        # First assert that the magic numbers match
        with open(tracefile, 'r') as f:
            line = f.readline()
            fileMagicNumber = line.strip()
            assert fileMagicNumber == magicNumber, "The magic number of the trace file does not compute"
    
    def __parse (self, tracefile):
        self.__on = []
        with open(tracefile, 'r') as f:
        # Move past the magic number line
            next(f)
            for line in f:
                if line.startswith(newTrace):
                    self.__resetToRoot()
                elif line.startswith(endTrace):
                    self.__analysePathInformation()
                else:
                    lexemes = shlex.split(line)
                    for lex in lexemes:
                        nextID = int(lex)
                        superv = self.__currentSuperg.getSuperBlock(nextID)
                        if superv.numberOfSuccessors() == 0 and nextID == superv.getRepresentativeID():
                            self.__on.append(superv)      
        for (superv1, superv2, pathRelation) in self.__currentSuperg.getTruePathRelationEdges():
            print "(%s %s %s) is TRUE" % (superv1.getVertexID(), superv2.getVertexID(), pathRelation) 
        for (superv1, superv2, pathRelation) in self.__currentSuperg.getFalsePathRelationEdges():
            print "(%s %s %s) is FALSE" % (superv1.getVertexID(), superv2.getVertexID(), pathRelation)  
    
    def __resetToRoot (self):
        self.__callStack     = []
        self.__currentCallv  = self.__rootv
        self.__currentSuperg = self.__program.getSuperBlockCFG(self.__rootv.getName())
    
    def __analysePathInformation (self):
        Debug.debugMessage("*** End of program run ***", 10)
        falsifySet = set([])
        for (superv1, superv2, pathRelation) in self.__currentSuperg.getTruePathRelationEdges():
            if pathRelation == SuperBlocks.PATHRELATION.MUTUAL_EXCLUSION \
            and superv1 in self.__on and superv2 in self.__on:
                falsifySet.add((superv1, superv2, pathRelation))    
            if pathRelation == SuperBlocks.PATHRELATION.MUTUAL_INCLUSION \
            and ((superv1 in self.__on and superv2 not in self.__on) or (superv1 not in self.__on and superv2 in self.__on)):
                falsifySet.add((superv1, superv2, pathRelation))
        for pathTuple in falsifySet:
            self.__currentSuperg.falsify(pathTuple)
        self.__on = []
        
        