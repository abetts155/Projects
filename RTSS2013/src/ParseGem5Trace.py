import Debug, ARM, SuperBlocks

firstAddress  = None
lastAddress   = None
rootICFG      = None
rootSuperg    = None
currentICFG   = None
currentBB     = None
currentSuperg = None
stack         = []
liveSuperBBs  = set([])

def getICFGWithAddress (program, address):
    for icfg in program.getICFGs():
        firstAddress = icfg.getFirstInstruction().getAddress()
        if firstAddress == address:
            return icfg
    assert False, "Unable to find CFG with start address %s" % hex(address)

def parseAddress (program, address):
    global currentICFG, currentBB, currentSuperg, liveSuperBBs
    if not currentBB.hasAddress(address):
        oldBB = currentBB
        for succID in currentBB.getSuccessorIDs():
            succv = currentICFG.getVertex(succID)
            if succv.hasAddress(address):
                currentBB = succv
                break
        # Unable to find a successor of the current basic block with that address
        # It must be a call or a return
        if oldBB == currentBB:
            if currentICFG.getExitID() == currentBB.getVertexID() and currentICFG != rootICFG:
                (currentICFG, currentBB, currentSuperg) = stack.pop()
            else:
                callerFrame = (currentICFG, currentBB, currentSuperg)
                stack.append(callerFrame)
                currentICFG   = getICFGWithAddress(program, address)
                currentBB     = currentICFG.getVertex(currentICFG.getEntryID())
                currentSuperg = program.getSuperBlockCFG(currentICFG.getName())
                assert currentBB.hasAddress(address), "Calling into '%s' because of address %s but basic block does not contain an instruction with that address" % (currentICFG.getName(), hex(address))      
    currentSuperBB = currentSuperg.getSuperBlock(currentBB.getVertexID())
    if currentSuperBB.numberOfSuccessors() == 0 and currentBB.getVertexID() == currentSuperBB.getRepresentativeID():
        liveSuperBBs.add(currentSuperBB)    
    Debug.debugMessage("Now in CFG '%s' at basic block %d" % (currentICFG.getName(), currentBB.getVertexID()), 20)    
    
def buildAddressInformation (program):
    global firstAddress, lastAddress, rootICFG, rootSuperg
    rootICFG     = program.getRootICFG()
    rootSuperg   = program.getRootSuperBlockCFG()
    firstAddress = rootICFG.getFirstInstruction().getAddress()
    lastbb       = rootICFG.getVertex(rootICFG.getExitID())
    for instruction in reversed(lastbb.getInstructions()):
        if instruction.getOp() not in ARM.ARMInstructionSet.Nops:
            lastAddress = instruction.getAddress()
            break
    assert lastAddress, "Unable to find last address"
    Debug.debugMessage("Start address of root function '%s' is %s" % (rootICFG.getName(), hex(firstAddress)), 1)    
    Debug.debugMessage("End address of root function '%s' is %s" % (rootICFG.getName(), hex(lastAddress)), 1)

def parse (program, traceFiles):
    global firstAddress, lastAddress, rootICFG, rootSuperg, currentICFG, currentBB, currentSuperg, currentSuperBB
    import shlex
    buildAddressInformation(program)
    for filename in traceFiles:
        parsing = False 
        with open(filename, 'r') as f:
            Debug.debugMessage("Analysing gem5 trace file '%s'" % filename, 1)
            for line in f:
                lexemes  = shlex.split(line)
                PCLexeme = lexemes[-1]
                assert len(PCLexeme) == 11, "Unable to parse program counter %s" % PCLexeme
                try:
                    PCLexeme = PCLexeme[5:]
                    PC       = int(PCLexeme, 16)
                    if PC == firstAddress:
                        parsing       = True
                        currentICFG   = rootICFG
                        currentBB     = currentICFG.getVertex(currentICFG.getEntryID())
                        currentSuperg = rootSuperg
                    if parsing:
                        parseAddress (program, PC)
                    if PC == lastAddress:
                        parsing       = False
                        currentICFG   = None
                        currentBB     = None
                        currentSuperg = None
                except ValueError:
                    Debug.exitMessage("Cannot cast %s into an integer: it is not a hexadecimal string." % PCLexeme)
        analysePathInformation(program)            

def analysePathInformation (program):
    global currentSuperg, liveSuperBBs
    Debug.debugMessage("*** End of program run ***", 10)
    for superblockg in program.getSuperBlockCFGs():
        falsifySet = set([])
        for (superv1, superv2, pathRelation) in superblockg.getTruePathRelationEdges():
            if pathRelation == SuperBlocks.PATHRELATION.MUTUAL_EXCLUSION \
            and superv1 in liveSuperBBs and superv2 in liveSuperBBs:
                falsifySet.add((superv1, superv2, pathRelation))    
            if pathRelation == SuperBlocks.PATHRELATION.MUTUAL_INCLUSION \
            and ((superv1 in liveSuperBBs and superv2 not in liveSuperBBs) or (superv1 not in liveSuperBBs and superv2 in liveSuperBBs)):
                falsifySet.add((superv1, superv2, pathRelation))
        for pathTuple in falsifySet:
            superblockg.falsify(pathTuple)
                    
