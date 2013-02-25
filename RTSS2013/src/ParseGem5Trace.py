import Debug, ARM

addressToBasicBlock = {}
addressToICFG       = {}
firstAddress        = None
lastAddress         = None

def parseAddress (program, address):
    assert address in addressToICFG, "Unable to find CFG for address %s" % hex(address)
    assert address in addressToBasicBlock, "Unable to find basic block for address %s" % hex(address)
    icfg = addressToICFG[address]
    v    = addressToBasicBlock[address]
    Debug.debugMessage("Now in CFG %s at basic block %d" % (icfg.getName(), v.getVertexID()), 1)
    
def buildAddressInformation (program):
    global firstAddress, lastAddress
    rootICFG     = program.getRootICFG()
    firstAddress = rootICFG.getFirstInstruction().getAddress()
    lastbb       = rootICFG.getVertex(rootICFG.getExitID())
    for instruction in reversed(lastbb.getInstructions()):
        if instruction.getOp() not in ARM.ARMInstructionSet.Nops:
            lastAddress = instruction.getAddress()
            break
    assert lastAddress, "Unable to find last address"
    Debug.debugMessage("Start address of root function '%s' is %s" % (rootICFG.getName(), hex(firstAddress)), 1)    
    Debug.debugMessage("End address of root function '%s' is %s" % (rootICFG.getName(), hex(lastAddress)), 1)
    for icfg in program.getICFGs():
        for v in icfg:
            for instruction in v.getInstructions():
                address                      = instruction.getAddress()
                addressToBasicBlock[address] = v
                addressToICFG[address]       = icfg

def parse (program, traceFiles):
    global firstAddress, lastAddress
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
                        parsing = True
                    if parsing:
                        parseAddress (program, PC)
                    if PC == lastAddress:
                        parsing = False
                except ValueError:
                    Debug.exitMessage("Cannot cast %s into an integer: it is not a hexadecimal string." % PCLexeme)