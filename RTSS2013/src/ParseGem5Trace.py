import Debug

addressToBasicBlock = {}
addressToICFG       = {}
firstAddress        = None
lastAddress         = None

def parseAddress (program, address):
    icfg = addressToICFG[address]
    v    = addressToBasicBlock[address]
    Debug.debugMessage("Now in CFG %s at basic block %d" % (icfg.getName(), v.getVertexID()), 1)
    
def buildAddressInformation (program):
    global firstAddress, lastAddress
    rootICFG     = program.getRootICFG()
    firstAddress = rootICFG.getFirstInstruction().getAddress()
    lastAddress  = rootICFG.getLastInstruction().getAddress()
    Debug.debugMessage("Start address of main function is %s" % hex(firstAddress), 1)    
    Debug.debugMessage("End address of main function is %s" % hex(lastAddress), 1)
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
                    Debug.exitMessage("Cannot cast %s into an integer. It is not a hexadecimal string." % PCLexeme)