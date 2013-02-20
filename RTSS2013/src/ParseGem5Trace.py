import Debug

def parse (program, traceFile):
    import shlex
    parsing      = False
    rootICFG     = program.getRootICFG()
    firstAddress = rootICFG.getFirstInstruction().getAddress()
    lastAddress  = rootICFG.getLastInstruction().getAddress()
    Debug.debugMessage("Start address of main function is %s" % hex(firstAddress), 1)    
    Debug.debugMessage("End address of main function is %s" % hex(lastAddress), 1)
    with open(traceFile, 'r') as f:
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
                    print hex(PC)
                if PC == lastAddress:
                    parsing = False
            except ValueError:
                Debug.exitMessage("Cannot cast %s into an integer. It is not a hexadecimal string." % PCLexeme)