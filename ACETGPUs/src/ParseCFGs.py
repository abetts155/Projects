import shlex
import CFGs

bbPrefix = "bb_"
IDPrefix = "ID:"
PCPrefix = "PC=0x"

def getInstructionString (lexemes):
    return ' '.join(lexemes)

def getAddress (string):
    return string[len(PCPrefix):]

def getBasicBlockID (string):
    assert string.startswith(bbPrefix), "String %s does not start with BB prefix '%s'" % (string, bbPrefix)
    return int(string[len(bbPrefix):])

def addBasicBlock (line, cfg):
    bbIndex         = 0
    PCIndex         = 2
    firstInstrIndex = 4
    
    lexemes = shlex.split(line)
    if "first:" not in lexemes or "last:" not in lexemes:
        if lexemes[PCIndex].startswith(PCPrefix):
            bbID = getBasicBlockID(lexemes[bbIndex])
            if not cfg.hasVertex(bbID):
                cfg.addVertex(bbID)
            
            address     = getAddress(lexemes[PCIndex])
            instrString = getInstructionString(lexemes[firstInstrIndex:]) 
            bb          = cfg.getVertex(bbID)
            instruction = CFGs.Instruction (address, instrString)
            bb.addInstruction(instruction)

def addEdges (line, cfg):
    bbIndex = 1
    
    lexemes = shlex.split(line)
    bbID = int(lexemes[bbIndex])
    if cfg.hasVertex(bbID):
        predecessors = False
        bb = cfg.getVertex (bbID)
        for lex in lexemes:
            if "Predecessors" in lex:
                predecessors = True
                continue
            if "Successors" in lex:
                break
            if predecessors:
                predID = int(lex)
                if cfg.hasVertex(predID):
                    predv  = cfg.getVertex(predID)
                    predv.addSuccessor(bbID)
                    bb.addPredecessor(predID)

def parseLine (line, cfg):
    if line.startswith(bbPrefix):
        addBasicBlock(line, cfg)
    elif line.startswith(IDPrefix):
        addEdges (line, cfg)