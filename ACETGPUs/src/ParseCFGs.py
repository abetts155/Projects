import shlex
import CFGs, Debug

vertexID    = 0
PCPrefix    = "PC=0x"
labelPrefix = "l0x"
braOP       = "bra"
callOP      = "call"
callpOP     = "callp"
retOP       = "ret"
retpOP      = "retp"
exitOP      = "exit"
breakOP     = "break"
predicated  = "@$p"

def getAddress (string):
    return string[len(PCPrefix):]

def getBasicBlockWithLabel (cfg, label):
    for bb in cfg:
        firstAddr, firstInstr = bb.getFirstInstruction()
        if firstInstr.containsLabel(label):
            return bb
    assert False, "Unable to find basic block with label '%s'" % label
        
def getBasicBlockWithNextAddress (cfg, addr):
    # Prioritise finding the block whose first address is
    # 8 bytes away from the last instruction of the current block
    for bb in cfg:
        firstAddr, firstInstr = bb.getFirstInstruction()
        if int(firstAddr, 16) == int(addr, 16) + 8:
            return bb
    for bb in cfg:
        firstAddr, firstInstr = bb.getFirstInstruction()
        if int(firstAddr, 16) == int(addr, 16) + 4:
            return bb
    assert False, "Unable to find basic block with next address of %s" % addr
    
def addEdges (cfg):
    for bb in cfg:
        lastAddr, lastInstr = bb.getLastInstruction()
        lastStr = lastInstr.getString()
        if braOP in lastStr:
            lexemes = shlex.split(lastStr)
            # Add successor with label
            label   = lexemes[-1]
            succ    = getBasicBlockWithLabel(cfg, label)
            bb.addSuccessor(succ.getVertexID())
            succ.addPredecessor(bb.getVertexID())
            if predicated in lastStr:
                # Add successor with next address
                succ2 = getBasicBlockWithNextAddress(cfg, lastAddr)
                bb.addSuccessor(succ2.getVertexID())
                succ2.addPredecessor(bb.getVertexID())
        elif exitOP not in lastStr:
            succ = getBasicBlockWithNextAddress(cfg, lastAddr)
            bb.addSuccessor(succ.getVertexID())
            succ.addPredecessor(bb.getVertexID())
                    
def createBasicBlocks (cfg, instructions):
    global vertexID
    leaders = []
    branch  = False
    # First identify the leaders
    for instr in instructions:
        if instr == instructions[0] \
        or instr.getString().startswith(labelPrefix) \
        or exitOP in instr.getString() \
        or branch:
            leaders.append(instr)
            branch = False
        elif braOP in instr.getString() \
        or retOP in instr.getString() \
        or retpOP in instr.getString() \
        or callOP in instr.getString() \
        or callpOP in instr.getString() \
        or breakOP in instr.getString():
            branch = True
    
    # Now create the basic blocks
    bb = None
    for instr in instructions:
        if instr in leaders:
            bb       = CFGs.BasicBlock(vertexID)
            vertexID += 1
            cfg.addVertex(bb)
            bb.addInstruction(instr)
        else:
            assert bb, "Basic block is currently null"
            bb.addInstruction(instr)

def getInstructionString (lexemes):
    return ' '.join(lexemes)
        
def analyseLine (line, cfg):
    lexemes = shlex.split(line)
    if len(lexemes) > 0:
        PCIndex         = 2
        startInstrIndex = 4
        if lexemes[PCIndex].startswith(PCPrefix):
            address     = getAddress(lexemes[PCIndex])
            instrString = getInstructionString(lexemes[startInstrIndex:]) 
            instruction = CFGs.Instruction (address, instrString)
            return instruction
        
def createProgram (cfgLines):
    program      = CFGs.Program()
    cfg          = None
    analyse      = False
    instructions = []
    for line in cfgLines:
        if "Summary of basic blocks for" in line:
            analyse = False
            assert cfg 
            assert instructions  
            createBasicBlocks(cfg, instructions)
            addEdges(cfg)
            instructions = []   
        if analyse:
            instr = analyseLine(line, cfg)
            if instr:
                instructions.append(instr)
        if "Printing basic blocks for function" in line:
            analyse      = True
            lexemes      = shlex.split(line)
            functionName = lexemes[-1][:-1]
            cfg          = CFGs.CFG()
            cfg.setName(functionName)
            program.addCFG(cfg)
            Debug.debugMessage("Found new CFG '%s'" % functionName, 1)
    return program     
