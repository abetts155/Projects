import shlex
import CFGs, Debug

vertexID    = 0
PCPrefix    = "PC=0x"
braOP       = "bra"
callOP      = "call"
callpOP     = "callp"
retOP       = "ret"
retpOP      = "retp"
exitOP      = "exit"
breakOP     = "break"
predicated  = "@%p"

def getAddress (string):
    return int(string[len("PC="):], 0)

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
        if firstAddr == addr + 8:
            return bb
    for bb in cfg:
        firstAddr, firstInstr = bb.getFirstInstruction()
        if firstAddr == addr + 4:
            return bb
    assert False, "Unable to find basic block with next address of %s" % addr
    
def addEdgeToBasicBlockWithNextAddress (cfg, bb, lastAddr):
    succ = getBasicBlockWithNextAddress(cfg, lastAddr)
    bb.addSuccessor(succ.getVertexID())
    succ.addPredecessor(bb.getVertexID())
    
def addEdges (cfg):
    import re
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
            # The regular expression pattern matches predicated instructions
            match = re.search(r'@\!?%p', lastStr)
            if match:
                # Add successor with next address
                addEdgeToBasicBlockWithNextAddress(cfg, bb, lastAddr)
        elif callOP in lastStr or callpOP in lastStr:
            lexemes = shlex.split(lastStr)
            label   = lexemes[-1]
            succ    = getBasicBlockWithLabel(cfg, label)
            bb.addSuccessor(succ.getVertexID())
            succ.addPredecessor(bb.getVertexID())
            # Add successor with next address
            addEdgeToBasicBlockWithNextAddress(cfg, bb, lastAddr)
        elif exitOP not in lastStr:
            addEdgeToBasicBlockWithNextAddress(cfg, bb, lastAddr)
                    
def createBasicBlocks (cfg, instructions):
    global vertexID
    leaders = []
    branch  = False
    # First identify the leaders
    for instr in instructions:
        if instr == instructions[0] \
        or instr.getString().startswith(CFGs.Instruction.labelPrefix) \
        or branch:
            leaders.append(instr)
            branch = False
        if braOP in instr.getString() \
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
        
def setEntryAndExit (cfg):
    withoutPred = []
    withoutSucc = []
    for bb in cfg:
        if bb.numberOfSuccessors() == 0:
            withoutSucc.append(bb.getVertexID())
        elif bb.numberOfSuccessors() == 1:
            if bb.hasSuccessor(bb.getVertexID()):
                withoutSucc.append(bb.getVertexID())
        if bb.numberOfPredecessors() == 0:
            withoutPred.append(bb.getVertexID())
        elif bb.numberOfPredecessors() == 1:
            if bb.hasPredecessor(bb.getVertexID()):
                withoutPred.append(bb.getVertexID())
    
    if len(withoutPred) == 0:
        Debug.exitMessage("CFG '%s' does not an entry point" % cfg.getName())
    elif len(withoutPred) > 1:
        debugStr = ""
        for bbID in withoutPred:
            bb       = cfg.getVertex(bbID)
            debugStr += bb.__str__()
        Debug.exitMessage("CFG '%s' has too many entry points: %s" % (cfg.getName(), debugStr))
    else:
        cfg.setEntryID(withoutPred[0])
        
    if len(withoutSucc) == 0:
        Debug.exitMessage("CFG '%s' does not an exit point" % cfg.getName())
    elif len(withoutSucc) > 1:
        debugStr = ""
        for bbID in withoutSucc:
            bb       = cfg.getVertex(bbID)
            debugStr += bb.__str__()
        Debug.exitMessage("CFG '%s' has too many exit points: %s" % (cfg.getName(), debugStr))
    else:
        cfg.setExitID(withoutSucc[0])    
        
def getInstructionString (lexemes):
    return ' '.join(lexemes)
        
def analyseLine (line, instructions, labels):
    lexemes = shlex.split(line)
    if len(lexemes) > 0:
        PCIndex         = 2
        startInstrIndex = 4
        if lexemes[PCIndex].startswith(PCPrefix):
            address     = getAddress(lexemes[PCIndex])
            instrString = getInstructionString(lexemes[startInstrIndex:]) 
            if labels:
                labelString = getInstructionString(labels)
                instrString = labelString + " " + instrString
                labels[:] = []
            instruction = CFGs.Instruction (address, instrString)
            instructions.append(instruction)
        else:
            labels.append(lexemes[-1])
        
def createProgram (cfgLines):
    program      = CFGs.Program()
    cfg          = None
    analyse      = False
    instructions = []
    labels       = []
    for line in cfgLines:
        if "Summary of basic blocks for" in line:
            analyse = False
            assert cfg 
            assert instructions  
            createBasicBlocks(cfg, instructions)
            addEdges(cfg)
            setEntryAndExit(cfg)
            instructions = []   
        if analyse:
            analyseLine(line, instructions, labels)    
        if "Printing basic blocks for function" in line:
            analyse      = True
            lexemes      = shlex.split(line)
            functionName = lexemes[-1][:-1]
            cfg          = CFGs.CFG()
            cfg.setName(functionName)
            program.addCFG(cfg)
            Debug.debugMessage("Found new CFG '%s'" % functionName, 1)
    return program     
