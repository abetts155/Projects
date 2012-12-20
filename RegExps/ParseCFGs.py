import shlex
import CFGs, Debug
        
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
        
def createProgram (outfile):
    program = CFGs.Program()
    cfg     = None
    bb      = None
    with open(outfile, 'r') as f:
        for line in f:
            if line.startswith('CFG:'):
                lexemes = shlex.split(line)
                assert len(lexemes) == 2, "Unable to parse CFG line %s" % line
                cfg          = CFGs.CFG()
                functionName = lexemes[-1]
                cfg.setName(functionName)
                program.addCFG(cfg)
                Debug.debugMessage("Found new CFG '%s'" % functionName, 1)
            elif line.startswith('bb:'):
                assert cfg, "Found basic block but current CFG is null"
                lexemes = shlex.split(line) 
                assert len(lexemes) == 2, "Unable to parse basic block line %s" % line
                vertexID = lexemes[-1]
                assert vertexID.isdigit(), "Vertex identifier '%s' is not an integer" % vertexID
                bb = CFGs.BasicBlock(int(vertexID))
                cfg.addVertex(bb) 
            elif line.startswith('succ:'):
                import string
                assert bb, "Found edge but current basic block is null"
                lexemes = shlex.split(line) 
                if len(lexemes) > 1:
                    for edgeStr in lexemes[1:]:
                        # Translate the characters in the first argument to the character in
                        # the corresponding position in the second argument. Basically stripping out
                        # '(', ')', and ','
                        transtab    = string.maketrans('(),', '   ')
                        edgeStr     = edgeStr.translate(transtab)
                        edgeLexemes = shlex.split(edgeStr)
                        assert len(edgeLexemes) == 2, "Unable to parse edge tuple %s" % edgeStr
                        functionName = edgeLexemes[0]
                        assert functionName == cfg.getName(), "Call edge found which is currently not handled"
                        succID = edgeLexemes[1]
                        assert succID.isdigit(), "Successor identifier '%s' is not an integer" % succID
                        bb.addSuccessor(int(succID))
        assert cfg, "Attempting to add predecessor edges but current CFG is null"
        cfg.addPredecessorEdges()
        setEntryAndExit(cfg)
    return program     
