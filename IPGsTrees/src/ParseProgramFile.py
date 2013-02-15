import shlex
import CFGs, Debug, Vertices
        
def setEntryAndExit (icfg):
    withoutPred = []
    withoutSucc = []
    for bb in icfg:
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
    
    entryID = None
    if len(withoutPred) == 0:
        Debug.exitMessage("CFG '%s' does not an entry point" % icfg.getName())
    elif len(withoutPred) > 1:
        debugStr = ""
        for bbID in withoutPred:
            bb       = icfg.getVertex(bbID)
            debugStr += bb.__str__()
        Debug.exitMessage("CFG '%s' has too many entry points: %s" % (icfg.getName(), debugStr))
    else:
        entryID = icfg.getNextVertexID()
        ipoint  = Vertices.Ipoint(entryID, entryID)
        icfg.addIpoint(ipoint)
        icfg.setEntryID(entryID)
        icfg.addEdge(entryID, withoutPred[0])
    
    exitID = None
    if len(withoutSucc) == 0:
        Debug.exitMessage("CFG '%s' does not an exit point" % icfg.getName())
    elif len(withoutSucc) > 1:
        debugStr = ""
        for bbID in withoutSucc:
            bb       = icfg.getVertex(bbID)
            debugStr += bb.__str__()
        Debug.exitMessage("CFG '%s' has too many exit points: %s" % (icfg.getName(), debugStr))
    else:
        exitID = icfg.getNextVertexID()
        ipoint = Vertices.Ipoint(exitID, exitID)
        icfg.addIpoint(ipoint)
        icfg.setExitID(exitID)
        icfg.addEdge(withoutSucc[0], exitID)
        
    assert entryID, "Unable to set entry ID"
    assert exitID, "Unable to set exit ID"
    icfg.addEdge(exitID, entryID)
    
def createProgram (outfile):
    program = CFGs.Program()
    icfg    = None
    bb      = None
    with open(outfile, 'r') as f:
        for line in f:
            line = line.lower()
            if line.startswith('cfg:'):
                lexemes = shlex.split(line)
                assert len(lexemes) == 2, "Unable to parse CFG line %s" % line
                icfg         = CFGs.ICFG()
                functionName = lexemes[-1]
                icfg.setName(functionName)
                program.addICFG(icfg)
                Debug.debugMessage("Found new CFG '%s'" % functionName, 1)
            elif line.startswith('bb:'):
                assert icfg, "Found basic block but current CFG is null"
                lexemes = shlex.split(line) 
                assert len(lexemes) == 2, "Unable to parse basic block line %s" % line
                vertexID = lexemes[-1]
                assert vertexID.isdigit(), "Vertex identifier '%s' is not an integer" % vertexID
                bb = CFGs.BasicBlock(int(vertexID))
                icfg.addVertex(bb)
            elif line.startswith('ipoint'):
                assert bb, "Trying to add an Ipoint to a basic block but current basic block is null"
                index = line.index(':')
                position = line[index+1:].replace(' ', '').strip()
                bb.setIpoint(position)
            elif line.startswith('succ:'):
                import string
                assert bb, "Found edge but current basic block is null"
                index = line.index(':')
                line = line[index+1:]
                splitter = shlex.shlex(line)
                splitter.whitespace += ')'
                splitter.whitespace += '('
                splitter.whitespace_split = False
                lexemes = list(splitter)
                assert len(lexemes) % 3 == 0, "Unable to parse edge information '%s'" % line
                if len(lexemes) > 1:
                    index = 0
                    for lex in lexemes:
                        if index % 3 == 0:
                            functionName = lexemes[index]
                            assert functionName == icfg.getName(), "Call edge found which is currently not handled"
                        elif index % 3 == 2:
                            succID = lexemes[index]
                            assert succID.isdigit(), "Successor identifier '%s' is not an integer" % succID
                            bb.addSuccessor(int(succID))
                        index += 1                        
        assert icfg, "Attempting to analyse CFG but current CFG is null"
        icfg.addPredecessorEdges()
        icfg.addIpointEdges()
        setEntryAndExit(icfg)
        icfg.setEdgeIDs()
    return program     
