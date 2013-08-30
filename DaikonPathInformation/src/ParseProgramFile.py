import CFGs, Debug, Programs, Vertices
import shlex

cfgIndicator          = 'cfg:'
bbIndicator           = 'bb:'
ipointIndicator       = 'ipoint:'
successorsIndicator   = 'succ:'
instructionsIndicator = 'instructions:'
        
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
        entryID = withoutPred[0]
        icfg.setEntryID(entryID)
    
    exitID = None
    if len(withoutSucc) == 0:
        Debug.warningMessage("CFG '%s' does not an exit point" % icfg.getName())
    elif len(withoutSucc) > 1:
        debugStr = ""
        for bbID in withoutSucc:
            bb       = icfg.getVertex(bbID)
            debugStr += bb.__str__()
        Debug.exitMessage("CFG '%s' has too many exit points: %s" % (icfg.getName(), debugStr))
    else:
        exitID = withoutSucc[0]
        icfg.setExitID(exitID)
    
    if entryID and exitID:
        icfg.addEdge(exitID, entryID)
    
def readInProgram (programFile):
    import re
    program = Programs.Program()
    # First parse the file for functions to partially build call graph
    with open(programFile, 'r') as f:
        for line in f:
            line = line.lower()
            if line.startswith(cfgIndicator):
                lexemes = shlex.split(line)
                assert len(lexemes) == 2, "Unable to parse CFG line %s" % line
                icfg         = CFGs.CFG()
                functionName = lexemes[-1]
                # Make sure that the first characters of the function name are non-digits
                # Otherwise function calls will be ambiguous with successor IDs inside a function
                match = re.match(r'\D+', functionName)
                if not match: 
                    Debug.exitMessage("Function name '%s' is disallowed. Every name must start with a non-digit character" % functionName)
                icfg.setName(functionName)
                program.addICFG(icfg, functionName)
                Debug.debugMessage("Found new CFG '%s'" % functionName, 1)
    # Now add the relevant edges to the CFGs and the call graph
    with open(programFile, 'r') as f: 
        bbIDs        = set([]) 
        icfg         = None
        bb           = None   
        instructions = False
        for line in f:
            line = line.lower()
            if line.startswith(cfgIndicator):
                instructions = False
                lexemes = shlex.split(line)
                assert len(lexemes) == 2, "Unable to parse CFG line %s" % line
                functionName = lexemes[-1]
                icfg         = program.getICFG(functionName)
                Debug.debugMessage("Retrieved CFG for '%s'" % functionName, 1)
            elif line.startswith(bbIndicator):
                instructions = False
                assert icfg, "Found basic block but current CFG is null"
                lexemes = shlex.split(line) 
                assert len(lexemes) == 2, "Unable to parse basic block line %s" % line
                vertexID = lexemes[-1]
                assert vertexID.isdigit(), "Vertex identifier '%s' is not an integer" % vertexID
                vertexID = int(vertexID)
                if vertexID in bbIDs:
                    Debug.exitMessage("Basic block IDs must be unique across ALL functions. Found duplicate ID %d in '%s'" % (vertexID, icfg.getName()))
                else:
                    bbIDs.add(vertexID)
                bb = Vertices.BasicBlock(vertexID, functionName)
                icfg.addVertex(bb)
            elif line.startswith(ipointIndicator):
                instructions = False
                assert bb, "Trying to add an Ipoint to a basic block but current basic block is null"
                index = line.index(':')
                position = line[index+1:].replace(' ', '').strip()
                bb.setIpoint(position)
            elif line.startswith(successorsIndicator):
                instructions = False
                assert bb, "Found edge but current basic block is null"
                index = line.index(':')
                line = line[index+1:]
                splitter = shlex.shlex(line)
                splitter.whitespace += ','
                splitter.whitespace_split = True
                lexemes = list(splitter)  
                if len(lexemes):
                    for lex in lexemes:
                        if lex.isdigit():
                            succID = int(lex)
                            bb.addSuccessor(succID)
                        else:
                            calleeName = lex
                            program.getCallGraph().addEdge(icfg.getName(), calleeName, bb.getVertexID())
                            icfg.addCallSite(bb.getVertexID(), calleeName)                                    
            elif line.startswith(instructionsIndicator):
                instructions = True
            # Ignore lines consisting of whitespace only
            elif instructions and not re.match(r'\s+', line):
                assert bb, "Found instruction but current basic block is null"
                lexemes = shlex.split(line)
                address = None
                fields  = []
                for index, lex in enumerate(lexemes):
                    if index == 0:
                        address = int(lex[1:-1], 16)
                    else:
                        fields.append(lex[1:-1])
                assert address is not None, "No address found in instruction %s" % line
                instruction = CFGs.Instruction(address, fields)
                bb.addInstruction(instruction)
    return program
    
def createProgram (programFile):
    program = readInProgram(programFile)
    program.getCallGraph().findAndSetRoot()
    for icfg in program.getICFGs():
        icfg.addPredecessorEdges()
        setEntryAndExit(icfg)
        icfg.setEdgeIDs()
    return program     
