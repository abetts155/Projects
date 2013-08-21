import CFGs, Debug, Programs, Vertices, PathData
import shlex, re

cfgIndicator          = 'cfg:'
bbIndicator           = 'bb:'
ipointIndicator       = 'ipoint:'
successorsIndicator   = 'succ:'
instructionsIndicator = 'instructions:'
pragmaRegex           = re.compile(r'\#pragma\s+(exclusive|inclusive|bounds)\s+.*')

def writeProgram (program, filename):
    with open(filename, 'w') as f:
        for cfg in program.getCFGs():
            f.write("%s %s\n" % (cfgIndicator, cfg.getName()))
            for v in cfg:
                vertexID = v.getVertexID()
                f.write("%s %d\n" % (bbIndicator, vertexID))
                f.write("%s " % successorsIndicator)
                i = 1
                totalSuccessors = v.numberOfSuccessors()
                if cfg.isCallSite(vertexID):
                    assert cfg.getExitID() != vertexID, "Exit vertex in CFG '%s' is a call site, which should be forbidden" % vertexID
                    totalSuccessors += 1
                if cfg.getExitID() != vertexID:
                    for succID in v.getSuccessorIDs():
                        f.write("%d" % succID)
                        if i < totalSuccessors:
                            f.write(", ")
                        i += 1
                    if cfg.isCallSite(vertexID):
                        calleeName = cfg.getCalleeName(vertexID)
                        f.write(calleeName)
                f.write("\n" * 2)        
        
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
    
    entryID = None
    if len(withoutPred) == 0:
        Debug.exitMessage("CFG '%s' does not an entry point" % cfg.getName())
    elif len(withoutPred) > 1:
        debugStr = ""
        for bbID in withoutPred:
            bb       = cfg.getVertex(bbID)
            debugStr += bb.__str__()
        Debug.exitMessage("CFG '%s' has too many entry points: %s" % (cfg.getName(), debugStr))
    else:
        entryID = withoutPred[0]
        cfg.setEntryID(entryID)
    
    exitID = None
    if len(withoutSucc) == 0:
        Debug.warningMessage("CFG '%s' does not an exit point" % cfg.getName())
    elif len(withoutSucc) > 1:
        debugStr = ""
        for bbID in withoutSucc:
            bb       = cfg.getVertex(bbID)
            debugStr += bb.__str__()
        Debug.exitMessage("CFG '%s' has too many exit points: %s" % (cfg.getName(), debugStr))
    else:
        exitID = withoutSucc[0]
        cfg.setExitID(exitID)
    
    if entryID and exitID:
        cfg.addEdge(exitID, entryID)
    
def readInProgram (programFile):
    program = Programs.Program()
    # First parse the file for functions to partially build call graph
    with open(programFile, 'r') as f:
        for line in f:
            line = line.lower()
            if line.startswith(cfgIndicator):
                lexemes = shlex.split(line)
                assert len(lexemes) == 2, "Unable to parse CFG line %s" % line
                cfg          = CFGs.CFG()
                functionName = lexemes[-1]
                # Make sure that the first characters of the function name are non-digits
                # Otherwise function calls will be ambiguous with successor IDs inside a function
                match = re.match(r'\D+', functionName)
                if not match: 
                    Debug.exitMessage("Function name '%s' is disallowed. Every name must start with a non-digit character" % functionName)
                cfg.setName(functionName)
                program.addCFG(cfg, functionName)
                Debug.debugMessage("Found new CFG '%s'" % functionName, 1)
    # Now add the relevant edges to the CFGs and the call graph
    with open(programFile, 'r') as f: 
        bbIDs        = set([]) 
        cfg          = None
        bb           = None   
        instructions = False
        for line in f:
            line = line.lower().strip()
            if line.startswith(cfgIndicator):
                instructions = False
                lexemes = shlex.split(line)
                assert len(lexemes) == 2, "Unable to parse CFG line %s" % line
                functionName = lexemes[-1]
                cfg          = program.getCFG(functionName)
                Debug.debugMessage("Retrieved CFG for '%s'" % functionName, 1)
            elif line.startswith(bbIndicator):
                instructions = False
                assert cfg, "Found basic block but current CFG is null"
                lexemes = shlex.split(line) 
                assert len(lexemes) == 2, "Unable to parse basic block line %s" % line
                vertexID = lexemes[-1]
                assert vertexID.isdigit(), "Vertex identifier '%s' is not an integer" % vertexID
                vertexID = int(vertexID)
                if vertexID in bbIDs:
                    Debug.exitMessage("Basic block IDs must be unique across ALL functions. Found duplicate ID %d in '%s'" % (vertexID, cfg.getName()))
                else:
                    bbIDs.add(vertexID)
                bb = Vertices.BasicBlock(vertexID, functionName)
                cfg.addVertex(bb)
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
                            program.getCallGraph().addEdge(cfg.getName(), calleeName, bb.getVertexID())
                            cfg.addCallSite(bb.getVertexID(), calleeName)                                    
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
                assert address, "No address found in instruction %s" % line
                instruction = CFGs.Instruction(address, fields)
                bb.addInstruction(instruction)
            elif pragmaRegex.match(line):
                assert cfg, "Found path information pragma but current CFG is null. List path information after the CFG definition."
                if 'exclusive' in line or 'inclusive' in line:
                    setInfo = re.findall(r'\{.*\}', line)
                    assert len(setInfo) == 1
                    # The first and last element of the split will be the empty character ''
                    basicBlockIDs = re.split(r'\W+', setInfo[0])[1:-1]
                    theSet        = set(map(int, basicBlockIDs))
                    for bbID in theSet:
                        assert cfg.hasVertex(bbID), "Path information pertaining to basic block %d is not valid as it is not contained in this CFG" % bbID
                    if 'exclusive' in line:
                        program.addPathInformation(functionName, PathData.MutualExclusion(theSet))
                    else:
                        program.addPathInformation(functionName, PathData.MutualInclusion(theSet))
                elif 'bounds' in line:
                    info = re.findall(r'\[.*\]', line)
                    assert len(info) == 1
                    bbID, lowerBound, upperBound = tuple(re.split(r'\W+', info[0])[1:-1])
                    assert bbID.isdigit(), "Vertex identifier '%s' is not an integer" % bbID
                    assert lowerBound.isdigit(), "Lower bound '%s' is not an integer" % lowerBound
                    assert upperBound.isdigit(), "Upper bound'%s' is not an integer" % upperBound
                    bbID       = int(bbID)
                    assert cfg.hasVertex(bbID), "Path information pertaining to basic block %d is not valid as it is not contained in this CFG" % bbID
                    lowerBound = int(lowerBound)
                    upperBound = int(upperBound)
                    assert upperBound >= lowerBound, "Upper bound %d is less than lower bound %d" % (upperBound, lowerBound)
                    program.addPathInformation(functionName, PathData.ExecutionBounds(bbID, lowerBound, upperBound))
                else:
                    Debug.exitMessage("Unable to handle path information line '%s'" % line)
        return program
    
def createProgram (programFile):
    program = readInProgram(programFile)
    program.getCallGraph().findAndSetRoot()
    for cfg in program.getCFGs():
        cfg.addPredecessorEdges()
        setEntryAndExit(cfg)
        cfg.setEdgeIDs()
    return program     
