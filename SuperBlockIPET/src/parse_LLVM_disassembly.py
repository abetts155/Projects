import Debug, Programs, CFGs, Vertices, ParseProgramFile
import re

# Vertex IDs across all CFGs will be unique
newVertexID = 1
        
def addCFGEdges (cfg, labelToBB, bbToPreds):
    for succv in labelToBB.values():
        if succv in bbToPreds:
            for predLabel in bbToPreds[succv]:
                predv = labelToBB[predLabel]
                cfg.addEdge(predv.getVertexID(), succv.getVertexID())
                
def createNewBasicBlock (cfg, labelToBB, label, vertexID):
    Debug.debugMessage("Creating new basic block with label '%s'" % label, 10)
    bb = Vertices.BasicBlock(vertexID, cfg.getName(), label)
    cfg.addVertex(bb)
    labelToBB[label] = bb
    return bb  
                
def parseFile (disassembly, functions, program):
    global newVertexID
    import shlex
    # ==> Regular expressions to trigger actions
    # Ignore lines composed of carriage returns, new lines, and beginning with '!'
    ignoreRegex = re.compile(r'[\r\n\!]+')
    # A new function definition is matched as follows:
    # keyword 'define', return type, function name, formal parameters delimited by parentheses              
    newFunctionRegex = re.compile(r'\s*define.*(void|i[0-9]+|half|float|double)? @\w+\(.*\)')
    # The closing brace of a definition is matched as follows:
    # whitespace, '}'
    closingBraceRegex = re.compile(r'\s*}')
    # An LLVM label is matched as follows:
    # any alpha-numerical character one or more times, ':', any character
    labelRegex = re.compile(r'[\w\.]+:.*')
    # An LLVM comment
    commentRegex = re.compile(r';.*')
    
    callSites = {}    
    with open(disassembly, 'r') as f:
        # Either we're ignoring the line or parsing it to construct a CFG
        cfgState  = False
        # The data structures used to construct a CFG
        cfg       = None
        labelToBB = {}
        bbToPreds = {}
        bb        = None
        for line in f:
            # Ignore comments and empty lines
            if not ignoreRegex.match(line):
                if newFunctionRegex.match(line):
                    functionNames = re.findall(r'@\w+', line)
                    assert len(functionNames) == 1
                    functionName = functionNames[0]
                    functions.add(functionName)
                    cfg = CFGs.CFG()
                    cfg.setName(functionName)
                    program.addCFG(cfg, functionName)
                    cfgState = True
                    continue
                elif cfgState and closingBraceRegex.match(line):
                    cfgState  = False
                    addCFGEdges(cfg, labelToBB, bbToPreds)
                    cfg       = None
                    labelToBB = {}
                    bbToPreds = {}
                    bb        = None
                    continue
                
                if cfgState:
                    if labelRegex.match(line):
                        # New basic block found
                        lexemes = shlex.split(line)
                        # Strip the colon from the label
                        label = lexemes[0][:-1]
                        bb    = createNewBasicBlock(cfg, labelToBB, label, newVertexID)            
                        newVertexID += 1
                        # Find predecessors
                        predLabels = re.findall(r'%[\w\.]+', line)
                        bbToPreds[bb] = set([])
                        for predLabel in predLabels:
                            predLabel = predLabel.replace('%', '')
                            bbToPreds[bb].add(predLabel)
                    elif commentRegex.match(line):
                        labels = re.findall(r'<label>:[0-9]+', line)
                        assert len(labels) == 1
                        label = labels[0][len('<label>:'):]
                        bb = createNewBasicBlock(cfg, labelToBB, label, newVertexID)
                        newVertexID += 1  
                        # Find predecessors 
                        predLabels = re.findall(r'%[0-9]+', line)
                        bbToPreds[bb] = set([])
                        for predLabel in predLabels:
                            predLabel = predLabel.replace('%', '')
                            bbToPreds[bb].add(predLabel)
                    else:
                        if not bb:
                            label = '0'
                            bb    = createNewBasicBlock(cfg, labelToBB, label, newVertexID)
                            newVertexID += 1
                        if re.match(r'\s+%call[0-9]* = call.*@.*', line) or re.match(r'\s+call void @.*', line):
                            calleeNames = re.findall(r'@\w+', line)
                            assert len(calleeNames) >= 1
                            calleeName = calleeNames[0]
                            if (cfg, bb) not in callSites:
                                callSites[(cfg, bb)] = []
                            callSites[(cfg, bb)].append(calleeName)                                
                            
                        #instruction = Instruction(line)
                        #bb.addInstruction(instruction)
    return callSites
    
def addCallGraphEdges (callSites, program):
    global newVertexID
    callg = program.getCallGraph()
    functionNames = set([]) 
    for callv in callg:
        functionNames.add(callv.getName())
    for cfg, bb in callSites.keys():
        callerName = cfg.getName()
        for calleeName in callSites[(cfg, bb)]:
            if calleeName in functionNames:
                newBB = Vertices.BasicBlock(newVertexID, callerName, None)
                newVertexID += 1
                cfg.addVertex(newBB)
                for predID in bb.getPredecessorIDs():
                    cfg.addEdge(predID, newBB.getVertexID())
                    cfg.removeEdge(predID, bb.getVertexID())
                cfg.addEdge(newBB.getVertexID(), bb.getVertexID())
                cfg.addCallSite(newBB.getVertexID(), calleeName)
                callg.addEdge(callerName, calleeName, newBB.getVertexID()) 
    if callg.numOfVertices() > 1:
        for functionName in functionNames:
            callv = callg.getVertexWithName(functionName)
            if callv.numberOfPredecessors() == 0 and callv.numberOfSuccessors() == 0:
                program.removeFunction(functionName)
                
def generateInternalFile (filename, program):
    import os
    base        = os.path.splitext(filename)[0]
    outfilename = base + '.txt'
    with open(outfilename, 'w') as f:
        for cfg in program.getCFGs():
            functionName = cfg.getName()
            f.write("%s %s\n" % (ParseProgramFile.cfgIndicator, functionName))
            for v in cfg:
                vertexID = v.getVertexID()
                f.write("%s %d\n" %  (ParseProgramFile.bbIndicator, vertexID))
                # Write out successors but only if this is not the exit vertex
                if vertexID != cfg.getExitID():
                    counter  = v.numberOfSuccessors()
                    if cfg.isCallSite(vertexID):
                        counter += 1
                    f.write("%s " % ParseProgramFile.successorsIndicator)
                    for succID in v.getSuccessorIDs():
                        f.write("%d" % succID)
                        if counter > 1:
                            f.write(", ")
                        counter -= 1
                if cfg.isCallSite(vertexID):
                    calleeName = cfg.getCalleeName(vertexID)
                    assert calleeName != functionName, "The caller and callee names '%s' match at call site %d" % (functionName, calleeName, vertexID)
                    f.write("%s" % calleeName)
                f.write("\n")
                f.write("\n")
            f.write("\n")

def createProgram (disassembly):
    Debug.verboseMessage("Parsing LLVM bitcode file '%s'" % disassembly)
    functions = set([])
    program   = Programs.Program()
    callSites = parseFile(disassembly, functions, program)
    addCallGraphEdges(callSites, program)
    for cfg in program.getCFGs():
        cfg.setEntryID()
        cfg.setExitID()
    program.getCallGraph().findAndSetRoot()
    generateInternalFile(disassembly, program)
    return program
    