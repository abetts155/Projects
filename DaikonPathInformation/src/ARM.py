import Debug, CFGs, Programs, Vertices, ParseProgramFile, Utils
import re, shlex, os, sys

debugLevel = 20

class ARMInstructionSet:
    Nops               = ['nop', 'nop.n', 'nop.w']
    PCRegister         = 'pc'
    Call               = 'bl'
    UnconditionalJumps = ['b', 'b.n', 'b.w']
    LoadInstructions   = ['ldr', 'ldr.n', 'ldr.w', 'tbh', 'tbb']
    Branches           = ['b',
                          'bl',
                          'bcc',
                          'bcs',
                          'beq',
                          'bge',
                          'bgt',
                          'bhi',
                          'ble',
                          'bls',
                          'blt',
                          'bmi',
                          'bne',
                          'bpl',
                          'bvs',
                          'bvc',
                          'b.n',
                          'b.w',
                          'bcc.n',
                          'bcc.w',
                          'bcs.n',
                          'bcs.w',
                          'beq.n',
                          'beq.w',
                          'bge.n',
                          'bge.w',
                          'bgt.n',
                          'bgt.w',
                          'bhi.n',
                          'bhi.w',
                          'ble.n',
                          'ble.w',
                          'bls.n',
                          'bls.w',
                          'blt.n',
                          'blt.w',
                          'bne.n',
                          'bne.w']
    

startAddressToFunction          = {}
lastAddressToFunction           = {}
jumpTableToDirectives           = {}
functionToInstructions          = {}
functionToStartAddress          = {}
functionToLastAddress           = {}
functionToLeaders               = {}
functionToDirectives            = {}
functionToJumpTableBasicBlocks  = {}
instructionToBasicBlock         = {}
program                         = None
newVertexID                     = 1

def getInstruction (lexemes):
    comment     = ';'
    address     = None
    instruction = []
    opCodeFound = False
    for index, lex in enumerate(lexemes):
        if index == 0:
            address = int(lex[:-1], 16)
        else:
            if lex == comment:
                break
            if not opCodeFound:
                if len(lex) != 4:
                    # Some instruction ops can be hexadecimal numbers.
                    opCodeFound = True
                else:
                    try:
                        int(lex, 16)
                    except ValueError:
                        opCodeFound = True
            if opCodeFound:
                instruction.append(lex)
    return CFGs.Instruction(address, instruction)
    
def extractInstructions (filename):
    with open(filename, 'r') as f:
        parse                    = False
        lastInstruction          = None
        lastJumpTableInstruction = None
        currentFunction          = None
        for line in f:
            if parse:
                if re.match(r'[0-9a-fA-F]+\s<.*>.*', line):
                    if lastInstruction:
                        assert currentFunction, "No function detected yet"
                        lastAddress                            = lastInstruction.getAddress()
                        functionToLastAddress[currentFunction] = lastAddress
                        lastAddressToFunction[lastAddress]     = currentFunction 
                        Debug.debugMessage("Last address of function '%s' is %s" % (currentFunction, hex(functionToLastAddress[currentFunction])), debugLevel)
                    lexemes = shlex.split(line)
                    assert len(lexemes) == 2, "Unable to handle disassembly line %s" % line
                    address      = int(lexemes[0], 16)
                    functionName = lexemes[1][1:-2]
                    Debug.debugMessage("Detected function '%s' @ start address %d" % (functionName, address), debugLevel)
                    startAddressToFunction[address]                  = functionName
                    currentFunction                                  = functionName
                    functionToInstructions[currentFunction]          = []
                    functionToDirectives[currentFunction]            = []
                    functionToStartAddress[currentFunction]          = address
                    functionToJumpTableBasicBlocks[currentFunction]  = []
                    lastJumpTableInstruction                         = None
                elif re.match(r'\s*[0-9a-fA-F]+:.*', line):
                    # Ignore directives reserving space for data
                    if '.word' not in line and '.short' not in line and '.byte' not in line:                  
                        lexemes     = shlex.split(line.strip())
                        instruction = getInstruction(lexemes)
                        functionToInstructions[currentFunction].append(instruction)
                        lastInstruction = instruction
                        if isJumpTableBranch(instruction):
                            lastJumpTableInstruction = instruction
                            jumpTableToDirectives[instruction] = []
                    else:
                        functionToDirectives[currentFunction].append(line)
                        if lastJumpTableInstruction:
                            jumpTableToDirectives[lastJumpTableInstruction].append(line)   
            elif line.startswith('Disassembly of section'):
                parse = '.text' in line
                
def getCalleeName (instructionFields):
    # The callee name should be delimited by '<' and '>'
    assert instructionFields[2].startswith('<') and instructionFields[2].endswith('>'), "Unable to parse callee name %s" % instructionFields[2]
    calleeName = instructionFields[2][1:-1]
    # Some of the target addresses are NOT the callee start address. In those cases it is an offset from the base address indicated by '+'
    index = calleeName.find('+')
    if index != -1:
        calleeName = calleeName[:index]
    startAddress = int(instructionFields[1], 16) 
    assert startAddress >= functionToStartAddress[calleeName] and startAddress <= functionToLastAddress[calleeName], \
    "The address %s is outside the legal range of addresses for %s" % (hex(startAddress), calleeName)
    return calleeName

def identifyCallGraph (rootFunction):
    assert rootFunction in functionToInstructions, "Unable to locate root function '%s' in disassembly" % rootFunction
    functions    = []
    analysed     = set([])
    functions.append(rootFunction)
    while functions:
        functionName = functions.pop()
        analysed.add(functionName)
        Debug.debugMessage("Analysing function '%s'" % functionName, debugLevel)
        assert functionName in functionToInstructions, "No instructions for '%s' discovered" % functionName
        for instruction in functionToInstructions[functionName]:
            instructionFields = instruction.getInstructionFields()
            if ARMInstructionSet.Call in instructionFields:
                assert len(instructionFields) == 3, "Unable to handle call instruction '%s' since it does not have 3 fields exactly" % instructionFields
                startAddress = int(instructionFields[1], 16)
                assert startAddress in startAddressToFunction, "Unable to find function with start address %s (it should be %s)" % (hex(startAddress), instructionFields[2])
                calleeName = startAddressToFunction[startAddress]
                if calleeName not in analysed:
                    functions.append(calleeName)
            elif instruction.getOp() in ARMInstructionSet.Branches:
                assert len(instructionFields) == 3, "Unable to handle branch instruction '%s' since it does not have 3 fields exactly" % instructionFields
                startAddress = int(instructionFields[1], 16)
                if startAddress < functionToStartAddress[functionName] or startAddress > functionToLastAddress[functionName]:
                    calleeName = getCalleeName(instructionFields)
                    if calleeName not in analysed:
                        functions.append(calleeName)
    return analysed

def isJumpTableBranch (instruction):
    op = instruction.getOp()
    if op in ARMInstructionSet.LoadInstructions: 
        fields      = instruction.getInstructionFields()
        destination = fields[1] 
        if re.match(r'.*%s.*' % ARMInstructionSet.PCRegister, destination):
            return True
    return False

def identifyLeaders (functions):
    functionToBranchTargets = {}
    for functionName in functions:
        Debug.debugMessage("Identifying leaders in '%s'" % functionName, debugLevel)
        functionToLeaders[functionName]       = set([])
        functionToBranchTargets[functionName] = set([])
        newLeader                             = True
        noopAfterJumpTableBranch              = False
        index                                 = 0
        for instruction, nextInstruction in Utils.peekaheadIterator(functionToInstructions[functionName]):
            if newLeader:
                functionToLeaders[functionName].add(instruction)
                newLeader = False
            elif noopAfterJumpTableBranch:
                assert instruction.getOp() in ARMInstructionSet.Nops, "Did not find an no-op after jump table branch. Instead found %s" % instruction
                noopAfterJumpTableBranch = False
                newLeader                = True
            
            op = instruction.getOp()
            if op in ARMInstructionSet.Branches:
                newLeader     = True
                fields        = instruction.getInstructionFields()
                addressTarget = int(fields[1], 16) 
                functionToBranchTargets[functionName].add(addressTarget)
            elif isJumpTableBranch(instruction):
                # Look for instructions with an explicit load into the PC
                Debug.debugMessage("Instruction '%s' is loading a value into the PC" % instruction, debugLevel)
                if nextInstruction and nextInstruction.getOp() in ARMInstructionSet.Nops:
                    noopAfterJumpTableBranch = True
                else:
                    newLeader = True
            index += 1
        for instruction in functionToInstructions[functionName]:
            if instruction.getAddress() in functionToBranchTargets[functionName]:
                functionToLeaders[functionName].add(instruction)
        
def identifyBasicBlocks (functions):
    global newVertexID
    for functionName in functions:
        Debug.debugMessage("Identifying basic blocks in '%s'" % functionName, debugLevel)
        icfg = CFGs.CFG()
        icfg.setName(functionName)
        program.addICFG(icfg, functionName)
        bb = None
        for instruction in functionToInstructions[functionName]:
            if instruction in functionToLeaders[functionName]:
                Debug.debugMessage("Instruction @ %s is a leader" % hex(instruction.getAddress()), debugLevel)
                vertexID = newVertexID
                bb       = Vertices.BasicBlock(vertexID, name=functionName)
                icfg.addVertex(bb)
                newVertexID += 1
            assert bb, "Basic block is currently null"
            bb.addInstruction(instruction)
            instructionToBasicBlock[instruction] = bb
            if isJumpTableBranch(instruction):
                functionToJumpTableBasicBlocks[functionName].append(bb)
            
def addEdges (functions):
    for functionName in functions:
        Debug.debugMessage("Adding edges in '%s'" % functionName, debugLevel)
        icfg   = program.getICFG(functionName)
        predID = Vertices.dummyVertexID
        for instruction in functionToInstructions[functionName]:
            v = instructionToBasicBlock[instruction]
            if predID != Vertices.dummyVertexID:
                icfg.addEdge(predID, v.getVertexID())
                predID = Vertices.dummyVertexID
            if v.getLastInstruction() == instruction:
                if instruction.getOp() == ARMInstructionSet.Call:
                    instructionFields = instruction.getInstructionFields()
                    calleeName        = getCalleeName(instructionFields)
                    icfg.addCallSite(v.getVertexID(), calleeName)
                    program.getCallGraph().addEdge(functionName, calleeName, v.getVertexID())
                    predID = v.getVertexID()
                elif instruction.getOp() in ARMInstructionSet.UnconditionalJumps:
                    instructionFields = instruction.getInstructionFields()
                    jumpAddress       = int(instructionFields[1], 16)
                    if jumpAddress >= functionToStartAddress[functionName] and jumpAddress <= functionToLastAddress[functionName]:
                        succv = icfg.getVertexWithAddress(jumpAddress)
                        icfg.addEdge(v.getVertexID(), succv.getVertexID())
                    else:
                        calleeName = startAddressToFunction[jumpAddress]
                        icfg.addCallSite(v.getVertexID(), calleeName)
                        program.getCallGraph().addEdge(functionName, calleeName, v.getVertexID())
                        predID = v.getVertexID()
                elif instruction.getOp() in ARMInstructionSet.Branches:
                    instructionFields = instruction.getInstructionFields()
                    branchAddress     = int(instructionFields[1], 16)
                    if branchAddress >= functionToStartAddress[functionName] and branchAddress <= functionToLastAddress[functionName]:
                        succv = icfg.getVertexWithAddress(branchAddress)
                        icfg.addEdge(v.getVertexID(), succv.getVertexID())
                    else:
                        calleeName = getCalleeName(instructionFields)
                        icfg.addCallSite(v.getVertexID(), calleeName)
                        program.getCallGraph().addEdge(functionName, calleeName, v.getVertexID())
                    predID = v.getVertexID()
                elif v in functionToJumpTableBasicBlocks[functionName]:
                    pass
                else:
                    predID = v.getVertexID()
                    
def addJumpTableEdges (functions):
    for functionName in functions:
        Debug.debugMessage("Adding jump table edges in '%s'" % functionName, debugLevel)
        icfg = program.getICFG(functionName)
        i    = 0
        hasJumpTablePredecessor = set([])
        for instr in functionToInstructions[functionName]:
            # If the instruction loads into the PC...
            if isJumpTableBranch(instr):
                # Get the number of directives associated with this instruction to work out
                # how many arms it has
                assert instr in jumpTableToDirectives
                numberOfBranchArms = len(jumpTableToDirectives[instr])
                if instr.getOp() == ARMInstructionSet.LoadInstructions[3] \
                or instr.getOp() == ARMInstructionSet.LoadInstructions[4]:
                    numberOfBranchArms = 3
                predv = icfg.getVertexWithAddress(instr.getAddress())
                # Now go through each successive address, get the vertex associated with
                # that address, and add an edge if the address belongs to a newly discovered
                # basic block
                for j in range(i, len(functionToInstructions[functionName])):
                    nextInstr = functionToInstructions[functionName][j]
                    address   = nextInstr.getAddress()
                    if not predv.hasAddress(address):
                        succv = icfg.getVertexWithAddress(address)
                        if not predv.hasSuccessor(succv.getVertexID()) and succv not in hasJumpTablePredecessor:
                            icfg.addEdge(predv.getVertexID(), succv.getVertexID())
                            hasJumpTablePredecessor.add(succv)
                            numberOfBranchArms -= 1
                        # We know how many arms to expect. As soon as the supply has been
                        # exhausted, stop adding edges
                        if not numberOfBranchArms or isJumpTableBranch(nextInstr):
                            break
            i += 1
                    
def generateInternalFile (filename):
    ext = os.path.splitext(filename)[1]
    if ext == '.dis':
        outfilename = filename[:-4] + '.txt'
    else:
        assert ext == '.s'
        outfilename = filename + '.txt'
    Debug.debugMessage("Outputting program to %s" % outfilename, debugLevel)
    with open(outfilename, 'w') as f:
        for icfg in program.getICFGs():
            functionName = icfg.getName()
            f.write("%s %s\n" % (ParseProgramFile.cfgIndicator, functionName))
            for v in icfg:
                vertexID = v.getVertexID()
                f.write("%s %d\n" %  (ParseProgramFile.bbIndicator, vertexID))
                # Write out successors but only if this is not the exit vertex
                if vertexID != icfg.getExitID():
                    counter  = v.numberOfSuccessors()
                    if icfg.isCallSite(vertexID):
                        counter += 1
                    f.write("%s " % ParseProgramFile.successorsIndicator)
                    for succID in v.getSuccessorIDs():
                        f.write("%d" % succID)
                        if counter > 1:
                            f.write(", ")
                        counter -= 1
                if icfg.isCallSite(vertexID):
                    calleeName = icfg.getCalleeName(vertexID)
                    assert calleeName != functionName, "The caller and callee names '%s' match at call site %d" % (functionName, calleeName, vertexID)
                    f.write("%s" % calleeName)
                f.write("\n")
                f.write("%s\n" % ParseProgramFile.instructionsIndicator)
                for instruction in v.getInstructions():
                    f.write("[0x%08X]" % instruction.getAddress())
                    for field in instruction.getInstructionFields():
                        f.write(" [%s] " % field)
                    f.write("\n")
                f.write("\n")
            f.write("\n")
    
def readARMDisassembly (filename, rootFunction):
    global newVertexID, program
    program     = Programs.Program()
    newVertexID = 1
    extractInstructions(filename)
    functions = identifyCallGraph(rootFunction)
    identifyLeaders(functions)
    identifyBasicBlocks(functions)
    addEdges(functions)
    addJumpTableEdges(functions)
    # Now compute entry and exit IDs of functions and root of call graph
    program.getCallGraph().setRoot(rootFunction)
    for icfg in program.getICFGs():
        Debug.debugMessage("Setting entry and exit in '%s'" % icfg.getName(), debugLevel)
        icfg.setEntryID()
        icfg.setExitID()
    program.removeProblematicFunctions()
    program.addExitEntryBackEdges()
    # Dump program to file
    generateInternalFile(filename)
    return program

def findSuccessor (icfg, address):
    succv = None
    closestAddress = sys.maxint
    for v in icfg:
        firstAddr = v.getFirstInstruction().getAddress()
        if firstAddr < address:
            continue
        if firstAddr < closestAddress and firstAddr > address: 
            closestAddress = firstAddr
            succv = v
    return succv

def findJumpTableDirectives (functionName, functionLabelToDirectives, lastAddr):
    closestAddress = sys.maxint
    directives     = None
    for key in functionLabelToDirectives.keys():
        if key[0] == functionName:
            firstAddr = key[1].getAddress()
            if firstAddr < closestAddress and firstAddr > lastAddr:
                closestAddress = firstAddr
                directives     = functionLabelToDirectives[key]
    assert directives, "Unable to find jump table information from address %s" % hex(lastAddr)
    return directives

def addAssemblyJumpTableEdges (icfg, v, directives, labelToBasicBlock):
    for dir in directives:
        lexemes = shlex.split(dir)
        assert len(lexemes) == 2
        labels = re.findall(r'\.L[0-9]+', lexemes[1])
        assert len(labels) == 1
        label = labels[0]
        succv = labelToBasicBlock[label]
        icfg.addEdge(v.getVertexID(), succv.getVertexID())

def readARMAssembly (filename, rootFunction):
    global newVertexID, program
    program     = Programs.Program()
    newVertexID = 1
    functionName = None
    functions = set([])
    functionToInstructions    = {}
    functionLabelToDirectives = {}
    functionToLeaders         = {}
    labelToBasicBlock         = {}
    labels = set([])
    lastLabel = None
    # Rip out instructions for each function
    instructionCounter = 0
    with open(filename, 'r') as f:
        for line in f:
            if re.match(r'\w+:', line):
                index = line.index(':')
                functionName = line[:index]
                functionToInstructions[functionName] = []
                functionToLeaders[functionName] = set([])
                functions.add(functionName)
                continue
            if functionName:
                line = line.replace('[', '')
                line = line.replace(']', '')
                if re.match(r'\s*[a-zA-Z]+', line):
                    lexemes = shlex.split(line)
                    instr = CFGs.Instruction(instructionCounter, lexemes)
                    instructionCounter += 1
                    functionToInstructions[functionName].append(instr)
                    lastLabel = None
                elif re.match(r'\.[a-zA-Z]+[0-9]+:', line):
                    lexemes = shlex.split(line)
                    instr = CFGs.Instruction(instructionCounter, lexemes)
                    instructionCounter += 1
                    functionToInstructions[functionName].append(instr)
                    labels.add(instr)
                    lastLabel = instr
                elif re.match(r'\s*\.word\s+\.[a-zA-Z]+[0-9]+', line):
                    assert lastLabel
                    key = (functionName, lastLabel)
                    if key not in functionLabelToDirectives:
                        functionLabelToDirectives[key] = []
                    functionLabelToDirectives[key].append(line)
    # Remove falsely identified functions (those which have no instructions)
    for functionName in functions:
        if not functionToInstructions[functionName]:
            del functionToInstructions[functionName]
            del functionToLeaders[functionName]
    # Identify leaders
    for functionName, instructions in functionToInstructions.iteritems():
        # The first instruction in a function is always a leader
        leader = True
        for instr in instructions:
            if leader and instr not in labels:
                functionToLeaders[functionName].add(instr)
                leader = False
            if instr not in labels:
                op = instr.getOp()
                if op in ARMInstructionSet.Branches or isJumpTableBranch(instr):
                    leader = True
            else:
                leader = True
    # Identify basic blocks
    for functionName, instructions in functionToInstructions.iteritems():
        icfg = CFGs.CFG()
        icfg.setName(functionName)
        program.addICFG(icfg, functionName)
        bb = None
        for idx, instr in enumerate(instructions):
            if instr in functionToLeaders[functionName]:
                Debug.debugMessage("Instruction '%s' is a leader" % instr, 1)
                vertexID = newVertexID
                bb       = Vertices.BasicBlock(vertexID, name=functionName)
                icfg.addVertex(bb)
                newVertexID += 1
                # Remember the label associated with this basic block
                if idx > 0:
                    previousIdx = idx-1
                    while instructions[previousIdx] in labels:
                        fields = instructions[previousIdx].getInstructionFields()
                        assert len(fields) == 1
                        # Strip colon from label
                        label = fields[0][:-1]
                        labelToBasicBlock[label] = bb
                        previousIdx -= 1
            assert bb, "Basic block is currently null"
            if instr not in labels:
                bb.addInstruction(instr)
    # Add edges
    for icfg in program.getICFGs():
        for v in icfg:
            lastInstr = v.getLastInstruction()
            op        = lastInstr.getOp()
            if op in ARMInstructionSet.UnconditionalJumps:
                fields = lastInstr.getInstructionFields()
                assert len(fields) == 2
                target = fields[1]
                succv = labelToBasicBlock[target]
                icfg.addEdge(v.getVertexID(), succv.getVertexID())
            elif op in ARMInstructionSet.Branches:
                fields = lastInstr.getInstructionFields()
                assert len(fields) == 2
                target = fields[1]
                if target in labelToBasicBlock:
                    succv = labelToBasicBlock[target]
                    icfg.addEdge(v.getVertexID(), succv.getVertexID())
                elif target in functionToInstructions:
                    icfg.addCallSite(v.getVertexID(), target)
                    program.getCallGraph().addEdge(icfg.getName(), target, v.getVertexID())
                succv = findSuccessor(icfg, lastInstr.getAddress())
                if succv:
                    icfg.addEdge(v.getVertexID(), succv.getVertexID())
            elif isJumpTableBranch(lastInstr):
                directives = findJumpTableDirectives(icfg.getName(), functionLabelToDirectives, lastInstr.getAddress())
                addAssemblyJumpTableEdges(icfg, v, directives, labelToBasicBlock)
            else:
                succv = findSuccessor(icfg, lastInstr.getAddress())
                if succv:
                    icfg.addEdge(v.getVertexID(), succv.getVertexID())
    for icfg in program.getICFGs():
        Debug.debugMessage("Setting entry and exit in '%s'" % icfg.getName(), debugLevel)
        icfg.setEntryID()
        icfg.setExitID()
    program.addExitEntryBackEdges()
    # Remove functions other than the root
    callg = program.getCallGraph()
    stack = []
    stack.append(callg.getVertexWithName(rootFunction))
    visited = {}
    for callv in callg:
        visited[callv] = False
    while stack:
        callv = stack.pop()
        visited[callv] = True
        for succID in callv.getSuccessorIDs():
            succv = callg.getVertex(succID)
            if not visited[succv]:
                stack.append(succv)
    for callv in visited:
        if not visited[callv]:
            program.removeFunction(callv.getName())
    callg.setRoot(rootFunction)
    # Dump program to file
    generateInternalFile(filename)
    return program
                

    