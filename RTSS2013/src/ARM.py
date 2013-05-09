import Debug, CFGs, Programs, Vertices, ParseProgramFile, Utils
import re, shlex

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
functionToInstructions          = {}
functionToStartAddress          = {}
functionToLastAddress           = {}
functionToLeaders               = {}
functionToDirectives            = {}
functionToJumpTableInstructions = {}
functionToJumpTableBasicBlocks  = {}
instructionToBasicBlock         = {}
program                         = Programs.Program()
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
        parse           = False
        lastInstruction = None
        currentFunction = None
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
                    functionToJumpTableInstructions[currentFunction] = []
                    functionToStartAddress[currentFunction]          = address
                    functionToJumpTableBasicBlocks[currentFunction]  = []
                elif re.match(r'\s*[0-9a-fA-F]+:.*', line):
                    # Ignore directives reserving space for data
                    if '.word' not in line and '.short' not in line and '.byte' not in line:                  
                        lexemes     = shlex.split(line.strip())
                        instruction = getInstruction(lexemes)
                        functionToInstructions[currentFunction].append(instruction)
                        lastInstruction = instruction
                    else:
                        functionToDirectives[currentFunction].append(line)
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

def parseDirectiveLine (line):
    lexemes       = shlex.split(line.strip())
    directiveAddr = int(lexemes[0][:-1], 16)
    value         = int(lexemes[1], 16)
    return directiveAddr, value

def identifyJumpTableTargets (functions):
    functionToJumpTableTargets = {}
    for functionName in functions:
        Debug.debugMessage("Computing jump table targets in '%s'" % functionName, debugLevel)
        jumpTableBranches                        = []
        functionToJumpTableTargets[functionName] = set([])
        # First scan the instructions looking for change of PC through explicit load
        for instruction in functionToInstructions[functionName]:
            if isJumpTableBranch(instruction):
                jumpTableBranches.append(instruction)
        if jumpTableBranches:
            for line in functionToDirectives[functionName]:
                # We minus one from the address here because the jump table address in the disassembly
                # always appears one byte away from the actual address
                directiveAddr, value = parseDirectiveLine(line)
                functionToJumpTableTargets[functionName].add(value - 1)
        else:
            if functionToDirectives[functionName]:
                Debug.warningMessage("Found directives in %s but no jump table instructions" % functionName)
    return functionToJumpTableTargets

def identifyLeaders (functions, functionToJumpTableTargets):
    functionToBranchTargets = {}
    for functionName in functions:
        Debug.debugMessage("Identifying leaders in '%s'" % functionName, debugLevel)
        functionToLeaders[functionName]       = set([])
        functionToBranchTargets[functionName] = set([])
        newLeader                             = True
        noopAfterJumpTableBranch              = False
        for instruction, nextInstruction in Utils.peekaheadIterator(functionToInstructions[functionName]):
            if newLeader:
                functionToLeaders[functionName].add(instruction)
                newLeader = False
            elif noopAfterJumpTableBranch:
                assert instruction.getOp() in ARMInstructionSet.Nops, "Did not find an no-op after jump table branch. Instead found %s" % instruction
                noopAfterJumpTableBranch = False
                newLeader                = True
            
            address = instruction.getAddress()
            if address in functionToJumpTableTargets[functionName]:
                functionToLeaders[functionName].add(instruction)
            op = instruction.getOp()
            if op in ARMInstructionSet.Branches:
                newLeader     = True
                fields        = instruction.getInstructionFields()
                addressTarget = int(fields[1], 16) 
                functionToBranchTargets[functionName].add(addressTarget)
            elif isJumpTableBranch(instruction):
                # Look for instructions with an explicit load into the PC
                Debug.debugMessage("Instruction '%s' is loading a value into the PC" % instruction, debugLevel)
                functionToJumpTableInstructions[functionName].append(instruction)
                if nextInstruction and nextInstruction.getOp() in ARMInstructionSet.Nops:
                    noopAfterJumpTableBranch = True
                else:
                    newLeader = True
        for instruction in functionToInstructions[functionName]:
            if instruction.getAddress() in functionToBranchTargets[functionName]:
                functionToLeaders[functionName].add(instruction)
        
def identifyBasicBlocks (functions):
    global newVertexID
    for functionName in functions:
        Debug.debugMessage("Identifying basic blocks in '%s'" % functionName, debugLevel)
        icfg = CFGs.ICFG()
        icfg.setName(functionName)
        program.addICFG(icfg, functionName)
        bb = None
        for instruction in functionToInstructions[functionName]:
            if instruction in functionToLeaders[functionName]:
                Debug.debugMessage("Instruction @ %s is a leader" % hex(instruction.getAddress()), debugLevel)
                vertexID = newVertexID
                bb       = CFGs.BasicBlock(vertexID, name=functionName)
                bb
                icfg.addVertex(bb)
                newVertexID += 1
            assert bb, "Basic block is currently null"
            bb.addInstruction(instruction)
            instructionToBasicBlock[instruction] = bb
            if isJumpTableBranch(instruction):
                functionToJumpTableBasicBlocks[functionName].append(bb)
            
def addJumpTableEdges (functionToJumpTableTargets, functionName, icfg):
    for jumpTableTarget in functionToJumpTableTargets[functionName]:
        succv          = icfg.getVertexWithAddress(jumpTableTarget)
        predv          = None
        highestAddress = 0
        for instruction in functionToJumpTableInstructions[functionName]:
            candidatev = instructionToBasicBlock[instruction] 
            address    = instruction.getAddress()
            if address > highestAddress and address < jumpTableTarget:
                predv          = candidatev
                highestAddress = address
        assert predv, "Unable to find source vertex for address %s" % hex(address)
        Debug.debugMessage("Adding jump table edge %d => %d" % (predv.getVertexID(), succv.getVertexID()), 1)
        icfg.addEdge(predv.getVertexID(), succv.getVertexID())
            
def addEdges (functions, functionToJumpTableTargets):
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
        for instruction in functionToInstructions[functionName]:
            if isJumpTableBranch(instruction):
                addJumpTableEdges(functionToJumpTableTargets, functionName, icfg)
                    
def generateInternalFile (filename):
    outfilename = filename[:-4] + ".txt"
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
                    f.write("[%s]" % hex(instruction.getAddress()))
                    for field in instruction.getInstructionFields():
                        f.write(" [%s] " % field)
                    f.write("\n")
                f.write("\n")
            f.write("\n")
    
def readARMDisassembly (filename, rootFunction):
    global newVertexID
    extractInstructions(filename)
    functions = identifyCallGraph(rootFunction)
    functionToJumpTableTargets = identifyJumpTableTargets(functions)
    identifyLeaders(functions, functionToJumpTableTargets)
    identifyBasicBlocks(functions)
    addEdges(functions, functionToJumpTableTargets)
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
    