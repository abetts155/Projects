import Debug, CFGs, Programs, Vertices, ParseProgramFile
import re, shlex

debugLevel = 20

class ARMInstructionSet:
    Nops               = ['nop', 'nop.n', 'nop.w']
    PCRegister         = 'pc'
    Call               = 'bl'
    UnconditionalJumps = ['b', 'b.n', 'b.w']
    LoadInstructions   = ['ldr', 'ldr.n', 'ldr.w']
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
functionToInstructions          = {}
functionToStartAddress          = {}
functionToLastAddress           = {}
functionToLeaders               = {}
functionToDirectives            = {}
functionToJumpTableInstructions = {}
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
    Debug.verboseMessage("Extracting instructions")
    with open(filename, 'r') as f:
        parse           = False
        lastInstruction = None
        currentFunction = None
        for line in f:
            if parse:
                if re.match(r'[0-9a-fA-F]+\s<.*>.*', line):
                    if lastInstruction:
                        assert currentFunction, "No function detected yet"
                        functionToLastAddress[currentFunction] = lastInstruction.getAddress()
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
                elif re.match(r'\s*[0-9a-fA-F]+:.*', line):
                    # Ignore directives reserving space for data
                    if '.word' not in line:                  
                        lexemes     = shlex.split(line.strip())
                        instruction = getInstruction(lexemes)
                        functionToInstructions[currentFunction].append(instruction)
                        lastInstruction = instruction
                    else:
                        functionToDirectives[currentFunction].append(line)
            elif line.startswith('Disassembly of section'):
                parse = '.text' in line

def identifyCallGraph (rootFunction):
    Debug.verboseMessage("Identifying call graph under analysis (stripping away linked-in functions")
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
                    assert startAddress in startAddressToFunction, "Unable to find function with start address %s (it should be %s)" % (hex(startAddress), instructionFields[2])
                    calleeName = startAddressToFunction[startAddress]
                    if calleeName not in analysed:
                        functions.append(calleeName)
    return analysed

def isJumpTableInstruction (instruction):
    op = instruction.getOp()
    if op in ARMInstructionSet.LoadInstructions: 
        fields      = instruction.getInstructionFields()
        destination = fields[1] 
        if re.match(r'%s' % ARMInstructionSet.PCRegister, destination):
            return True
    return False         

def identifyLeaders (functions):
    functionToBranchTargets = {}
    for functionName in functions:
        Debug.debugMessage("Identifying leaders in '%s'" % functionName, debugLevel)
        functionToLeaders[functionName]       = set([])
        functionToBranchTargets[functionName] = set([])
        newLeader                             = True
        for instruction in functionToInstructions[functionName]:
            if newLeader:
                functionToLeaders[functionName].add(instruction)
                newLeader = False
            else:
                address = instruction.getAddress()
                if address in functionToBranchTargets[functionName]:
                    functionToLeaders[functionName].add(instruction)
                op = instruction.getOp()
                if op in ARMInstructionSet.Branches:
                    newLeader     = True
                    fields        = instruction.getInstructionFields()
                    addressTarget = int(fields[1], 16) 
                    functionToBranchTargets[functionName].add(addressTarget)
                elif isJumpTableInstruction(instruction):
                    # Look for instructions with an explicit load into the PC
                    Debug.debugMessage("Instruction '%s' is loading a value into the PC" % instruction, debugLevel)
                    functionToJumpTableInstructions[functionName].append(instruction)
                    newLeader = True
        Debug.debugMessage("Leaders in '%s' are %s" % (functionName,functionToLeaders[functionName]), debugLevel)
        
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
                vertexID = newVertexID
                bb       = CFGs.BasicBlock(vertexID)
                icfg.addVertex(bb)
                newVertexID += 1
            assert bb, "Basic block is currently null"
            bb.addInstruction(instruction)
            instructionToBasicBlock[instruction] = bb
            
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
                    startAddress      = int(instructionFields[1], 16)
                    calleeName        = startAddressToFunction[startAddress]
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
                        program.getCallGraph().addEdge(functionName, calleeName, v.getVertexID())
                        predID = v.getVertexID()
                elif instruction.getOp() in ARMInstructionSet.Branches:
                    instructionFields = instruction.getInstructionFields()
                    branchAddress     = int(instructionFields[1], 16)
                    if branchAddress >= functionToStartAddress[functionName] and branchAddress <= functionToLastAddress[functionName]:
                        succv = icfg.getVertexWithAddress(branchAddress)
                        icfg.addEdge(v.getVertexID(), succv.getVertexID())
                    else:
                        calleeName = startAddressToFunction[branchAddress]
                        program.getCallGraph().addEdge(functionName, calleeName, v.getVertexID())
                    predID = v.getVertexID()
                elif isJumpTableInstruction(instruction):
                    # Do not add any edges out of these basic blocks as they will be handled
                    # later on
                    pass
                else:
                    predID = v.getVertexID()
                    
def getAddressFromDirectiveLine (line):
    lexemes = shlex.split(line.strip())
    address = int(lexemes[1], 16)
    return address

def getSourceVertex (sourceVertices, address):
    sourcev = None
    for v in sourceVertices:
        lastInstruction = v.getLastInstruction()
        if lastInstruction.getAddress() < address:
            sourcev = v
    assert sourcev, "Unable to find source vertex for address %s" % hex(address)
    return sourcev
            
def addJumpTableEdges (functions):
    for functionName in functions:
        icfg  = program.getICFG(functionName)
        Debug.debugMessage("Adding jump table edges in '%s'" % functionName, debugLevel)
        # First scan the instructions looking for change of PC through explicit load
        sourceVertices = []
        for instruction in functionToJumpTableInstructions[functionName]:
            v = instructionToBasicBlock[instruction] 
            sourceVertices.append(v)
        if sourceVertices:
            for line in functionToDirectives[functionName]:
                # We minus one from the address here because the jump table address in the disassembly
                # always appears one byte away from the actual address
                address = getAddressFromDirectiveLine(line) - 1
                predv   = getSourceVertex(sourceVertices, address)
                succv   = icfg.getVertexWithAddress(address)
                icfg.addEdge(predv.getVertexID(), succv.getVertexID())
        else:
            if functionToDirectives[functionName]:
                Debug.warningMessage("Found directives in %s but no jump table instructions" % functionName)
                    
def generateInternalFile (filename):
    outfilename = filename[:-4] + ".txt"
    Debug.debugMessage("Outputting program to %s" % outfilename, debugLevel)
    with open(outfilename, 'w') as f:
        for icfg in program.getICFGs():
            functionName = icfg.getName()
            f.write("%s %s\n" % (ParseProgramFile.cfgIndicator, functionName))
            for v in icfg:
                vertexID = v.getVertexID()
                counter  = v.numberOfSuccessors()
                if program.getCallGraph().isCallSite(vertexID):
                    counter += 1
                f.write("%s %d\n" %  (ParseProgramFile.bbIndicator, vertexID))
                f.write("%s " % ParseProgramFile.successorsIndicator)
                for succID in v.getSuccessorIDs():
                    f.write("%d" % succID)
                    if counter > 1:
                        f.write(", ")
                    counter -= 1
                if program.getCallGraph().isCallSite(vertexID):
                    callNames = program.getCallGraph().getCallEdgeNames(vertexID)
                    assert callNames[0] == functionName, "The function name '%s' of the ICFG and the caller name '%s' do not match" % (functionName, callNames[0])
                    f.write("%s" % callNames[1])
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
    extractInstructions(filename)
    functions = identifyCallGraph(rootFunction)
    identifyLeaders(functions)
    identifyBasicBlocks(functions)
    addEdges(functions)
    addJumpTableEdges(functions)
    generateInternalFile(filename)
    # Program created
    # Now compute entry and exit IDs of functions and root of call graph
    program.getCallGraph().findAndSetRoot()
    for icfg in program.getICFGs():
        Debug.debugMessage("Setting entry and exit in %s" % icfg.getName(), debugLevel)
        icfg.setEntryID()
        icfg.setExitID()
    return program
    