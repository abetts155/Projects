import Debug, CFGs, Programs, Vertices, ParseProgramFile
import re, shlex

class ARMInstructionSet:
    Call               = 'bl'
    UnconditionalJumps = ['b', 'b.n', 'b.w']
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

startAddressToFunction  = {}
functionToInstructions  = {}
functionToLeaders       = {}
instructionToBasicBlock = {}
program                 = Programs.Program()
newVertexID             = 1

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
        parse = False
        for line in f:
            if parse:
                if re.match(r'[0-9a-fA-F]+\s<.*>.*', line):
                    lexemes = shlex.split(line)
                    assert len(lexemes) == 2, "Unable to handle disassembly line %s" % line
                    address      = int(lexemes[0], 16)
                    functionName = lexemes[1][1:-2]
                    Debug.debugMessage("Detected function '%s' @ start address %d" % (functionName, address), 20)
                    startAddressToFunction[address]         = functionName
                    currentFunction                         = functionName
                    functionToInstructions[currentFunction] = []
                elif re.match(r'\s*[0-9a-fA-F]+:.*', line):
                    # Ignore directives reserving space for data
                    if '.word' not in line:                  
                        lexemes     = shlex.split(line.strip())
                        instruction = getInstruction(lexemes)
                        functionToInstructions[currentFunction].append(instruction)
            elif line.startswith('Disassembly of section'):
                parse = '.text' in line

def identifyCallGraph ():
    Debug.verboseMessage("Identifying call graph under analysis (stripping away linked-in functions")
    mainFunction = 'main'
    functions    = []
    analysed     = set([])
    functions.append(mainFunction)
    while functions:
        functionName = functions.pop()
        analysed.add(functionName)
        Debug.debugMessage("Analysing function '%s'" % functionName, 20)
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
    return analysed            

def identifyLeaders (functions):
    functionToBranchTargets = {}
    for functionName in functions:
        Debug.debugMessage("Identifying leaders in '%s'" % functionName, 20)
        functionToLeaders[functionName]       = set([])
        functionToBranchTargets[functionName] = set([])
        newLeader                       = True
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
                    newLeader         = True
                    instructionFields = instruction.getInstructionFields()
                    addressTarget     = int(instructionFields[1], 16) 
                    functionToBranchTargets[functionName].add(addressTarget)
        Debug.debugMessage("Leaders in '%s' are %s" % (functionName,functionToLeaders[functionName]), 20)
        
def identifyBasicBlocks (functions):
    global newVertexID
    for functionName in functions:
        Debug.debugMessage("Identifying basic blocks in '%s'" % functionName, 20)
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
        Debug.debugMessage("Identifying basic blocks in '%s'" % functionName, 20)
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
                    program.getCallGraph().addEdge(icfg.getName(), calleeName, v.getVertexID())
                    predID = v.getVertexID()
                elif instruction.getOp() in ARMInstructionSet.UnconditionalJumps:
                    instructionFields = instruction.getInstructionFields()
                    jumpAddress       = int(instructionFields[1], 16)
                    succv             = icfg.getVertexWithAddress(jumpAddress)
                    icfg.addEdge(v.getVertexID(), succv.getVertexID())
                elif instruction.getOp() in ARMInstructionSet.Branches:
                    instructionFields = instruction.getInstructionFields()
                    branchAddress     = int(instructionFields[1], 16)
                    succv             = icfg.getVertexWithAddress(branchAddress)
                    icfg.addEdge(v.getVertexID(), succv.getVertexID())
                    predID = v.getVertexID()
                else:
                    predID = v.getVertexID()
                    
def generateInternalFile (filename):
    outfilename = filename[:-4] + ".txt"
    Debug.debugMessage("Outputting program to %s" % outfilename, 20)
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
    
def readARMDisassembly (filename):
    extractInstructions(filename)
    functions = identifyCallGraph()
    identifyLeaders(functions)
    identifyBasicBlocks(functions)
    addEdges(functions)
    generateInternalFile(filename)
    return program
    