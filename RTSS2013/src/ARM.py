import Debug, CFGs, Programs, UDrawGraph, Vertices
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
                    Debug.debugMessage("Detected function '%s' @ start address %d" % (functionName, address), 5)
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
        Debug.debugMessage("Analysing function '%s'" % functionName, 1)
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
        Debug.debugMessage("Identifying leaders in '%s'" % functionName, 1)
        functionToLeaders[functionName]       = set([])
        functionToBranchTargets[functionName] = set([])
        newLeader                       = True
        for instruction in functionToInstructions[functionName]:
            if newLeader:
                functionToLeaders[functionName].add(instruction)
                newLeader = False
                print instruction
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
        Debug.debugMessage("Leaders in '%s' are %s" % (functionName,functionToLeaders[functionName]), 1)
        
def identifyBasicBlocks (functions):
    global newVertexID
    for functionName in functions:
        Debug.debugMessage("Identifying basic blocks in '%s'" % functionName, 1)
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
        Debug.debugMessage("Identifying basic blocks in '%s'" % functionName, 1)
        icfg   = program.getICFG(functionName)
        predID = Vertices.dummyVertexID
        for instruction in functionToInstructions[functionName]:
            v = instructionToBasicBlock[instruction]
            if predID != Vertices.dummyVertexID:
                icfg.addEdge(predID, v.getVertexID())
                predID = Vertices.dummyVertexID
            if v.isLastInstruction(instruction):
                print "%s is last instruction in %d" % (instruction, v.getVertexID())
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

def readARMDisassembly (filename):
    extractInstructions(filename)
    functions = identifyCallGraph()
    identifyLeaders(functions)
    identifyBasicBlocks(functions)
    addEdges (functions)
    program.getCallGraph().findAndSetRoot()
    for icfg in program.getICFGs():
        icfg.setEntryID()
        icfg.setExitID()
        functionName = icfg.getName()
        UDrawGraph.makeUdrawFile(icfg, "%s.%s" % (functionName, "icfg"))
    