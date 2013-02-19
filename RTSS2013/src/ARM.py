import Debug, CFGs
import re, shlex

class ARMInstructionSet:
    CallInstruction     = 'bl'
    BranchInstrunctions = ['b',
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

startAddressToFunction = {}
functionToInstructions = {}

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
                try:
                    int(lex, 16)
                except ValueError:
                    opCodeFound = True
            if opCodeFound:
                instruction.append(lex)
    return CFGs.Instruction(address, instruction)
    
def getInstructions (filename):
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
            if ARMInstructionSet.CallInstruction in instructionFields:
                assert len(instructionFields) == 3, "Unable to handle call instruction '%s' since it does not have 3 fields exactly" % instructionFields
                startAddress = int(instructionFields[1], 16)
                assert startAddress in startAddressToFunction, "Unable to find function with start address %s (it should be %s)" % (hex(startAddress), instructionFields[2])
                calleeName = startAddressToFunction[startAddress]
                if calleeName not in analysed:
                    functions.append(calleeName)    
    return analysed            

def identifyLeaders (functions):
    functionToLeaders = {}
    for functionName in functions:
        Debug.debugMessage("Identifying leaders in '%s'" % functionName, 1)
        functionToLeaders[functionName] = set([])
        firstInstruction                = True
        for instruction in functionToInstructions[functionName]:
            if firstInstruction:
                functionToLeaders[functionName].add(instruction.getAddress())
                firstInstruction = False
            else:
                print instruction.getOp()
        

def identifyBasicBlocks ():
    Debug.verboseMessage("Identifying basic blocks")
    pass

def readARMDisassembly (filename):
    instructions = getInstructions(filename)
    functions    = identifyCallGraph()
    identifyLeaders(functions)
    identifyBasicBlocks()
    