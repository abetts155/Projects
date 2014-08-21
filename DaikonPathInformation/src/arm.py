import debug
import directed_graphs
import programs
import vertices
import utils
import re
import shlex

class InstructionSet:
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

class Disassembler:
    def __init__(self, filename, root_function):
        self.start_address_to_function           = {}
        self.last_address_to_function            = {}
        self.jump_table_to_directives            = {}
        self.function_to_instructions            = {}
        self.function_to_start_address           = {}
        self.function_to_last_address            = {}
        self.function_to_leaders                 = {}
        self.function_to_directives              = {}
        self.function_to_jump_table_basic_blocks = {}
        self.instruction_to_basic_block          = {}
        self.next_vertexID                       = 1
        self.program                             = programs.Program()
        self.do_it(filename, root_function)
        
    def do_it(self, filename, root_function):
        self.extract_instructions(filename)
        self.identify_call_graph(root_function)
        self.identify_leaders()
        self.create_basic_blocks()
        self.add_edges()
        self.add_jump_table_edges()
        self.program.callg.rootID = self.program.callg.get_vertex_with_name(root_function).vertexID
        for cfg in self.program.cfgs.values():
            cfg.set_entry_and_exit()
        self.program.remove_problematic_functions()
        
    def extract_instructions(self, filename):
        with open(filename, 'r') as f:
            parse                       = False
            last_instruction            = None
            last_jump_table_instruction = None
            current_function            = None
            for line in f:
                if parse:
                    if re.match(r'[0-9a-fA-F]+\s<.*>.*', line):
                        if last_instruction:
                            assert current_function, "No function detected yet"
                            self.function_to_last_address[current_function]         = last_instruction.address
                            self.last_address_to_function[last_instruction.address] = current_function 
                        lexemes = shlex.split(line)
                        assert len(lexemes) == 2, "Unable to handle disassembly line %s" % line
                        address       = int(lexemes[0], 16)
                        function_name = lexemes[1][1:-2]
                        debug.debug_message("Detected function '%s' @ start address %d" % (function_name, address), __name__, 10)
                        self.start_address_to_function[address]                    = function_name
                        current_function                                           = function_name
                        self.function_to_instructions[current_function]            = []
                        self.function_to_directives[current_function]              = []
                        self.function_to_start_address[current_function]           = address
                        self.function_to_jump_table_basic_blocks[current_function] = []
                        last_jump_table_instruction                                = None
                    elif re.match(r'\s*[0-9a-fA-F]+:.*', line):
                        # Ignore directives reserving space for data
                        if '.word' not in line and '.short' not in line and '.byte' not in line:                  
                            instruction = vertices.BasicBlock.Instruction.get_instruction(line)
                            self.function_to_instructions[current_function].append(instruction)
                            last_instruction = instruction
                            if self.is_jump_table_branch(instruction):
                                last_jump_table_instruction = instruction
                                self.jump_table_to_directives[instruction] = []
                        else:
                            self.function_to_directives[current_function].append(line)
                            if last_jump_table_instruction:
                                self.jump_table_to_directives[last_jump_table_instruction].append(line)   
                elif line.startswith('Disassembly of section'):
                    parse = '.text' in line
        
    def identify_call_graph(self, root_function):
        assert root_function in self.function_to_instructions, "Unable to locate root function '%s' in disassembly" % root_function
        self.functions = set()
        stack = [root_function]
        while stack:
            function_name = stack.pop()
            self.functions.add(function_name)
            debug.debug_message("Analysing function '%s'" % function_name, __name__, 10)
            assert function_name in self.function_to_instructions, "No instructions for '%s' discovered" % function_name
            for instruction in self.function_to_instructions[function_name]:
                if InstructionSet.Call in instruction.the_instruction:
                    assert len(instruction.the_instruction) == 4, "Unable to handle call instruction '%s' since it does not have 3 fields exactly" % instruction.the_instruction
                    startAddress = int(instruction.the_instruction[2], 16)
                    assert startAddress in self.start_address_to_function, "Unable to find function with start address %s (it should be %s)" % (hex(startAddress), instruction.the_instruction[3])
                    callee_name = self.start_address_to_function[startAddress]
                    if callee_name not in self.functions:
                        stack.append(callee_name)
                elif instruction.the_instruction[1] in InstructionSet.Branches:
                    assert len(instruction.the_instruction) == 4, "Unable to handle branch instruction '%s' since it does not have 3 fields exactly" % instruction.the_instruction
                    startAddress = int(instruction.the_instruction[2], 16)
                    if startAddress < self.function_to_start_address[function_name] or startAddress > self.function_to_last_address[function_name]:
                        callee_name = self.get_callee_name(instruction.the_instruction)
                        if callee_name not in self.functions:
                            stack.append(callee_name)
                    
    def get_callee_name(self, an_instruction):
        # The callee name should be delimited by '<' and '>'
        assert an_instruction[3].startswith('<') and an_instruction[3].endswith('>'), "Unable to parse callee name %s" % an_instruction[3]
        callee_name = an_instruction[3][1:-1]
        # Some of the target addresses are NOT the callee start address. In those cases it is an offset from the base address indicated by '+'
        index = callee_name.find('+')
        if index != -1:
            callee_name = callee_name[:index]
        startAddress = int(an_instruction[2], 16) 
        assert startAddress >= self.function_to_start_address[callee_name] and startAddress <= self.function_to_last_address[callee_name], \
        "The address %s is outside the legal range of addresses for %s" % (hex(startAddress), callee_name)
        return callee_name
    
    def is_jump_table_branch(self, instruction):
        op = instruction.the_instruction[1]
        if op in InstructionSet.LoadInstructions: 
            destination = instruction.the_instruction[2] 
            if re.match(r'.*%s.*' % InstructionSet.PCRegister, destination):
                return True
        return False
    
    def identify_leaders(self):
        functionToBranchTargets = {}
        for function_name in self.functions:
            debug.debug_message("Identifying leaders in '%s'" % function_name, __name__, 10)
            self.function_to_leaders[function_name] = set()
            functionToBranchTargets[function_name]  = set()
            newLeader                               = True
            noopAfterJumpTableBranch                = False
            index                                   = 0
            for instruction, nextInstruction in utils.peekahead_iterator(self.function_to_instructions[function_name]):
                if newLeader:
                    self.function_to_leaders[function_name].add(instruction)
                    newLeader = False
                elif noopAfterJumpTableBranch:
                    assert instruction.the_instruction[1] in InstructionSet.Nops, "Did not find an no-op after jump table branch. Instead found %s" % instruction
                    noopAfterJumpTableBranch = False
                    newLeader                = True
                
                op = instruction.the_instruction[1]
                if op in InstructionSet.Branches:
                    newLeader     = True
                    addressTarget = int(instruction.the_instruction[2], 16) 
                    functionToBranchTargets[function_name].add(addressTarget)
                elif self.is_jump_table_branch(instruction):
                    # Look for instructions with an explicit load into the PC
                    debug.debug_message("Instruction '%s' is loading a value into the PC" % instruction, __name__, 10)
                    if nextInstruction and nextInstruction.the_instruction[0] in InstructionSet.Nops:
                        noopAfterJumpTableBranch = True
                    else:
                        newLeader = True
                index += 1
            for instruction in self.function_to_instructions[function_name]:
                if instruction.address in functionToBranchTargets[function_name]:
                    self.function_to_leaders[function_name].add(instruction)
            
    def create_basic_blocks(self):
        for function_name in self.functions:
            debug.debug_message("Identifying basic blocks in '%s'" % function_name, __name__, 10)
            cfg      = directed_graphs.CFG()
            cfg.name = function_name
            self.program.addCFG(cfg)
            bb = None
            for instruction in self.function_to_instructions[function_name]:
                if instruction in self.function_to_leaders[function_name]:
                    debug.debug_message("Instruction @ %s is a leader" % hex(instruction.address), __name__, 10)
                    bb       = vertices.BasicBlock(self.next_vertexID, function_name)
                    cfg.addVertex(bb)
                    self.next_vertexID += 1
                assert bb, "Basic block is currently null"
                bb.instructions.append(instruction)
                self.instruction_to_basic_block[instruction] = bb
                if self.is_jump_table_branch(instruction):
                    self.function_to_jump_table_basic_blocks[function_name].append(bb)
                
    def add_edges(self):
        for function_name in self.functions:
            debug.debug_message("Adding edges in '%s'" % function_name, __name__, 10)
            cfg   = self.program.cfgs[function_name]
            predID = vertices.dummyID
            for instruction in self.function_to_instructions[function_name]:
                v = self.instruction_to_basic_block[instruction]
                if predID != vertices.dummyID:
                    cfg.addEdge(predID, v.vertexID)
                    predID = vertices.dummyID
                if v.instructions[-1] == instruction:
                    if instruction.the_instruction[1] == InstructionSet.Call:
                        callee_name = self.get_callee_name(instruction.the_instruction)
                        cfg.add_call_site(v.vertexID, callee_name)
                        self.program.callg.addEdge(function_name, callee_name, v.vertexID)
                        predID = v.vertexID
                    elif instruction.the_instruction[1] in InstructionSet.UnconditionalJumps:
                        jump_address = int(instruction.the_instruction[2], 16)
                        if jump_address >= self.function_to_start_address[function_name] and jump_address <= self.function_to_last_address[function_name]:
                            succv = cfg.get_basic_block_with_address(jump_address)
                            cfg.addEdge(v.vertexID, succv.vertexID)
                        else:
                            callee_name = self.start_address_to_function[jump_address]
                            cfg.add_call_site(v.vertexID, callee_name)
                            self.program.callg.addEdge(function_name, callee_name, v.vertexID)
                            predID = v.vertexID
                    elif instruction.the_instruction[1] in InstructionSet.Branches:
                        branch_address = int(instruction.the_instruction[2], 16)
                        if branch_address >= self.function_to_start_address[function_name] and branch_address <= self.function_to_last_address[function_name]:
                            succv = cfg.get_basic_block_with_address(branch_address)
                            cfg.addEdge(v.vertexID, succv.vertexID)
                        else:
                            callee_name = self.get_callee_name(instruction.the_instruction)
                            cfg.add_call_site(v.vertexID, callee_name)
                            self.program.callg.addEdge(function_name, callee_name, v.vertexID)
                        predID = v.vertexID
                    elif v in self.function_to_jump_table_basic_blocks[function_name]:
                        pass
                    else:
                        predID = v.vertexID
                        
    def add_jump_table_edges(self):
        for function_name in self.functions:
            debug.debug_message("Adding jump table edges in '%s'" % function_name, __name__, 10)
            cfg = self.program.cfgs[function_name]
            i    = 0
            hasJumpTablePredecessor = set()
            for instr in self.function_to_instructions[function_name]:
                # If the instruction loads into the PC...
                if self.is_jump_table_branch(instr):
                    # Get the number of directives associated with this instruction to work out
                    # how many arms it has
                    assert instr in self.jump_table_to_directives
                    numberOfBranchArms = len(self.jump_table_to_directives[instr])
                    if instr.the_instruction[0] == InstructionSet.LoadInstructions[3] \
                    or instr.the_instruction[0] == InstructionSet.LoadInstructions[4]:
                        numberOfBranchArms = 3
                    predv = cfg.get_basic_block_with_address(instr.address)
                    # Now go through each successive address, get the vertex associated with
                    # that address, and add an edge if the address belongs to a newly discovered
                    # basic block
                    for j in range(i, len(self.function_to_instructions[function_name])):
                        nextInstr = self.function_to_instructions[function_name][j]
                        address   = nextInstr.address
                        if not predv.hasAddress(address):
                            succv = cfg.get_basic_block_with_address(address)
                            if not predv.hasSuccessor(succv.vertexID) and succv not in hasJumpTablePredecessor:
                                cfg.addEdge(predv.vertexID, succv.vertexID)
                                hasJumpTablePredecessor.add(succv)
                                numberOfBranchArms -= 1
                            # We know how many arms to expect. As soon as the supply has been
                            # exhausted, stop adding edges
                            if not numberOfBranchArms or self.is_jump_table_branch(nextInstr):
                                break
                i += 1 
