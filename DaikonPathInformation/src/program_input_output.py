import directed_graphs
import debug
import programs
import vertices
import shlex
import re

cfg_lexeme          = "cfg"
basic_block_lexeme  = "bb"
successors_lexeme   = "succ"
instructions_lexeme = "instructions"

name_regex       = re.compile("[\d\w]+")
int_regex        = re.compile(r"\d+")
edges_regex      = re.compile(r"\([\w\d\s,]+\)")
edge_tuple_regex = re.compile(r"[\w\d]+")

def write_disassembled_program_to_file(program, disassembly_filename):
    filename = disassembly_filename[:-4] + '.txt'
    with open(filename, 'w') as the_file:
        for cfg in program.cfgs.values():
            the_file.write("%s: %s\n" % (cfg_lexeme, cfg.name))
            for v in cfg:
                the_file.write("%s: %d\n" % (basic_block_lexeme, v.vertexID))
                the_file.write("%s: " % successors_lexeme)
                if cfg.get_exitID() != v.vertexID:
                    for succID in v.successors.keys():
                        the_file.write("(%s, %d)" % (cfg.name, succID))
                    callee_name = cfg.is_call_site(v.vertexID)
                    if callee_name is not None:
                        the_file.write("(%s, %d)" % (callee_name, program.cfgs[callee_name].get_entryID()))
                the_file.write("\n")
                the_file.write("%s\n" % instructions_lexeme)
                for instruction in v.instructions:
                    the_file.write("[0x%08X]" % instruction.address)
                    for field in instruction.the_instruction:
                        the_file.write(" [%s] " % field)
                    the_file.write("\n")
                the_file.write("\n" * 2) 

def first_pass(filename):
    # Partially build call graph
    program = programs.Program()
    with open(filename) as the_file:
        for line in the_file:
            line = line.lower()
            if line.startswith(cfg_lexeme):
                names = name_regex.findall(line)
                assert len(names) == 2, "Too many names found '%s'" % ' '.join(names)
                cfg      = directed_graphs.CFG()
                cfg.name = names[1]
                debug.debug_message("Found new CFG '%s'" % cfg.name, __name__, 1)
                program.addCFG(cfg)
    return program

def second_pass(filename, program):
    # Build the CFGs
    cfg          = None
    bb           = None
    instructions = False
    with open(filename) as the_file:
        for line in the_file:
            line = line.lower()
            if line.startswith(cfg_lexeme):
                names = name_regex.findall(line)
                assert len(names) == 2, "Too many names found '%s'" % ' '.join(names)
                cfg = program.cfgs[names[1]]
            elif line.startswith(basic_block_lexeme):
                assert cfg, "Found basic block but current CFG is null"
                ids = int_regex.findall(line) 
                assert len(ids) == 1, "Too many identifiers found '%s'" % ' '.join(ids)
                assert ids[0].isdigit(), "Vertex identifier '%s' is not an integer" % ids[0]
                bb = vertices.BasicBlock(int(ids[0]))
                cfg.addVertex(bb)
            elif line.startswith(successors_lexeme):
                assert bb, "Found edge but current basic block is null"
                edges = edges_regex.findall(line)
                for edge in edges:
                    a_tuple = edge_tuple_regex.findall(edge)
                    assert len(a_tuple) == 2, "Too many components in edge tuple: %s" % edge
                    assert a_tuple[1].isdigit(), "Successor identifier '%s' is not an integer" % a_tuple[1]
                    if a_tuple[0] == cfg.name:
                        bb.add_successor(int(a_tuple[1])) 
                    else:
                        program.callg.addEdge(cfg.name, a_tuple[0], bb.vertexID)
                        cfg.add_call_site(bb.vertexID, a_tuple[0])        
            elif line.startswith(instructions_lexeme):
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
                instruction = vertices.BasicBlock.Instruction(address, fields)
                bb.instructions.append(instruction)

def read_file(filename):
    program = first_pass(filename)
    second_pass(filename, program)
    program.callg.find_and_set_root()
    for cfg in program.cfgs.values():
        cfg.add_predecessor_edges()
        cfg.set_entry_and_exit()
        cfg.set_edgeIDs()
    return program     
