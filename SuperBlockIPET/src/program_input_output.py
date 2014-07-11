import programs
import directed_graphs
import vertices
import debug
import shlex

new_cfg         = 'cfg'
new_basic_block = 'bb'
new_successors  = 'succ'

def write_file(program, filename):
    with open(filename, 'w') as the_file:
        for cfg in program.cfgs.values():
            the_file.write("%s: %s\n" % (new_cfg, cfg.name))
            for v in cfg:
                the_file.write("%s: %d\n" % (new_basic_block, v.vertexID))
                the_file.write("%s: " % new_successors)
                if cfg.get_exitID() != v.vertexID:
                    for succID in v.successors.keys():
                        the_file.write("(%s, %d)" % (cfg.name, succID))
                the_file.write("\n" * 2)   
    
def read_file(filename):
    program = programs.Program()
    cfg     = None
    bb      = None
    with open(filename) as the_file:
        for line in the_file:
            line = line.lower()
            if line.startswith(new_cfg):
                lexemes = shlex.split(line)
                assert len(lexemes) == 2, "Unable to parse CFG line %s" % line
                cfg           = directed_graphs.CFG()
                function_name = lexemes[-1]
                cfg.name      = function_name
                debug.debug_message("Found new CFG '%s'" % function_name, __name__, 1)
            elif line.startswith(new_basic_block):
                assert cfg, "Found basic block but current CFG is null"
                lexemes = shlex.split(line) 
                assert len(lexemes) == 2, "Unable to parse basic block line %s" % line
                vertexID = lexemes[-1]
                assert vertexID.isdigit(), "Vertex identifier '%s' is not an integer" % vertexID
                bb = vertices.CFGVertex(int(vertexID))
                cfg.addVertex(bb)
            elif line.startswith(new_successors):
                assert bb, "Found edge but current basic block is null"
                index = line.index(':')
                line = line[index+1:]
                splitter = shlex.shlex(line)
                splitter.whitespace += ')'
                splitter.whitespace += '('
                splitter.whitespace_split = False
                lexemes = list(splitter)
                assert len(lexemes) % 3 == 0, "Unable to parse edge information '%s'" % line
                if len(lexemes) > 1:
                    index = 0
                    for lex in lexemes:
                        if index % 3 == 0:
                            function_name = lexemes[index]
                            assert function_name == cfg.name, "Call edge found which is currently not handled"
                        elif index % 3 == 2:
                            succID = lexemes[index]
                            assert succID.isdigit(), "Successor identifier '%s' is not an integer" % succID
                            bb.add_successor(int(succID))
                        index += 1                        
        assert cfg, "Attempting to analyse CFG but current CFG is null"
        cfg.add_predecessor_edges()
        cfg.set_entry_and_exit()
        program.add_CFG(cfg)
    return program     
