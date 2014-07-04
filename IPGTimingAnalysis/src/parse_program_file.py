import config
import programs
import cfgs
import vertices
import debug
import shlex
    
def parse_file():
    program = programs.Program()
    icfg    = None
    bb      = None
    with open(config.Arguments.program_file) as f:
        for line in f:
            line = line.lower()
            if line.startswith('cfg:'):
                lexemes = shlex.split(line)
                assert len(lexemes) == 2, "Unable to parse CFG line %s" % line
                icfg          = cfgs.ICFG()
                function_name = lexemes[-1]
                icfg.name     = function_name
                debug.debug_message("Found new ICFG '%s'" % function_name, __name__, 1)
            elif line.startswith('bb:'):
                assert icfg, "Found basic block but current ICFG is null"
                lexemes = shlex.split(line) 
                assert len(lexemes) == 2, "Unable to parse basic block line %s" % line
                vertexID = lexemes[-1]
                assert vertexID.isdigit(), "Vertex identifier '%s' is not an integer" % vertexID
                bb = vertices.CFGVertex(int(vertexID), False)
                icfg.addVertex(bb)
            elif line.startswith('ipoint'):
                assert bb, "Trying to add an Ipoint to a basic block but current basic block is null"
                index    = line.index(':')
                position = line[index+1:].replace(' ', '').strip().lower()
                icfg.ipoint_positions[bb.vertexID] = position
            elif line.startswith('succ:'):
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
                            assert function_name == icfg.name, "Call edge found which is currently not handled"
                        elif index % 3 == 2:
                            succID = lexemes[index]
                            assert succID.isdigit(), "Successor identifier '%s' is not an integer" % succID
                            bb.add_successor(int(succID))
                        index += 1                        
        assert icfg, "Attempting to analyse ICFG but current ICFG is null"
        icfg.add_predecessor_edges()
        icfg.set_entry_and_exit()
        program.add_ICFG(icfg)
    return program     
