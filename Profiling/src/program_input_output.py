import programs
import directed_graphs
import vertices
import debug
import udraw
import re

cfg_lexeme         = "cfg"
basic_block_lexeme = "bb"
successors_lexeme  = "succ"

name_regex       = re.compile("[\d\w]+")
int_regex        = re.compile(r"\d+")
edges_regex      = re.compile(r"\([\w\d\s,]+\)")
edge_tuple_regex = re.compile(r"[\w\d]+")

def write_file(program, filename):
    with open(filename, 'w') as the_file:
        for cfg in program.cfgs.values():
            the_file.write("%s: %s\n" % (cfg_lexeme, cfg.name))
            for v in cfg:
                the_file.write("%s: %d\n" % (basic_block_lexeme, v.vertexID))
                the_file.write("%s: " % successors_lexeme)
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
            if line.startswith(cfg_lexeme):
                names = name_regex.findall(line)
                assert len(names) == 2, "Too many names found '%s'" % ' '.join(names)
                cfg      = directed_graphs.CFG()
                cfg.name = names[1]
                program.add_CFG(cfg)
                debug.debug_message("Found new CFG '%s'" % cfg.name, __name__, 1)
            elif line.startswith(basic_block_lexeme):
                assert cfg, "Found basic block but current CFG is null"
                ids = int_regex.findall(line) 
                assert len(ids) == 1, "Too many identifiers found '%s'" % ' '.join(ids)
                assert ids[0].isdigit(), "Vertex identifier '%s' is not an integer" % ids[0]
                bb = vertices.Vertex(int(ids[0]))
                cfg.add_vertex(bb)
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
                        program.callg.add_edge(cfg.name, a_tuple[0], bb.vertexID)
    for cfg in program.cfgs.values():
        cfg.add_predecessor_edges()
        cfg.set_entry_and_exit()
        cfg.add_dummy_loop_between_exit_and_entry()
        udraw.make_file(cfg, "%s.cfg" % (cfg.name))
    return program     
