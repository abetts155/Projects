import programs
import directed_graphs
import vertices
import debug
import re

cfg_lexeme         = "cfg"
basic_block_lexeme = "bb"
successors_lexeme  = "succ"
profile_lexeme     = "profile"

name_regex        = re.compile("[\d\w]+")
int_regex         = re.compile(r"\d+")
edges_regex       = re.compile(r"\([\w\d\s]+,[\w\d\s]+\)")
edge_tuple_regex  = re.compile(r"[\w\d]+") 
basic_block_regex = re.compile(r"\(\d+\)")
    
def read_file(filename):
    program = programs.Program()
    cfg     = None
    bb      = None
    with open(filename) as the_file:
        for line in the_file:
            line = line.lower()
            if line.startswith(cfg_lexeme):
                if cfg is not None:
                    cfg.add_predecessor_edges()
                    cfg.set_entry_and_exit()
                    program.add_CFG(cfg)
                names = name_regex.findall(line)
                assert len(names) == 2, "Too many names found '%s'" % ' '.join(names)
                cfg                          = directed_graphs.CFG()
                cfg.name                     = names[1]
                debug.debug_message("Found new CFG '%s'" % cfg.name, __name__, 1)
            elif line.startswith(basic_block_lexeme):
                assert cfg, "Found basic block but current CFG is null"
                ids = int_regex.findall(line) 
                assert len(ids) == 1, "Too many identifiers found '%s'" % ' '.join(ids)
                assert ids[0].isdigit(), "Vertex identifier '%s' is not an integer" % ids[0]
                bb = vertices.CFGVertex(int(ids[0]))
                cfg.addVertex(bb)
            elif line.startswith(successors_lexeme):
                assert bb, "Found edge but current basic block is null"
                edges = edges_regex.findall(line)
                for edge in edges:
                    a_tuple = edge_tuple_regex.findall(edge)
                    assert len(a_tuple) == 2, "Too many components in edge tuple: %s" % edge
                    assert a_tuple[0] == cfg.name, "Call edge found which is currently not handled" 
                    assert a_tuple[1].isdigit(), "Successor identifier '%s' is not an integer" % a_tuple[1]
                    bb.add_successor(int(a_tuple[1]))
            elif line.startswith(profile_lexeme):
                assert cfg, "Found profile information but current CFG is null"
                edges = edges_regex.findall(line)
                basic_blocks = basic_block_regex.findall(line)
                for edge_string in edges:
                    a_tuple  = edge_tuple_regex.findall(edge_string)
                    the_edge = int(a_tuple[0]), int(a_tuple[1])
                    assert cfg.hasEdge(the_edge[0], the_edge[1]), "CFG %s does not have edge %s" % (cfg.name, edge_string)
                    cfg.program_points_to_profile.add(the_edge)
                for basic_block_string in basic_blocks:
                    a_tuple         = int_regex.findall(basic_block_string)
                    the_basic_block = int(a_tuple[0])
                    assert cfg.hasVertex(the_basic_block), "CFG %s does not have basic block %d" % (cfg.name, the_basic_block)
                    cfg.program_points_to_profile.add(the_basic_block)
    cfg.add_predecessor_edges()
    cfg.set_entry_and_exit()
    program.add_CFG(cfg)
    return program     
