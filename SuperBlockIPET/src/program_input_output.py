import programs
import directed_graphs
import trees
import vertices
import debug
import database
import re
import random
import numpy

cfg_lexeme         = "cfg"
basic_block_lexeme = "bb"
successors_lexeme  = "succ"
wcet_lexeme        = "wcet"
upper_bound_lexeme = "upper bound"

name_regex       = re.compile("[\d\w]+")
int_regex        = re.compile(r"\d+")
edges_regex      = re.compile(r"\([\w\d\s,]+\)")
edge_tuple_regex = re.compile(r"[\w\d]+")

def write_file(program, filename):
    with open(filename, 'w') as the_file:
        for cfg in program.cfgs.values():
            lnt          = trees.LoopNests(cfg, cfg.get_entryID())
            upper_bounds = {}
            for the_vertices in lnt.level_by_level_iterator(False):
                for treev in the_vertices:
                    if isinstance(treev, vertices.HeaderVertex):
                        if treev.vertexID == lnt.rootID:
                            upper_bounds[treev.headerID] = (1,)
                        else:
                            parentv = lnt.getVertex(treev.parentID)
                            upper_bound_on_parent = numpy.sum(list(upper_bounds[parentv.headerID]))
                            upper_bound = ()
                            for i in xrange(1,upper_bound_on_parent+1):
                                upper_bound += (random.randint(1, 20),)
                            upper_bounds[treev.headerID] = upper_bound
            the_file.write("%s: %s\n" % (cfg_lexeme, cfg.name))
            for v in cfg:
                the_file.write("%s: %d\n" % (basic_block_lexeme, v.vertexID))
                the_file.write("%s: " % successors_lexeme)
                if cfg.get_exitID() != v.vertexID:
                    for succID in v.successors.keys():
                        the_file.write("(%s, %d) " % (cfg.name, succID))
                the_file.write("\n")  
                the_file.write("%s: %d" % (wcet_lexeme, random.randint(1,50)))
                if v.vertexID in upper_bounds:                
                    the_file.write("\n")
                    the_file.write("%s: %s" % (upper_bound_lexeme, upper_bounds[v.vertexID]))
                the_file.write("\n" * 2)  
    
def read_file(filename):
    data    = database.Database()
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
                data.function_data[cfg.name] = database.FunctionData()
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
            elif line.startswith(wcet_lexeme):
                values = int_regex.findall(line) 
                assert len(values) == 1, "Too many values found '%s'" % ' '.join(values)
                assert values[0].isdigit(), "WCET value '%s' is not an integer" % values[0]
                data.function_data[cfg.name].basic_block_WCETs[bb.vertexID] = int(values[0])
            elif line.startswith(upper_bound_lexeme):
                bound_tuple = int_regex.findall(line)
                data.function_data[cfg.name].upper_bounds_on_headers[bb.vertexID] = map(int, bound_tuple)
    cfg.add_predecessor_edges()
    cfg.set_entry_and_exit()
    program.add_CFG(cfg)
    return data, program     
