import programs
import directed_graphs
import vertices
import debug
import database
import re
import random
import config
import udraw

cfg_lexeme         = "cfg"
basic_block_lexeme = "bb"
successors_lexeme  = "succ"
wcet_lexeme        = "wcet"
upper_bound_lexeme = "upper bound"
profile_lexeme     = "profile"
divider            = '-' * 50

name_regex       = re.compile("[\d\w]+")
int_regex        = re.compile(r"\d+")
edges_regex      = re.compile(r"\([\w\d\s,]+\)")
edge_tuple_regex = re.compile(r"[\w\d]+")

def write_file(program, filename):
    with open(filename, 'w') as the_file:
        for cfg in program.cfgs.values():
            if config.Arguments.add_WCET_information:
                lnt          = directed_graphs.LoopNests(cfg, cfg.get_entryID())
                upper_bounds = lnt.get_random_loop_bounds(config.Arguments.maximum_loop_bound)
            the_file.write("%s\n" % divider)
            the_file.write("%s: %s\n" % (cfg_lexeme, cfg.name))
            for v in cfg:
                the_file.write("%s: %d\n" % (basic_block_lexeme, v.vertexID))
                the_file.write("%s: " % successors_lexeme)
                if cfg.get_exitID() != v.vertexID:
                    for succID in v.successors.keys():
                        the_file.write("(%s, %d) " % (cfg.name, succID))
                callee_name = cfg.is_call_site(v.vertexID)
                if callee_name is not None:
                    the_file.write("(%s, %d)" % (callee_name, program.cfgs[callee_name].get_entryID()))
                the_file.write("\n")
                if config.Arguments.add_WCET_information:
                    the_file.write("%s: %d" % (wcet_lexeme, random.randint(1,config.Arguments.maximum_execution_time)))    
                    if v.vertexID in upper_bounds:                
                        the_file.write("\n")
                        the_file.write("%s: %s" % (upper_bound_lexeme, upper_bounds[v.vertexID]))
                    the_file.write("\n") 
            if config.Arguments.add_profile_information:
                the_file.write("%s: " % profile_lexeme)
                for v in cfg:
                    if bool(random.getrandbits(1)):
                        the_file.write("(%d) " % v.vertexID)
                    if cfg.get_exitID() != v.vertexID:
                        for succID in v.successors.keys():
                            if bool(random.getrandbits(1)):
                                the_file.write("(%d, %d) " % (v.vertexID, succID))
                the_file.write("\n")
                     
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
                program.add_CFG(cfg)
    return program

def second_pass(filename, program):
    # Build the CFGs
    data = database.Database()
    cfg  = None
    bb   = None
    with open(filename) as the_file:
        for line in the_file:
            line = line.lower()
            if line.startswith(cfg_lexeme):
                names = name_regex.findall(line)
                assert len(names) == 2, "Too many names found '%s'" % ' '.join(names)
                cfg = program.cfgs[names[1]]
                data.function_data[cfg.name] = database.FunctionData()
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
                    assert a_tuple[1].isdigit(), "Successor identifier '%s' is not an integer" % a_tuple[1]
                    if a_tuple[0] == cfg.name:
                        bb.add_successor(int(a_tuple[1])) 
                    else:
                        program.callg.addEdge(cfg.name, a_tuple[0], bb.vertexID)
                        cfg.add_call_site(bb.vertexID, a_tuple[0])
            elif line.startswith(wcet_lexeme):
                values = int_regex.findall(line) 
                assert len(values) == 1, "Too many values found '%s'" % ' '.join(values)
                assert values[0].isdigit(), "WCET value '%s' is not an integer" % values[0]
                data.function_data[cfg.name].basic_block_WCETs[bb.vertexID] = int(values[0])
            elif line.startswith(upper_bound_lexeme):
                bound_tuple = int_regex.findall(line)
                data.function_data[cfg.name].upper_bounds_on_headers[bb.vertexID] = map(int, bound_tuple)
    return data 

def read_file(filename):
    program = first_pass(filename)
    data    = second_pass(filename, program)
    program.callg.find_and_set_root()
    for cfg in program.cfgs.values():
        cfg.add_predecessor_edges()
        cfg.set_entry_and_exit()
        cfg.set_edgeIDs()
        udraw.make_file(cfg, "%s.cfg" % (cfg.name))
    return data, program     
