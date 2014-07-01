import config
import programs
import cfgs
import vertices
import debug
import shlex
        
def set_entry_and_exit(icfg):
    without_predecessors = []
    without_successors   = []
    for bb in icfg:
        if bb.number_of_successors() == 0:
            without_successors.append(bb.vertexID)
        elif bb.number_of_successors() == 1:
            if bb.has_successor(bb.vertexID):
                without_successors.append(bb.vertexID)
        if bb.number_of_predecessors() == 0:
            without_predecessors.append(bb.vertexID)
        elif bb.number_of_predecessors() == 1:
            if bb.has_predecessor(bb.vertexID):
                without_predecessors.append(bb.vertexID)
    
    entryID = None
    if len(without_predecessors) == 0:
        debug.exit_message("CFG '%s' does not an entry point" % icfg.getName())
    elif len(without_predecessors) > 1:
        debugStr = ""
        for bbID in without_predecessors:
            bb       = icfg.getVertex(bbID)
            debugStr += bb.__str__()
        debug.exit_message("CFG '%s' has too many entry points: %s" % (icfg.getName(), debugStr))
    else:
        entryID = icfg.getNextVertexID()
        ipoint  = vertices.Ipoint(entryID, entryID)
        icfg.addIpoint(ipoint)
        icfg.setEntryID(entryID)
        icfg.addEdge(entryID, without_predecessors[0])
    
    exitID = None
    if len(without_successors) == 0:
        debug.exit_message("CFG '%s' does not an exit point" % icfg.getName())
    elif len(without_successors) > 1:
        debugStr = ""
        for bbID in without_successors:
            bb       = icfg.getVertex(bbID)
            debugStr += bb.__str__()
        debug.exit_message("CFG '%s' has too many exit points: %s" % (icfg.getName(), debugStr))
    else:
        exitID = icfg.getNextVertexID()
        ipoint = vertices.Ipoint(exitID, exitID)
        icfg.addIpoint(ipoint)
        icfg.setExitID(exitID)
        icfg.addEdge(without_successors[0], exitID)
    assert entryID, "Unable to set entry ID"
    assert exitID, "Unable to set exit ID"
    icfg.addEdge(exitID, entryID)
    
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
                bb = vertices.BasicBlock(int(vertexID))
                icfg.addVertex(bb)
            elif line.startswith('ipoint'):
                assert bb, "Trying to add an Ipoint to a basic block but current basic block is null"
                index    = line.index(':')
                position = line[index+1:].replace(' ', '').strip().lower()
                bb.set_ipoint(position)
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
        assert icfg, "Attempting to analyse CFG but current CFG is null"
        icfg.add_predecessor_edges()
        icfg.addIpointEdges()
        set_entry_and_exit(icfg)
        icfg.setEdgeIDs()
        program.addICFG(icfg)
    return program     
