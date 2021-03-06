import os
import config
import directed_graphs
import vertices

begin_attrs = "["
end_attrs   = "],"
begin_graph = "[\n"
end_graph   = "]\n"
end_vertex  = "])),"
new_edge    = "\ne(\"tEdge\","
end_edge    = ")"
new_line    = "\\n"

class COLOR:
    RED    = "red"
    YELLOW = "yellow" 
    BLUE   = "blue"
    GREEN  = "green"
    BLACK  = "black"
    
class SHAPE:
    BOX      = "box" 
    CIRCLE   = "circle"
    ELLIPSE  = "ellipse" 
    RHOMBUS  = "rhombus" 
    TRIANGLE = "triangle"
    
class EDGESHAPE:
    SOLID  = "solid"
    DASHED = "dashed"
    DOTTED = "dotted"
    
class DIRECTION:
    DESTINATION = "last"
    SOURCE      = "first"
    BOTH        = "both"
    NONE        = "none"

def new_vertex(vertexID):
    return "l(\"v" + str(vertexID) + "\",n(\"tVertex\","

def edge_link(vertexID):
    return "r(\"v" + str(vertexID) + "\")"

def set_name(name):
    return "a(\"OBJECT\", \"" + name + "\"),"

def set_color(color):
    return "a(\"COLOR\", \"" + color + "\"),"

def set_shape(shape):
    return "a(\"_GO\", \"" + shape + "\"),"

def set_tool_tip(tooltip):
    return "a(\"INFO\", \"" + tooltip + "\"),"

def set_edge_pattern(shape, width):
    return "a(\"EDGEPATTERN\", \"single;" + shape + ";" + str(width) + ";1\"),"

def set_edge_color(color):
    return "a(\"EDGECOLOR\", \"" + color + "\"),"

def set_edge_direction(direction):
    return "a(\"_DIR\", \"" + direction + "\"),"

def make_file(g, graph_name):
    if config.Arguments.udraw:
        filename = "%s%s%s.%s.%s" % (config.Arguments.basepath, os.sep, config.Arguments.basename, graph_name, "udraw")
        with open(filename, 'w') as the_file:
            the_file.write(begin_graph)
            if isinstance(g, directed_graphs.CFG) or isinstance(g, directed_graphs.EnhancedCFG):
                write_CFG_vertex(g, g.get_entryID(), the_file)
                for v in g:
                    if v.vertexID != g.get_entryID():
                        write_CFG_vertex(g, v.vertexID, the_file)   
            elif isinstance(g, directed_graphs.SuperBlockCFG):
                for subgraph in g.whole_body_subgraphs.values():
                    for superv in subgraph:
                        write_super_block_vertex(superv, the_file)
                for subgraph in g.tails_only_subgraphs.values():
                    for superv in subgraph:
                        write_super_block_vertex(superv, the_file)
                for subgraph in g.exits_only_subgraphs.values():
                    for superv in subgraph:
                        write_super_block_vertex(superv, the_file)               
            elif isinstance(g, directed_graphs.LoopNests):
                for v in g:
                    writeTreeVertex(g, v.vertexID, the_file)
            elif isinstance(g, directed_graphs.CallGraph):
                for v in g:
                    writeCallGraphVertex(g, v.vertexID, the_file)
            else:
                for v in g:
                    writeVertex(g, v.vertexID, the_file)
            the_file.write(end_graph) 
            
def writeVertex(g, vertexID, the_file):
    v = g.getVertex(vertexID)        
    the_file.write(new_vertex(vertexID))
    the_file.write(begin_attrs)
    the_file.write(set_name(str(vertexID)))
    the_file.write(end_attrs)
    
    the_file.write(begin_attrs)
    for succID in v.successors.keys():
        the_file.write(new_edge)
        the_file.write(begin_attrs)
        the_file.write(set_name(str(succID)))
        the_file.write(end_attrs)
        the_file.write(edge_link(succID))
        the_file.write(end_edge + ",\n")
    the_file.write(end_vertex + "\n") 
    
def write_CFG_vertex(cfg, vertexID, the_file):
    v = cfg.getVertex(vertexID)
    the_file.write(new_vertex(vertexID))
    the_file.write(begin_attrs)
    if isinstance(v, vertices.CFGVertex):
        the_file.write(set_name(str(vertexID)))
    elif isinstance(v, vertices.CFGEdge):
        name = str(v.edge) + "\\n" + str(vertexID)
        the_file.write(set_name(name))
    else:
        the_file.write(set_name(str(vertexID)))
        the_file.write(set_color(COLOR.RED))
    if v.dummy:
        the_file.write(set_color(COLOR.YELLOW))
    the_file.write(end_attrs)
    
    the_file.write(begin_attrs)
    for succe in v.successors.values():
        the_file.write(new_edge)
        the_file.write(begin_attrs)
        the_file.write(set_name("%d" % succe.edgeID))
        the_file.write(end_attrs)
        the_file.write(edge_link(succe.vertexID))
        the_file.write(end_edge + ",\n")
    the_file.write(end_vertex + "\n")
    
def write_super_block_vertex(superv, the_file):
    the_file.write(new_vertex(superv.vertexID))
    the_file.write(begin_attrs)
    name = "super ID = %d%s" % (superv.vertexID, new_line)
    for idx, program_point in enumerate(superv.program_points):
        if isinstance(program_point, vertices.CFGVertex):
            name += "%d" % (program_point.vertexID)
        elif isinstance(program_point, vertices.CFGEdge):
            name += "(%d, %d)" % (program_point.edge[0], program_point.edge[1])
        else:
            name += "%d"% (program_point.headerID)
        if idx < len(superv.program_points) - 1:
            name += new_line
    if superv.exit_edge:
        the_file.write(set_color(COLOR.YELLOW))
    the_file.write(set_name(name))
    the_file.write(end_attrs)
    
    added = set()
    the_file.write(begin_attrs)
    for branchID, partition in superv.successor_partitions.iteritems():
        for succID in partition:
            the_file.write(new_edge)
            the_file.write(begin_attrs)
            the_file.write(set_name("%d" % branchID))
            the_file.write(end_attrs)
            the_file.write(edge_link(succID))
            the_file.write(end_edge + ",\n")
            added.add(succID)
            
    for succe in superv.successors.values():
        if succe.vertexID not in added:
            the_file.write(new_edge)
            the_file.write(begin_attrs)
            the_file.write(end_attrs)
            the_file.write(edge_link(succe.vertexID))
            the_file.write(end_edge + ",\n")
    the_file.write(end_vertex + "\n")  

def writeTreeVertex (tree, vertexID, the_file):
    v = tree.getVertex(vertexID)
    the_file.write(new_vertex(vertexID))
    the_file.write(begin_attrs)
    the_file.write(set_name(str(vertexID)))
    if isinstance(tree, directed_graphs.LoopNests):
        if isinstance(v, vertices.HeaderVertex):
            the_file.write(set_shape(SHAPE.TRIANGLE))
            the_file.write(set_color(COLOR.RED))
            the_file.write(set_tool_tip("Header ID = %s" % v.headerID))
    the_file.write(end_attrs)
    
    the_file.write(begin_attrs)
    for succID in v.successors.keys():
        the_file.write(new_edge)
        the_file.write(begin_attrs)
        the_file.write(set_name(str(succID)))
        the_file.write(end_attrs)
        the_file.write(edge_link(succID))
        the_file.write(end_edge + ",\n")
    the_file.write(end_vertex + "\n")  
    
def writeCallGraphVertex(callg, vertexID, the_file):
    v = callg.getVertex(vertexID)
    the_file.write(new_vertex(vertexID))
    the_file.write(begin_attrs)
    the_file.write(set_name(v.name))
    if callg.getVertex(callg.rootID) == v:
        the_file.write(set_color(COLOR.RED))
    the_file.write(end_attrs)
    
    the_file.write(begin_attrs)
    for succID in v.successors.keys():
        the_file.write(new_edge)
        the_file.write(begin_attrs)
        the_file.write(set_name(str(succID)))
        the_file.write(end_attrs)
        the_file.write(edge_link(succID))
        the_file.write(end_edge + ",\n")
    the_file.write(end_vertex + "\n")  
     
    
    